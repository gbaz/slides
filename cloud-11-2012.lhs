% Putting Cloud Haskell To Work
% Gershom Bazerman
% NY Haskell Users Group

Literate Haskell, prepared with Pandoc
===

> {-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, 
> TupleSections, ScopedTypeVariables, TemplateHaskell #-}
> import System.Environment (getArgs)
> import Network.BSD(getHostName)
> import Control.Distributed.Static (staticClosure)
> import Control.Distributed.Process
> import Control.Distributed.Process.Serializable
> import Control.Distributed.Process.Closure
> import Control.Distributed.Process.Node hiding (newLocalNode)
> import Control.Distributed.Process.Backend.SimpleLocalnet
> import Control.Concurrent hiding (newChan)
> import qualified Control.Concurrent as C
> import Control.Monad
> import Control.Monad.State
> import Control.Applicative
> import Control.Monad.Trans
> import Data.Binary hiding (get)
> import Data.Monoid
> import Data.Typeable
> import Data.IORef
> import qualified Data.Set as S
> import qualified Data.Map as M

Cloud Haskell is not...
===
Cloud Haskell is not Magic
===
  - It does not automatically parallelize anything
  - It does not automatically configure an architecture
  - It does not automatically mean that things don't fail
  - It does not automatically reconfigure when things do fail

Cloud Haskell is not even a way to distribute computation
===
  - It does not give you a combinator to chunk things up and recombine them
  - It does not have a function named mapReduce, map, *or* reduce.

Cloud Haskell is a substrate for distributed computation
===
 - It gives you a way to build many sorts of distributed computation
 - It gives you ways to parallelize things
 - It gives you ways to configure arbitrary architectures
 - It gives you ways to manage and recover from failure

Cloud Haskell is not even Plumbing
===
- It is pipes and wrenches

Distributed Computation is Different
===
Actors, a la Carl  Hewitt
===
 - processing
 - storage
 - communication

Quoth Sussman and Steele
===

"Once we got the interpreter working correctly and had played with it for a while, writing small actors programs, we were astonished to discover that the program fragments in apply that implemented function application and actor invocation were identical!"

___
(The key is tail call optimization)


A Koan
===
Objects are a poor man's closures. Closures are a poor man's object.

Add locality and they're all actors.
===

Actors are good
===
Can express just about any sort of concurrency
---

Actors are bad
===
Can express just about any sort of concurrency
---

Cloud Haskell = Lambdas in the Cloud!
===

****
Pay no attention

> printProcess :: String -> Process ()
> printProcess s = liftIO $ putStrLn s

> sumProcess :: (Int,Int) -> Process Int
> sumProcess (x,y) = do
>   liftIO . putStrLn $ "summing these: " ++ show (x,y)
>   return $ x + y

> chanProcess = do
>   sendP <- expect
>   sendChan sendP "message one"
>   liftIO $ threadDelay 10000
>   sendChan sendP "message two"
>   liftIO $ threadDelay 10000
>   sendChan sendP "message three"

> remotable ['printProcess,'sumProcess,'chanProcess]

A Program
===

> main = do
>   -- Get some instructions
>   [serverType, port] <- getArgs
>   -- Set up the context
>   hostName <- getHostName
>   distributedContext <- initializeBackend hostName port (__remoteTable initRemoteTable)
>   -- The first thing a context lets you do is create a node.
>   node <- newLocalNode distributedContext
>   -- The other thing a context lets you do is find what other nodes are out there.
>   putStrLn "Discovering peers"
>   peers <- findPeers distributedContext 2000
>   putStrLn "Peers discovered"
>   -- About the only thing a node lets you do is run processes on it.
>   runProcess node (go serverType distributedContext peers)

Our hierarchy is Host/Port (location) --> Nodes -> Processes

Process is a Monad, and MonadIO
===

> go :: String -> Backend -> [NodeId] -> Process ()
> go "boring" dc ns = do
>   liftIO $ putStrLn "IO within a process"
>   return ()

This process just hangs out, to keep the node alive
===

> go "volunteer" dc ns = do
>   () <- expect
>   return ()

This process tells other nodes to do stuff
===

> go "remotePrint" dc ns = do
>   let tellPrint n = spawn n $ $(mkClosure 'printProcess) "hi there!"
>   mapM_ tellPrint ns
>   return ()
> {- printProcess s = liftIO $ putStrLn s -}

This process shouldn't compile. Bad cloud haskell! Bad!
===

> go "remotePrintBad" dc ns = do
>   let tellPrint n = spawn n $ $(mkClosure 'printProcess) (123::Int)
>   -- Oh no! What happened to my static types!!!?
>   mapM_ tellPrint ns
>   return ()
>
> {- printProcess s = liftIO $ putStrLn s -}

(Like sending a text message in french to an anglophone).

This process tells other nodes to compute stuff and return
===

> go "remoteCall" dc ns = do
>   let doIt n = call $(functionTDict 'sumProcess) n $ 
>                         $(mkClosure 'sumProcess) (12::Int,25::Int)
>   res <- mapM doIt ns
>   liftIO $ putStrLn $ "returned: " ++ show res
>
> {-
> sumProcess :: (Int,Int) -> Process Int
> sumProcess (x,y) = do
>   liftIO . putStrLn $ "summing these: " ++ show (x,y)
>   return $ x + y
> -}

Interprocess communication is always strict (the wire enforces this).

After synchronous and asynchronus computation we have *aynchronous communication*
===

> go "remoteChan" dc ns = do
>   (sendP,recP) <- newChan
>   let doIt n = do
>         pid <- spawn n $ staticClosure $(mkStatic 'chanProcess)
>         send pid sendP
>   mapM_ doIt ns
>   forever $ liftIO . putStrLn =<< receiveChan recP
>
> {-
> chanProcess = do
>   sendP <- expect
>   sendChan sendP "message one"
>   liftIO $ threadDelay 10000
>   sendChan sendP "message two"
>   liftIO $ threadDelay 10000
>   sendChan sendP "message three"
> -}

Some classic concurrency primitives
===

MVars
===

> newtype DMVar a = DMVar ProcessId deriving (Binary, Typeable)
> newtype MVTake = MVTake ProcessId deriving (Binary, Typeable)
> newtype MVPut a = MVPut (a,ProcessId) deriving (Binary, Typeable)
> newtype MVResponse a = MVResponse {getMVResponse :: a} deriving (Binary, Typeable)
> newtype MVSuccess = MVSuccess () deriving (Binary, Typeable)

> newDMVar :: forall a. Serializable a => Process (DMVar a)
> newDMVar = do
>   mv <- liftIO $ (newEmptyMVar :: IO (MVar a))
>   -- Note that this really is forever. No systemwide GC :-(
>   p <- spawnLocal $ forever $ receiveWait 
>         [
>          -- note the extra spawnlocal
>           match $ \(MVTake pid) -> spawnLocal $ 
>                        liftIO (takeMVar mv) >>= send pid . MVResponse
>         , match $ \(MVPut (v,pid)) -> spawnLocal $
>                        liftIO (putMVar mv v) >> send pid (MVSuccess ())
>         ]
>   return $ DMVar p

___

> takeDMVar :: Serializable a => DMVar a -> Process a
> takeDMVar (DMVar pid) = do
>   spid <- getSelfPid
>   send pid (MVTake spid)
>   link pid
>   res <- getMVResponse <$> expect
>   unlink pid
>   return res

> putDMVar (DMVar pid) x = do
>   spid <- getSelfPid
>   send pid $ MVPut (x,spid)
>   link pid
>   MVSuccess () <- expect
>   unlink pid
>   return ()

The general pattern: Process wrap state, messages interact with processes.

Broadcast Channels
===

Note that standard Chans have only the send end serializable

> newtype DChan a = DChan (SendPort a, ProcessId) deriving (Binary, Typeable)
> newtype DCSubscribe = DCSubscribe ProcessId deriving (Binary, Typeable)
> newtype DCDie = DCDie () deriving (Binary, Typeable)
> newtype DCResponse a = DCResponse {getDCResponse :: a} deriving (Binary, Typeable)

> newDChan :: forall a. Serializable a => Process (DChan a)
> newDChan = do
>   (sendP,recP) <- newChan
>   localChan <- liftIO $ (C.newChan :: IO (C.Chan a))
>   p <- spawnLocal $ getSelfPid >>= \mp -> forever . receiveWait $
>        [
>          match $ \(DCSubscribe pid) -> do
>            newChan <- liftIO $ dupChan localChan
>            spawnLocal $ do
>              link pid
>              link mp
>              forever $ send pid . DCResponse =<< liftIO (readChan newChan)
>        , match $ \(DCDie ()) -> terminate
>         ]
>   return $ DChan (sendP, p)
> 
> writeDChan (DChan (sp,_)) x = sendChan sp

___

> subscribeDChanGen (DChan (_,pid)) f = do
>   spid <- getSelfPid
>   spawnLocal $ do
>     link spid
>     send pid . DCSubscribe =<< getSelfPid
>     forever $ f . getDCResponse =<< expect
>   return ()

> subscribeDChan dc f = do
>   localChan <- liftIO $ C.newChan
>   subscribeDChanGen dc (liftIO . writeChan localChan)
>   return $ readChan localChan

> subscribeCommutativeMonoid dc f = do
>   localVar <- liftIO $ newIORef mempty
>   subscribeDChanGen dc (\x -> liftIO $ atomicModifyIORef localVar (\v -> (v `mappend` x,())))
>   return $ readIORef localVar

Barriers (a la Communicating Sequential Processes)
===

> --Value types
> newtype DBarrier = DBarrier ProcessId deriving (Binary, Typeable)
> newtype Enrolled a = Enrolled a deriving (Binary, Typeable)
> 
> -- Message types
> newtype DBSync = DBSync ProcessId deriving (Binary, Typeable)
> newtype DBEnroll = DBEnroll ProcessId deriving (Binary, Typeable) 
> newtype DBResign = DBResign ProcessId deriving (Typeable, Binary)
> newtype DBClear = DBClear () deriving (Typeable, Binary)

By the way we can use strings instead of newtypes

---

> newDBarrier = do

>   enrolledSet <- liftIO $ newMVar (S.empty :: S.Set ProcessId, 
>                                    S.empty :: S.Set ProcessId, 
>                                    M.empty :: M.Map ProcessId MonitorRef)

>   let resign pid = do
>           mr <- liftIO . modifyMVar enrolledSet $ \(enrolled,synced,mrmap) -> 
>                    return ((S.delete pid enrolled,
>                             S.delete pid synced,
>                             M.delete pid mrmap), 
>                            M.lookup pid mrmap)
>           maybe (return ()) unmonitor mr

>   p <- spawnLocal $ forever . receiveWait $
>        [
>          match $ \(DBEnroll pid) -> do
>            mr <- monitor pid
>            liftIO . modifyMVar_ enrolledSet $ \(enrolled,synced,mrmap) -> 
>                   return (S.insert pid enrolled,
>                           synced,
>                           M.insert pid mr mrmap),
>          match $ \(DBSync pid) -> do
>              releaseList <- liftIO . modifyMVar enrolledSet $ 
>                \(enrolled,synced,mrmap) -> 
>                       let synced' = S.insert pid synced 
>                       in return $
>                           if enrolled == synced' 
>                             then ((enrolled,S.empty,mrmap),synced') 
>                             else ((enrolled,synced',mrmap),S.empty)
>              mapM_ (`send` DBClear ()) $ S.toList releaseList,
>          match $ \(DBResign pid) -> resign pid,
>          match $ \(ProcessMonitorNotification mr pid dr) -> resign pid
>         ]
>   return $ DBarrier p

---
  
> dbEnroll db@(DBarrier pid) = do
>   spid <- getSelfPid
>   send pid (DBEnroll spid)
>   return $ Enrolled db

> dbSync (Enrolled db@(DBarrier pid)) = do
>   spid <- getSelfPid
>   send pid $ DBSync spid
>   link pid
>   DBClear () <- expect
>   unlink pid
>   return db

Monads for consistent distributed traces
===

> -- A Lamport Clock is a simple logical clock
> -- It tracks an abstract notion of time, not wall-clock time.
> -- Every clock has a time and a location (process).
> newtype LamportClock = LamportClock (Integer, ProcessId) 
>    deriving (Eq, Ord, Binary, Typeable)
>
> -- When a clock sees the state of another clock, it sets its time
> -- to be greater than the max of the two times.
> updateClock :: LamportClock -> LamportClock -> LamportClock
> updateClock (LamportClock (xi, xpid)) (LamportClock (yi,_)) = 
>        LamportClock (max xi yi + 1, xpid)

A Lamport Monad
===

> newtype LamProcess a = LamProcess (StateT LamportClock Process a) 
>     deriving (Functor, Applicative, Monad)

> getClock = LamProcess get
> incrClock = LamProcess . modify $ \(LamportClock (xi,xpid)) -> 
>                   LamportClock (xi+1,xpid)

> -- Only safe for lifting single message primitives.
> liftP :: Process a -> LamProcess a
> liftP x = incrClock >> LamProcess (lift x) 

> -- IO doesn't increment clock
> instance MonadIO LamProcess where
>    liftIO x = LamProcess $ lift . liftIO $ x

Some lifted primitives
===

> lsend :: Serializable a => ProcessId -> a -> LamProcess ()
> lsend pid x = getClock >>= \c -> liftP $ send pid (c,x)

> lexpectClocked :: forall a. Serializable a => LamProcess (LamportClock,a)
> lexpectClocked = liftP expect >>= \(c,x) -> LamProcess $ 
>                       modify (updateClock c) >> return (c,x)

> lexpect :: forall a. Serializable a => LamProcess a
> lexpect = snd <$> lexpectClocked

* if A before B then timestamp at A < timestamp at B
* if timestamp A > timestamp B then A did not happen before B
* same tricks work for vector clocks

What we don't have
===
* Logging trees (aka distributed logging) (lamports help)
* Supervision (Monitoring is halfway there)
* Out-of-process supervision (i.e. FFI code)
* Code swapping (Supervision takes you halfway there) [Out of scope]
* Core monitoring functionality / pluggable, inspectable servers.
* Interactive, interruptable, resumable computation (lisp style)
* Full sets of concurrency models -- CSP, Orc, etc.
* Classic algos -- Vector Clocks, Paxos, etc.


In conclusion:
===
This old gem:

![punk!](threechords.jpg "chords")

___
* This is lambda abstraction
* This is application
* This is distribution

Now go and write some programs!


