% JMacro and Layered Abstractions
% Gershom Bazerman
% Bay Area Haskell Users Group

Or: Writing Haskell to write JavaScript to call Haskell with
===
Prepared with Pandoc

"The JavaScript Problem"
===
JavaScript is:

 - Untyped
 - Weirdly Scoped
 - Ugly
 - Dynamic

---

JavaScript is also:

 - Full of libraries
 - Higher order
 - Essential

Represenation Space
===

- Strings
- AST
- Combinators / Embedded DSL
- Languge / Exernal DSL
- AJAX Server Framework

JMacro
===

AST _and_ Embedded DSL _and_ External DSL _and_ AJAX Framework _and_ Other AJAX Framework!

Comes with a set of steak knifes.

Built with a tower of abstractions.

Typing space
==
- No Typing
- Embedded Typing
- External Typing

And...

- Type some things, all of the time.
- Type some things, some of the time.
- Type all the things!

Expressiveness vs. Safety
===

 - Types are good
 - _But_ common libraries have insane types

 - Functional is good
 - _But_ common libraries are mutatey.
 
 - Other languages are better than JS
 - _But_ you need to FFI to JS

 - Full compilers are powerful
 - _But_ it's hard to interop with Haskell

JMacro
===

Privilages full expressiveness

_But_ provides foundations for restrictive subsets

(Not typeful, but able to be retrofitted for types)

Where we started
===

 - Plain JS Includes and string calls
 - But strings grew and got unwieldy
 - HJavaScript -- either _too_ typed or _wrongly_ typed
 - Untyped AST Combinators

Name Representation
===

 - Bad old days I did PHP, JSP, etc.
 - Use the same block twice in a page.
 - But _don't_ steal names.

Approaches to Names
===

 - Explicit (with a supply)
 - DeBruijn
 - HOAS (overload names in the host language)

JMacro
===

 - Typeclasses + State Monad + Lambdas = HOAS/Explicit
 - More convenient than HOAS
 - Safer than Explicit

> newtype IdentSupply a = IS {runIdentSupply :: State [Ident] a}
>
> data JStat = ReturnStat JExpr
>            | AssignStat JExpr JExpr
>            | UnsatBlock (IdentSupply JStat)
>              ...

Lambdas don't take a name, they take an name _supply_.

Allows us to mix named and nameless styles, allows us to write n-ary functions.

Typeclass sugar
===

> class ToSat a where
>     toSat_ :: a -> [Ident] -> IdentSupply (JStat, [Ident])
>
> instance ToSat JStat where
>    toSat_ f vs = IS $ return $ (f, reverse vs)
>
> instance (ToSat a, b ~ JExpr) => ToSat (b -> a) where
>     toSat_ f vs = IS $ do
>       x <- freshName
>       runIdentSupply $ toSat_ (f (ValExpr $ JVar x)) (x:vs)

Usage
===

> jLam :: (ToSat a) => a -> JExpr
> jLam f = ValExpr . UnsatVal . IS $ do
>            (block,is) <- runIdentSupply $ toSat_ f []
>            return $ JFunc is block

> jlam $ \ x y z -> x + y + z :: JExpr

Marshalling
===
Splice Haskell values into JavaScript

> class ToJExpr a where
>    toJExpr :: a -> JExpr
>    toJExprFromList :: [a] -> JExpr -- <- show hack
>    toJExprFromList = ValExpr . JList . map toJExpr
>
> instance ToJExpr Bool where
>    toJExpr True  = jsv "true"
>    toJExpr False = jsv "false"
>
> instance ToJExpr Char where
>    toJExpr = ValExpr . JStr . (:[])
>    toJExprFromList = ValExpr . JStr

This was Phase One
===

 - Syntax safety
 - Name safety
 - Embedded combinators
 - Marshalling
 
... But it was _ugly_

... And in a .hs file, we kept accidentally writing Haskell.

And so
===

A parser was born

Started parsing JavaScript

Began parsing "Haskell"

Now can parse almost all JavaScript _and_ a fair number of FP idioms

Combinators parsers are great for exploratory syntax development.

Some sample code
===

>    fun foldl f v xs {
>        var acc = v;
>        for( var i = 0; i < xs.length; i++) {acc = f acc xs[i];};
>        return acc;
>    };
>
>    fun map f xs -> foldl (\acc x {acc.push(f x); return acc}) [] xs;
>
>    fun minimumBy cmp xs ->
>       foldl (\x y -> cmp x y ? x : y) (xs[0]) xs.slice(1);

Next, Antiquotation
===

> ...
>   | AntiStat String
> ...
>
> \x -> [jmacroE| `(x)` + 5|]

Haskell variables can be used in JavaScript.

The x in the antiquote is the x from the lambda. (A var name, or _any_ instance of ToJExpr)


Something unexpected emerges
===

And...

> [jmacroE| \x -> `(generateWith x)` |]

JavaScript variables (names) can be used in Haskell!

---

Tron went through the wall.

Powerful abstractions generate more than you put in.

Examples
===

ghcjs

> genPrim IndexByteArrayOp_Float [r] [a,i] =
>      PrimInline [j| `r` = `a`.getFloat32(`i`<<2); |]
> genPrim IndexByteArrayOp_Double [r] [a,i] =
>      PrimInline [j| `r` = `a`.getFloat64(`i`<<3); |]

> genToplevelRhs i (StgRhsClosure _cc _bi [] upd_flag _srt args body) =
>    toplevel <$> do
>       ...
>       return [j| `tci`;
>                  `decl eid`;
>                  `eid` = `JFunc funArgs (preamble <> body0)`;
>                  `ClosureInfo eid' (genArgInfo False $ map idType args) (istr idi)
>                         (CILayoutFixed 1 []) et (genStaticRefs body)`;
>                  `decl idi`;
>                  `id` = static_fun(`eid`);
>              |]

forml

>     toJExpr (ApplyExpression (SymbolExpression f) []) = ref (to_name f)
>     toJExpr (ApplyExpression f []) = [jmacroE| `(f)` |]
>     toJExpr (ApplyExpression f (end -> x : xs)) =
>          [jmacroE| `(ApplyExpression f xs)`(`(x)`) |]
>     toJExpr (AccessorExpression (Addr _ _ x) []) =
>
>     toJExpr (LetExpression bs ex)
>         | length bs > 0 =
>             [jmacroE| (function() { `(foldl1 mappend $ map toLocal bs)`;
>                                     return `(ex)` })() |]
>         | otherwise = toJExpr ex


We lived with postbacks for as long as we could bear
===

But then, jmacro-rpc was born.

jmacro-rpc provides json rpcs

(details elided)

> -- | A JSON request is a list of values
> type JRequest  = [Value]
>
> -- | A JSON result is either an error or a value.
> type JResult = Either String Value
>
> -- | A JsonRPC is a named function that takes a handle to some state, and
> -- yields a function from request to result in some monad.
> -- It is a representation of the server side of an RPC call.
> data JsonRPC m s = JsonRPC String (s -> JRequest -> m JResult)

Serve and call RPCs from Haskell
===
> -- Pack functions as RPCs
> toJsonRPC :: ToJsonRPC a m => String -> a -> JsonRPC m ()
> toJsonRPC nm f = JsonRPC nm $ \() -> toJsonRPC_ f
>
> toJsonConvRPC :: ToJsonRPC a m => String -> (s -> a) -> JsonRPC m s
> toJsonConvRPC nm f = JsonRPC nm (\s -> toJsonRPC_ (f s))

> -- Encode signatures as RPC Stubs
> class ToJsonRPCCall a b | a -> b where
>    toJsonRPCCall_ :: [Value] -> a -> b
>
> instance (ToJSON a, ToJsonRPCCall b c) => ToJsonRPCCall (a -> b) (a -> c) where
>    toJsonRPCCall_ xs f = \x -> toJsonRPCCall_ (toJSON x : xs) (f x)
>
> instance ... (base case)
>
> toJsonRPCCall :: ToJsonRPCCall a b => a -> b
> ...

Call RPCs from JavaScript!
===

> jsonRPCToDecl :: JsonRPC a m -> JStat
> jsonRPCToDecl (JsonRPC n _) =
>              BlockStat [
>                     DeclStat (StrI n) Nothing,
>                     AssignStat (ValExpr $ JVar $ StrI n)
>                                    [jmacroE|\ {
>                                       var a = Array.prototype.slice.call(arguments);
>                                       return (invokeJsonRPC (serverLoc + "jrpcs") `(n)` a);
>                                     }
>                                     |]
>           ]

Serve RPCs over the web
===

> serveRpcs :: MonadSnap m => (Int -> m s) -> [JsonRPC m s] -> m ()

Serve a page and associated RPCs at once

> -- Generalized version
> handleRpcs :: (Functor m, Monad m) => (Int -> m s) ->
>               [JsonRPC m s] -> BL.ByteString -> m BL.ByteString

Serve stateful conversations over the web
==

> mkConversationPageSimple :: (MonadSnap m) => (JStat -> m ()) -> IO s -> [JsonRPC m s] -> IO (m ())

We can write asynchronous RPCs on top of synchronous ones. We can have global state or session state or page local state.

> -- Generalized version
> mkConversationPageGen :: (MonadIO m1, MonadIO m) =>
>                             IO timestamp
>                             -> (IM.IntMap (timestamp,s) -> IO (IM.IntMap (timestamp,s)))
>                             -> ((Int -> m s) -> [JsonRPC m s] -> m1 resp)
>                             -> (JStat -> m1 resp)
>                             -> IO s
>                             -> [JsonRPC m s]
>                             -> IO (m1 resp, m1 resp)

Generality through higher order functions, straightforward composition

> mkConversationPage getStamp cullMap pageFun emptyState rpcs =
>          (\(rpcPage, mainPage) -> dir (B.pack "jrpcs") rpcPage <|> mainPage) <$>
>          mkConversationPageGen getStamp cullMap serveRpcs pageFun emptyState rpcs

Panels, Reactivity, The Future
===
