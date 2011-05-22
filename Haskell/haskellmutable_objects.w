>﻿{{Haskell minitoc|chapter=Advanced Haskell}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">﻿{{Haskell minitoc|chapter=Advanced Haskell}}

As one of the key strengths of Haskell is its ''purity'': all side-effects are encapsulated in a [[Haskell/Monads|monad]]. This makes reasoning about programs much easier, but many practical programming tasks require manipulating state and using imperative structures. This chapter will discuss advanced programming techniques for using imperative constructs, such as references and mutable arrays, without compromising (too much) purity.

== The ST and IO monads ==

Recall the [[Haskell/Monads#The_State_monad|The State Monad]] and [[Haskell/Monads#The_.28I.29O_monad|The IO monad]] from the chapter on Monads. These are two methods of structuring imperative effects. Both references and arrays can live in state monads or the IO monad, so which one is more appropriate for what, and how does one use them?

== State references: STRef and IORef ==

== Mutable arrays ==

== Examples ==

<pre>
import Control.Monad.ST
import Data.STRef
import Data.Map(Map)
import qualified Data.Map as M
import Data.Monoid(Monoid(..))

memo :: (Ord a) => (a -> b) -> ST s (a -> ST s b)
memo f = do m <- newMemo
            return (withMemo m f)

newtype Memo s a b = Memo (STRef s (Map a b))

newMemo :: (Ord a) => ST s (Memo s a b)
newMemo = Memo `fmap` newSTRef mempty

withMemo :: (Ord a) => Memo s a b -> (a -> b) -> (a -> ST s b)
withMemo (Memo r) f a = do m <- readSTRef r
                           case M.lookup a m of
                             Just b -> return b
                             Nothing -> do let b = f a
                                           writeSTRef r (M.insert a b m)
                                           return b
</pre>

{{Haskell navigation|chapter=Advanced Haskell}}
