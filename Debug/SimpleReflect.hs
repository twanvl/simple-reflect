-----------------------------------------------------------------------------
-- |
-- Module      :  Debug.SimpleReflect
-- Copyright   :  (c) 2008-2014 Twan van Laarhoven
-- License     :  BSD-style
-- 
-- Maintainer  :  twanvl@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Simple reflection of haskell expressions containing variables.
-- 
-- Some examples:
-- 
-- > > sum [1..5] :: Expr
-- > 0 + 1 + 2 + 3 + 4 + 5
-- 
-- > > foldr1 f [a,b,c]
-- > f a (f b c)
-- 
-- > > take 5 (iterate f x)
-- > [x,f x,f (f x),f (f (f x)),f (f (f (f x)))]
-- 
-- > > mapM_ print $ reduction (1+2*(3+4))
-- > 1 + 2 * (3 + 4)
-- > 1 + 2 * 7
-- > 1 + 14
-- > 15
-----------------------------------------------------------------------------
module Debug.SimpleReflect
    ( module Debug.SimpleReflect.Expr
    , module Debug.SimpleReflect.Vars
    ) where

import Debug.SimpleReflect.Expr
import Debug.SimpleReflect.Vars
