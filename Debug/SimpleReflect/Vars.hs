-----------------------------------------------------------------------------
-- |
-- Module      :  Debug.SimpleReflect.Vars
-- Copyright   :  (c) 2008-2014 Twan van Laarhoven
-- License     :  BSD-style
-- 
-- Maintainer  :  twanvl@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Single letter variable names.
--
-- All names have type @Expr@, except for @f@, @g@ and @h@, which are generic functions.
-- This means that @show (f x :: Expr) == \"f x\"@, but that @show (a x :: Expr)@ gives a type error.
-- On the other hand, the type of @g@ in @show (f g)@ is ambiguous.
--
-----------------------------------------------------------------------------
module Debug.SimpleReflect.Vars
    ( -- * Variables
      a,b,c,d,e,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
      -- * Functions
    , f,f',f'',g,h
      -- * Operators
    , (⊗), (⊕), (@@)
    ) where

import Debug.SimpleReflect.Expr

------------------------------------------------------------------------------
-- Variables!
------------------------------------------------------------------------------

a,b,c,d,e,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z :: Expr
[a,b,c,d,e,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
   = [var [letter] | letter <- ['a'..'e']++['i'..'z']]

f,f',f'',g,h :: FromExpr a => a
f   = fun "f"
f'  = fun "f'"
f'' = fun "f''"
g   = fun "g"
h   = fun "h"

------------------------------------------------------------------------------
-- Operators
------------------------------------------------------------------------------

-- | A non-associative infix 9 operator
(@@) :: Expr -> Expr -> Expr
(@@) = op Infix 9 " @@ "

infix 9 @@

-- | A non-associative infix 7 operator
(⊗) :: Expr -> Expr -> Expr
(⊗) = op Infix 7 " ⊗ "

infix 7 ⊗

-- | A non-associative infix 6 operator
(⊕) :: Expr -> Expr -> Expr
(⊕) = op Infix 6 " ⊕ "

infix 6 ⊕


