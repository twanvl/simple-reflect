{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Debug.SimpleReflect.Expr
-- Copyright   :  (c) 2008-2014 Twan van Laarhoven
-- License     :  BSD-style
--
-- Maintainer  :  twanvl@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Simple reflection of haskell expressions containing variables.
--
-----------------------------------------------------------------------------
module Debug.SimpleReflect.Expr
    ( -- * Construction
      Expr
    , FromExpr(..)
    , var, fun, Associativity(..), op
      -- * Evaluating
    , expr, reduce, reduction
    ) where

import Data.List
import Data.Monoid
#if MIN_VERSION_base(4,9,0) && !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Control.Applicative

------------------------------------------------------------------------------
-- Data type
------------------------------------------------------------------------------

-- | A reflected expression
data Expr = Expr
   { showExpr   :: Int -> ShowS  -- ^ Show with the given precedence level
   , intExpr    :: Maybe Integer -- ^ Integer value?
   , doubleExpr :: Maybe Double  -- ^ Floating value?
   , reduced    :: Maybe Expr    -- ^ Next reduction step
   }

instance Show Expr where
    showsPrec p r = showExpr r p

-- | Default expression
emptyExpr :: Expr
emptyExpr = Expr { showExpr   = \_ -> showString ""
                 , intExpr    = Nothing
                 , doubleExpr = Nothing
                 , reduced    = Nothing
                 }

------------------------------------------------------------------------------
-- Lifting and combining expressions
------------------------------------------------------------------------------

-- | A variable with the given name
var :: String -> Expr
var s = emptyExpr { showExpr = \_ -> showString s }

lift :: Show a => a -> Expr
lift x = emptyExpr { showExpr = \p -> showsPrec p x }

-- | This data type specifies the associativity of operators: left, right or none.
data Associativity = InfixL | Infix | InfixR deriving Eq

-- | An infix operator with the given associativity, precedence and name
op :: Associativity -> Int -> String -> Expr -> Expr -> Expr
op fix prec opName a b = emptyExpr { showExpr = showFun }
 where showFun p = showParen (p > prec)
                     $ showExpr a (if fix == InfixL then prec else prec + 1)
                     . showString opName
                     . showExpr b (if fix == InfixR then prec else prec + 1)

------------------------------------------------------------------------------
-- Adding numeric results
------------------------------------------------------------------------------

iOp :: (Expr -> Expr) -> (Integer -> Integer) -> Expr -> Expr
iOp2 :: (Expr -> Expr -> Expr) -> (Integer -> Integer -> Integer) -> Expr -> Expr -> Expr
dOp :: (Expr -> Expr) -> (Double -> Double) -> Expr -> Expr
dOp2 :: (Expr -> Expr -> Expr) -> (Double -> Double -> Double) -> Expr -> Expr -> Expr

iOp  r f a   = (r a  ) { intExpr    = f <$> intExpr    a }
iOp2 r f a b = (r a b) { intExpr    = f <$> intExpr    a <*> intExpr    b }
dOp  r f a   = (r a  ) { doubleExpr = f <$> doubleExpr a }
dOp2 r f a b = (r a b) { doubleExpr = f <$> doubleExpr a <*> doubleExpr b }

withReduce :: (Expr -> Expr) -> (Expr -> Expr)
withReduce r a    = let rr = r a in
                    rr { reduced = withReduce r <$> reduced a
                               <|> fromInteger <$> intExpr    rr
                               <|> fromDouble  <$> doubleExpr rr
                       }
withReduce2 :: (Expr -> Expr -> Expr) -> (Expr -> Expr -> Expr)
withReduce2 r a b = let rr = r a b in
                    rr { reduced = (\a' -> withReduce2 r a' b) <$> reduced a
                               <|> (\b' -> withReduce2 r a b') <$> reduced b
                               <|> fromInteger <$> intExpr    rr
                               <|> fromDouble  <$> doubleExpr rr
                       }

------------------------------------------------------------------------------
-- Function types
------------------------------------------------------------------------------

-- | Conversion from @Expr@ to other types
class FromExpr a where
    fromExpr :: Expr -> a

instance FromExpr Expr where
    fromExpr = id

instance (Show a, FromExpr b) => FromExpr (a -> b) where
    fromExpr f a = fromExpr $ op InfixL 10 " " f (lift a)

-- | A generic, overloaded, function variable
fun :: FromExpr a => String -> a
fun = fromExpr . var

------------------------------------------------------------------------------
-- Forcing conversion & evaluation
------------------------------------------------------------------------------

-- | Force something to be an expression.
expr :: Expr -> Expr
expr = id

-- | Reduce (evaluate) an expression once.
--
--   For example @reduce (1 + 2 + 3 + 4)  ==  3 + 3 + 4@
reduce :: Expr -> Expr
reduce e = maybe e id (reduced e)

-- | Show all reduction steps when evaluating an expression.
reduction :: Expr -> [Expr]
reduction e0 = e0 : unfoldr (\e -> do e' <- reduced e; return (e',e')) e0

------------------------------------------------------------------------------
-- Numeric classes
------------------------------------------------------------------------------

instance Eq Expr where
    Expr{ intExpr    = Just a } == Expr{ intExpr    = Just b }  =  a == b
    Expr{ doubleExpr = Just a } == Expr{ doubleExpr = Just b }  =  a == b
    a                           == b                            =  show a == show b

instance Ord Expr where
    compare Expr{ intExpr    = Just a } Expr{ intExpr    = Just b }  =  compare a b
    compare Expr{ doubleExpr = Just a } Expr{ doubleExpr = Just b }  =  compare a b
    compare a                           b                            =  compare (show a) (show b)
    min = fun "min" `iOp2` min `dOp2` min
    max = fun "max" `iOp2` max `dOp2` max

instance Num Expr where
    (+)    = withReduce2 $ op InfixL 6 " + " `iOp2` (+)   `dOp2` (+)
    (-)    = withReduce2 $ op InfixL 6 " - " `iOp2` (-)   `dOp2` (-)
    (*)    = withReduce2 $ op InfixL 7 " * " `iOp2` (*)   `dOp2` (*)
    negate = withReduce  $ fun "negate" `iOp` negate `dOp` negate
    abs    = withReduce  $ fun "abs"    `iOp` abs    `dOp` abs
    signum = withReduce  $ fun "signum" `iOp` signum `dOp` signum
    fromInteger i = (lift i)
                     { intExpr    = Just i
                     , doubleExpr = Just $ fromInteger i }

instance Real Expr where
    toRational someExpr = case (doubleExpr someExpr, intExpr someExpr) of
          (Just d,_) -> toRational d
          (_,Just i) -> toRational i
          _          -> error $ "not a rational number: " ++ show someExpr

instance Integral Expr where
    quotRem a b = (quot a b, rem a b)
    divMod  a b = (div  a b, mod a b)
    quot = withReduce2 $ op InfixL 7 " `quot` " `iOp2` quot
    rem  = withReduce2 $ op InfixL 7 " `rem` "  `iOp2` rem
    div  = withReduce2 $ op InfixL 7 " `div` "  `iOp2` div
    mod  = withReduce2 $ op InfixL 7 " `mod` "  `iOp2` mod
    toInteger someExpr = case intExpr someExpr of
          Just i -> i
          _      -> error $ "not an integer: " ++ show someExpr

instance Fractional Expr where
    (/)   = withReduce2 $ op InfixL 7 " / " `dOp2` (/)
    recip = withReduce  $ fun "recip"  `dOp` recip
    fromRational r = fromDouble (fromRational r)

fromDouble :: Double -> Expr
fromDouble d = (lift d) { doubleExpr = Just d }

instance Floating Expr where
    pi    = (var "pi") { doubleExpr = Just pi }
    exp   = withReduce  $ fun "exp"   `dOp` exp
    sqrt  = withReduce  $ fun "sqrt"  `dOp` sqrt
    log   = withReduce  $ fun "log"   `dOp` log
    (**)  = withReduce2 $ op InfixR 8 "**" `dOp2` (**)
    sin   = withReduce  $ fun "sin"   `dOp` sin
    cos   = withReduce  $ fun "cos"   `dOp` cos
    sinh  = withReduce  $ fun "sinh"  `dOp` sinh
    cosh  = withReduce  $ fun "cosh"  `dOp` cosh
    asin  = withReduce  $ fun "asin"  `dOp` asin
    acos  = withReduce  $ fun "acos"  `dOp` acos
    atan  = withReduce  $ fun "atan"  `dOp` atan
    asinh = withReduce  $ fun "asinh" `dOp` asinh
    acosh = withReduce  $ fun "acosh" `dOp` acosh
    atanh = withReduce  $ fun "atanh" `dOp` atanh

instance Enum Expr where
    succ   = withReduce  $ fun "succ" `iOp` succ `dOp` succ
    pred   = withReduce  $ fun "pred" `iOp` pred `dOp` pred
    toEnum = fun "toEnum"
    fromEnum = fromEnum . toInteger
    enumFrom       a     = map fromInteger $ enumFrom       (toInteger a)
    enumFromThen   a b   = map fromInteger $ enumFromThen   (toInteger a) (toInteger b)
    enumFromTo     a   c = map fromInteger $ enumFromTo     (toInteger a)               (toInteger c)
    enumFromThenTo a b c = map fromInteger $ enumFromThenTo (toInteger a) (toInteger b) (toInteger c)

instance Bounded Expr where
    minBound = var "minBound"
    maxBound = var "maxBound"

------------------------------------------------------------------------------
-- Other classes
------------------------------------------------------------------------------

#if MIN_VERSION_base(4,9,0)
instance Semigroup Expr where
    (<>) = withReduce2 $ op InfixR 6 " <> "
#endif

instance Monoid Expr where
    mempty = var "mempty"
#if !(MIN_VERSION_base(4,11,0))
    mappend = withReduce2 $ op InfixR 6 " <> "
#endif
    mconcat = fun "mconcat"

