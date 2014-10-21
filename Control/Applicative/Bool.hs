{-# LANGUAGE FlexibleInstances #-}

module Control.Applicative.Bool where

import Control.Applicative
import Data.Monoid


class AppBool b where

    true  :: b
    false :: b

    -- | Negation
    --
    -- @
    -- neg . neg = `id`
    -- neg true = `false`
    -- neg false = `true`
    -- @
    --
    neg   :: b -> b

    -- | Lifted `(&&)`
    --
    -- @
    -- true  <&&> true  = true
    -- true  <&&> false = false
    -- false <&&> true  = false
    -- false <&&> false = false
    -- @
    --
    (<&&>) :: b -> b -> b

    -- | Lifted `(||)`
    --
    -- @
    -- true  <||> true  = true
    -- true  <||> false = true
    -- false <||> true  = true
    -- false <||> false = false
    -- @
    --
    (<||>) :: b -> b -> b

    -- | Exclusive disjunction, i.e "xor"
    --
    -- @
    -- p <+> q  =  (p <||> q) <&&> neg (p <&&> q)
    -- p <+> q  =  p /= q
    -- @
    --
    (<+>) :: b -> b -> b

    -- | Equivalence
    --
    -- @
    -- p <=> q  =  (neg p <||> q) <&&> (neg q <||> p)
    -- p <=> q  =  p == q
    -- @
    --
    (<=>) :: b -> b -> b


infixl 3 <&&>
infixl 2 <||>


andA :: (AppBool b) => [b] -> b
andA = foldr (<&&>) true

orA :: (AppBool b) => [b] -> b
orA = foldr (<||>) false

xorA :: (AppBool b) => [b] -> b
xorA = foldr (<+>) false


instance AppBool Bool where
    true   = True
    false  = False
    neg    = not
    (<&&>) = (&&)
    (<||>) = (||)
    (<+>)  = (/=)
    (<=>)  = (==)

instance AppBool Any where
    true  = Any True
    false = Any False
    neg (Any b) = Any $ neg b
    Any a <&&> Any b = Any $ a && b
    Any a <||> Any b = Any $ a || b
    a <+> b = Any $ a /= b
    a <=> b = Any $ a == b

instance AppBool All where
    true  = All True
    false = All False
    neg (All b) = All $ neg b
    All a <&&> All b = All $ a && b
    All a <||> All b = All $ a || b
    a <+> b = All $ a /= b
    a <=> b = All $ a == b

instance (Applicative f, AppBool b) => AppBool (f b) where
    true   = pure true
    false  = pure false
    neg    = liftA neg
    (<&&>) = liftA2 (<&&>)
    (<||>) = liftA2 (<||>)
    (<+>)  = liftA2 (<+>)
    (<=>)  = liftA2 (<=>)


-- | Short-circuiting `<&&>`
(>>&&) :: (Monad m) => m Bool -> m Bool -> m Bool
m >>&& n = m >>= \b -> if b then n else return b

-- | Short-circuiting `<||>`
(>>||) :: (Monad m) => m Bool -> m Bool -> m Bool
m >>|| n = m >>= \b -> if b then return b else n

infixl 1 >>&&
infixl 1 >>||
