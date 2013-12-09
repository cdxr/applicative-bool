{-# LANGUAGE FlexibleInstances #-}

module Control.Applicative.Bool where

import Control.Applicative
import Data.Monoid


class AppBool b where
    (<&&>) :: b -> b -> b
    (<||>) :: b -> b -> b
    true  :: b
    false :: b

infixl 3 <&&>
infixl 2 <||>


sequenceAnd :: (AppBool b) => [b] -> b
sequenceAnd = foldr (<&&>) true

sequenceOr :: (AppBool b) => [b] -> b
sequenceOr = foldr (<||>) false


instance AppBool Bool where
    (<&&>) = (&&)
    (<||>) = (||)
    true  = True
    false = False

instance AppBool Any where
    Any a <&&> Any b = Any $ a && b
    Any a <||> Any b = Any $ a || b
    true  = Any True
    false = Any False

instance AppBool All where
    All a <&&> All b = All $ a && b
    All a <||> All b = All $ a || b
    true  = All True
    false = All False

instance (Applicative f, AppBool b) => AppBool (f b) where
    a <&&> b = (<&&>) <$> a <*> b
    a <||> b = (<||>) <$> a <*> b
    true = pure true
    false = pure false


(>>&&) :: (Monad m) => m Bool -> m Bool -> m Bool
m >>&& n = m >>= \b -> if b then n else return b

(>>||) :: (Monad m) => m Bool -> m Bool -> m Bool
m >>|| n = m >>= \b -> if b then return b else n

infixl 1 >>&&
infixl 1 >>||
