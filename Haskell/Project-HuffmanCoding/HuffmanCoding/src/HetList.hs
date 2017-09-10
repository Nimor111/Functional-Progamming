{-# LANGUAGE GADTs #-}

module HetList where

type HetList = [Element]

data Element where
  Element :: Show a => a -> Element

instance Show Element where
  show (Element a) = show a

instance Eq Element where
  (Element a) == (Element b) = show a == show b
