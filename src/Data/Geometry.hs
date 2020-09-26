module Data.Geometry where

import Linear.V2

data Rectangle a = Rectangle (V2 a) (V2 a) deriving (Eq, Show, Functor)

data Line a = Line (V2 a) (V2 a) deriving (Eq, Show, Functor)