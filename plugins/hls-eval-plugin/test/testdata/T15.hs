{-# LANGUAGE TypeApplications #-}
module T15 where

foo :: Show a => a -> String
foo = show

-- >>> :type  +v  foo @Int
