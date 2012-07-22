module Language.C.Simple.Meta.Utils where

padTo :: Int -> Int -> Int
padTo amount x =  (((2 * amount) - (x `mod` amount)) `mod` amount) + x