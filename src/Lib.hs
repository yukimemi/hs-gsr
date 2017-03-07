module Lib where

chomp :: String -> String
chomp str
  | x == '\r' = chomp xs
  | x == '\n' = chomp xs
  | otherwise = str
  where
    x = last str
    xs = init str
