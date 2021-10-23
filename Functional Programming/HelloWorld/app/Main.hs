module Main where

newtype NoQuotes = NoQuotes String
instance Show NoQuotes where show (NoQuotes string) = string

helloWorld :: String
helloWorld = "Hello World"

main :: IO ()
main = print $ NoQuotes helloWorld
