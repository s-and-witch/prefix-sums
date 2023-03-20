{-# LANGUAGE BlockArguments, TemplateHaskell #-}

module Main (main) where

import qualified Test.Tasty.Bench as Tasty.Bench

import GHC.Exts ( noinline )
import System.Environment (lookupEnv)
import Data.Maybe ( fromMaybe )
import Implementations ( implementations )

main :: IO ()
main = do
  !n :: Int <- fromMaybe 10000 . fmap read <$> lookupEnv "TEST_DATA_SIZE"

  putStr "Run tests with lists of size "; print n
  let arg = [1..n]

  putStrLn "\nvalidation:\n"

  let checkImpl impl name =
        if impl arg == scanr1 (+) arg
          then putStrLn (name <> " is OK")
          else putStrLn (name <> " is broken")

  $$( foldr
        (\(impl, name) others -> [|| checkImpl $$impl $$name >> $$others ||] )
        [|| pure () ||]
        implementations)

  putStrLn "\nbenchmarks:\n"
  let tests fun =
        $$( foldr
              (\(impl, name) xs -> [|| (Tasty.Bench.bench $$name (fun $$impl)) : $$xs ||]  )
              [|| [] ||]
              implementations )
      {-# INLINE tests #-}

  Tasty.Bench.defaultMain
    [ Tasty.Bench.bgroup "materialized"     do tests \f -> Tasty.Bench.nf f arg
    , Tasty.Bench.bgroup "materialized sum" do tests \f -> Tasty.Bench.nf (sum . f) arg
    , Tasty.Bench.bgroup "noinline lazy"    do tests \f -> Tasty.Bench.nf (\x -> f (noinline id [1..x])) n
    , Tasty.Bench.bgroup "inline lazy"      do tests \f -> Tasty.Bench.nf (\x -> f [1..x]) n
    ]
