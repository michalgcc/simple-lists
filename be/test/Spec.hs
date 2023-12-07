{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Spec.Hspec.Discover (IO, Monad (return), Spec, hspec)
import Test.Hspec.Discover (IO, Spec, describe, hspec)

main :: IO ()
main = hspecWith defaultConfig {configFormatter = Just progress} Spec.spec
