{-# LANGUAGE OverloadedStrings #-}

module Main where
import           Control.Applicative
import           Data.Functor
import           Data.List           ()
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           NLP.Romkan

main :: IO ()
main = mapM_ T.putStrLn makeMozc

makeMozc :: [T.Text]
makeMozc = map (uncurry (\a b -> a <> "\t" <> toHiragana b)) makeTable

makeTable :: [(T.Text, T.Text)]
makeTable = single <>
            concatMap (\x -> liftA2 (<>) (start x) vowel) consonant <>
            concatMap (\x -> liftA2 (\(cf, cs) (vf, vs) -> (cf <> soku x <> vf, cs <> vs <> "xtu")) (start x) vowel) consonant <>
            concatMap (\x -> liftA2 (\(cf, cs) (vf, vs) -> (cf <> primaryYo x <> vf, cs <> "y" <> vs)) (start x) vowel) consonant

single :: [(T.Text, T.Text)]
single = [ ("'", "xtu")
         , ("p", "…")
         , ("a", "a")
         , ("o", "o")
         , ("e", "e")
         , ("u", "u")
         , ("i", "i")
         , (";", "an'")
         , ("q", "on'")
         , ("j", "en'")
         , ("k", "un'")
         , ("x", "in'")]

data Consonant = Consonant{ start       :: [(T.Text, T.Text)]
                          , soku        :: T.Text
                          , primaryYo   :: T.Text
                          , secondaryYo :: T.Text}

consonant :: [Consonant]
consonant = [ Consonant{ start = [ ("f", "p")
                                 , ("g", "g")
                                 , ("c", "k")
                                 , ("r", "r")]
                       , soku = "g"
                       , primaryYo = "c"
                       , secondaryYo = "r"}
            , Consonant{ start = [ ("d", "d")
                                 , ("h", "h")
                                 , ("t", "t")
                                 , ("n", "n")
                                 , ("s", "s")]
                       , soku = "h"
                       , primaryYo = "t"
                       , secondaryYo = "n"}
            , Consonant{ start = [ ("b", "b")
                                 , ("m", "m")
                                 , ("w", "w")
                                 , ("v", "y")
                                 , ("z", "z")]
                       , soku = "m"
                       , primaryYo = "w"
                       , secondaryYo = "v"}]

secondaryYoSwitch :: [(T.Text, T.Text)]
secondaryYoSwitch = [ ("c", "ux")
                    , ("h", "ux")
                    , ("t", "ex")]

vowel :: [(T.Text, T.Text)]
vowel = [ ("'", "ai")
        , (",", "oi")
        , (".", "ei")
        , ("p", "uu")
        , ("y", "ui")
        , ("a", "a")
        , ("o", "o")
        , ("e", "e")
        , ("u", "u")
        , ("i", "i")
        , (";", "an'")
        , ("q", "on'")
        , ("j", "en'")
        , ("k", "un'")
        , ("x", "in'")]
