{-# LANGUAGE OverloadedStrings #-}

module Main where
import           Data.List    ()
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           NLP.Romkan

main :: IO ()
main = mapM_ T.putStrLn makeMozc

makeMozc :: [T.Text]
makeMozc = map (uncurry (\a b -> a <> "\t" <> toHiragana b)) makeTable

makeTable :: [(T.Text, T.Text)]
makeTable = single <>           -- 1 sequence
            concatMap (\x -> [ c <> v | c <- start x, v <- basicVowel]) consonant <> -- 2 sequence
            concatMap (\x -> [ (cf <> soku x <> vf, cs <> vs <> "xtu") | (cf, cs) <- start x, (vf, vs) <- sokuVowel]) consonant <> -- 3 sequence soku
            concatMap (\x -> [ (cf <> primaryYo x <> vf, cs <> "ixy" <> vs) | (cf, cs) <- start x, (vf, vs) <- basicVowel]) consonant <> -- 3 sequence primary Yo
            concatMap (\x -> [ c <> (yf, fromJust ys) <> v | c <- start x, yf <- [secondaryYo x], ys <- [lookup (fst c) (secondaryYoTable x)], v <- basicVowel, isJust ys ]) consonant -- 3 sequence secondary Yo

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

data Consonant = Consonant{ start            :: [(T.Text, T.Text)]
                          , soku             :: T.Text
                          , primaryYo        :: T.Text
                          , secondaryYo      :: T.Text
                          , secondaryYoTable :: [(T.Text, T.Text)]
                          }

consonant :: [Consonant]
consonant = [ Consonant{ start = [ ("f", "p")
                                 , ("g", "ng")
                                 , ("c", "k")
                                 , ("r", "r")]
                       , soku = "g"
                       , primaryYo = "c"
                       , secondaryYo = "r"
                       , secondaryYoTable = [("c", "ux")]
                       }
            , Consonant{ start = [ ("d", "d")
                                 , ("h", "h")
                                 , ("t", "t")
                                 , ("n", "n")
                                 , ("s", "s")]
                       , soku = "h"
                       , primaryYo = "t"
                       , secondaryYo = "n"
                       , secondaryYoTable = [("h", "ux")]
                       }
            , Consonant{ start = [ ("b", "b")
                                 , ("m", "m")
                                 , ("w", "w")
                                 , ("v", "y")
                                 , ("z", "z")]
                       , soku = "m"
                       , primaryYo = "w"
                       , secondaryYo = "v"
                       , secondaryYoTable = [("t", "ex")]
                       }
            ]

baseVowel :: [(T.Text, T.Text)]
baseVowel = [ ("a", "a")
            , ("o", "o")
            , ("e", "e")
            , ("u", "u")
            , ("i", "i")
            , (";", "an'")
            , ("q", "on'")
            , ("j", "en'")
            , ("k", "un'")
            , ("x", "in'")
            ]

basicVowel :: [(T.Text, T.Text)]
basicVowel = baseVowel <>
            [ ("'", "ai")
            , (",", "oi")
            , (".", "ei")
            , ("p", "uu")
            , ("y", "ui")
            ]

sokuVowel :: [(T.Text, T.Text)]
sokuVowel = baseVowel <>
            [ ("'", "ixya")
            , (",", "ixyo")
            , (".", "ixe")
            , ("p", "ixyu")
            , ("i", "ixi")
            ]
