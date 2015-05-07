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
makeTable = special <>
            single <> -- 1 sequence
            concatMap (\x -> [ c <> v | c <- start x, v <- basicVowel]) consonant <> -- 2 sequence consonant vowel
            concatMap (\x -> [ (cf <> soku x <> vf, cs <> vs) | (cf, cs) <- start x, (vf, vs) <- sokuVowel]) consonant <> -- 3 sequence soku
            concatMap (\x -> [ (cf <> primaryYo x <> vf, cs <> vs) | (cf, cs) <- start x, (vf, vs) <- primaryYoVowel]) consonant <> -- 3 sequence primary Yo
            concatMap (\x -> [ c <> (yf, fromJust ys) <> v | c <- start x, yf <- [secondaryYo x], ys <- [lookup (fst c) (secondaryYoTable x)], v <- basicVowel, isJust ys ]) consonant -- 3 sequence secondary Yo

special :: [(T.Text, T.Text)]
special = [ ("-", "ー")
          , ("nn", "n'")
          , ("lh", "←")
          , ("lt", "↑")
          , ("ln", "↓")
          , ("ls", "→")
          , ("l'", "xai")
          , ("l,", "xoi")
          , ("l.", "xei")
          , ("lp", "xuu")
          , ("ly", "xui")
          , ("la", "xa")
          , ("lo", "xo")
          , ("le", "xe")
          , ("lu", "xu")
          , ("li", "xi")
          , ("l;", "xan'")
          , ("lq", "xon'")
          , ("lj", "xen'")
          , ("lk", "xun'")
          , ("lx", "xin'")
          , ("lca", "ヵ")
          ]

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
         , ("x", "in'")
         ]

data Consonant = Consonant{ start            :: [(T.Text, T.Text)]
                          , soku             :: T.Text
                          , primaryYo        :: T.Text
                          , secondaryYo      :: T.Text
                          , secondaryYoTable :: [(T.Text, T.Text)]
                          }

consonant :: [Consonant]
consonant = [ Consonant{ start = [ ("f", "p")
                                 , ("g", "g")
                                 , ("c", "k")
                                 , ("r", "r")]
                       , soku = "g"
                       , primaryYo = "c"
                       , secondaryYo = "r"
                       , secondaryYoTable = [ ("c", "ux")]
                       }
            , Consonant{ start = [ ("d", "d")
                                 , ("h", "h")
                                 , ("t", "t")
                                 , ("n", "n")
                                 , ("s", "s")]
                       , soku = "h"
                       , primaryYo = "t"
                       , secondaryYo = "n"
                       , secondaryYoTable = [ ("h", "ux")
                                            , ("t", "ex")]
                       }
            , Consonant{ start = [ ("b", "b")
                                 , ("m", "m")
                                 , ("w", "w")
                                 , ("v", "y")
                                 , ("z", "z")]
                       , soku = "m"
                       , primaryYo = "w"
                       , secondaryYo = "v"
                       , secondaryYoTable = []
                       }
            ]

basicVowel :: [(T.Text, T.Text)]
basicVowel = [ ("'", "ai")
             , (",", "ou")
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
             , ("x", "in'")
            ]

sokuVowel :: [(T.Text, T.Text)]
sokuVowel = [ ("'", "ixyaxtu")
            , (",", "ixyoxtu")
            , (".", "ixextu")
            , ("p", "ixyuxtu")
            , ("y", "ixixtu")
            , ("a", "axtu")
            , ("o", "oxtu")
            , ("e", "extu")
            , ("u", "uxtu")
            , ("i", "ixtu")
            , (";", "an'xtu")
            , ("q", "on'xtu")
            , ("j", "en'xtu")
            , ("k", "un'xtu")
            , ("x", "in'xtu")
            ]

primaryYoVowel :: [(T.Text, T.Text)]
primaryYoVowel = [ ("'", "ixyai")
                 , (",", "ixyou")
                 , (".", "ixei")
                 , ("p", "ixyuu")
                 , ("y", "ixyui")
                 , ("a", "ixya")
                 , ("o", "ixyo")
                 , ("e", "ixe")
                 , ("u", "ixyu")
                 , ("i", "ixi")
                 , (";", "ixyan'")
                 , ("q", "ixyon'")
                 , ("j", "ixen'")
                 , ("k", "ixyun'")
                 , ("x", "ixin'")
                 ]
