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
makeMozc = map (uncurry (\a b -> a <> "\t" <> toDwimKana b)) makeTable

toDwimKana :: T.Text -> T.Text
toDwimKana roma | T.head roma == 'v' = toKatakana roma
                | otherwise = toHiragana roma

makeTable :: [(T.Text, T.Text)]
makeTable = manual <>
            single <> -- 1 sequence
            concatMap (\x -> [ c <> v | c <- start x, v <- basicVowel]) consonant <> -- 2 sequence basic consonant + vowel
            concatMap (\x -> [ (cf <> shortcut x <> vf, cs <> vs) | (cf, cs) <- start x, (vf, vs) <- shortcutVowel]) consonant <> -- 3 sequence shortcut soku on
            concatMap (\x -> [ (cf <> yoon x <> vf, cs <> vs) | (cf, cs) <- start x, (vf, vs) <- yoonVowel]) consonant <> -- 3 sequence basic yo on
            concatMap (\x -> [ c <> (yf, fromJust ys) <> v | c <- start x, yf <- [fst (loan x)], ys <- [lookup (fst c) (snd (loan x))], v <- basicVowel, isJust ys ]) consonant -- 3 sequence loan speak

manual :: [(T.Text, T.Text)]
manual = [ ("-", "ー")
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

data Consonant = Consonant{ start       :: [(T.Text, T.Text)]
                          , shortcut    :: T.Text
                          , yoon        :: T.Text
                          , loan        :: (T.Text, [(T.Text, T.Text)])
                          , asLevelKeys :: [T.Text]
                          }

consonant :: [Consonant]
consonant = [ Consonant{ start = [ ("f", "p")
                                 , ("g", "g")
                                 , ("c", "k")
                                 , ("r", "r")
                                 ]
                       , shortcut = "g"
                       , yoon = "c"
                       , loan = ("r", [ ("c", "ux")
                                      ])
                       , asLevelKeys = [ "f"
                                       , "g"
                                       , "c"
                                       , "r"
                                       , "l"
                                       ]
                       }
            , Consonant{ start = [ ("d", "d")
                                 , ("h", "h")
                                 , ("t", "t")
                                 , ("n", "n")
                                 , ("s", "s")
                                 ]
                       , shortcut = "h"
                       , yoon = "t"
                       , loan = ("n", [ ("h", "ux")
                                      , ("t", "ex")
                                      ])
                       , asLevelKeys = [ "d"
                                       , "h"
                                       , "t"
                                       , "n"
                                       , "s"
                                       ]
                       }
            , Consonant{ start = [ ("b", "b")
                                 , ("m", "m")
                                 , ("w", "w")
                                 , ("v", "y")
                                 , ("vv", "v")
                                 , ("z", "z")
                                 ]
                       , shortcut = "m"
                       , yoon = "w"
                       , loan = ("v", [])
                       , asLevelKeys = [ "b"
                                       , "m"
                                       , "w"
                                       , "v"
                                       , "z"
                                       ]
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

shortcutVowel :: [(T.Text, T.Text)]
shortcutVowel = [ ("'", "ixyaxtu")
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

yoonVowel :: [(T.Text, T.Text)]
yoonVowel = [ ("'", "ixyai")
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

yoonShortcut :: [T.Text] -> [(T.Text, T.Text)]
yoonShortcut keys = zip keys base
    where base = [ "xyatu"
                 , "xyaku"
                 , "xyoku"
                 , "xyuku"
                 , "xyutu"
                 ]
