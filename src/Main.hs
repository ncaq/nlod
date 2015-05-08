{-# LANGUAGE OverloadedStrings #-}

module Main where
import           Control.Applicative
import           Control.Arrow
import           Data.Char
import           Data.List           ()
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           NLP.Romkan

main :: IO ()
main = mapM_ T.putStrLn makeMozc

makeMozc :: [T.Text]
makeMozc = map (uncurry (\s h -> s <> "\t" <> h)) romaKana

romaKana :: [(T.Text, T.Text)]
romaKana = map (fromJust <$>) $ filter (isJust . snd) $ map (toDwimKanaMaybe <$>) seqRoma
  where toDwimKanaMaybe roma = let kana = toDwimKana roma
                               in if isNothing (T.find (\c -> isLatin1 c && isAlpha c) kana) then Just kana else Nothing

toDwimKana :: T.Text -> T.Text
toDwimKana roma | T.head roma == 'v' = toKatakana roma -- "ヴ"
                | otherwise = toHiragana roma

seqRoma :: [(T.Text, T.Text)]
seqRoma = concat
          [ manual
          , single -- 1 sequence
          , concatMap (\x -> [ c <> v | c <- start x, v <- basicVowel (de $ asLevelKeys x)]) consonant -- 2 sequence basic consonant + vowel
          , concatMap (\x -> [ (cf <> yoon x <> vf, cs <> vs) | (cf, cs) <- start x, (vf, vs) <- yoonVowel (asLevelKeys x)]) consonant -- 3 sequence yoon and shortcut
          , concatMap (\x -> [ (cf <> shortcut x <> vf, cs <> vs) | (cf, cs) <- start x, (vf, vs) <- shortcutVowel]) consonant -- 3 sequence shortcut sokuon and etc
          ]
  where de xs = (head xs, last xs)

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
                          , yoon        :: T.Text
                          , shortcut    :: T.Text
                          , asLevelKeys :: [T.Text]
                          }

consonant :: [Consonant]
consonant = [ Consonant{ start = [ ("f", "p")
                                 , ("g", "g")
                                 , ("c", "k")
                                 , ("r", "r")
                                 ] <>
                                 map (first (<> "r"))
                                 [ ("f", "pux")
                                 , ("g", "gux")
                                 , ("c", "kux")
                                 , ("r", "rux")
                                 ]
                       , yoon = "c"
                       , shortcut = "g"
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
                                 ] <>
                                 map (first (<> "n"))
                                 [ ("d", "dux")
                                 , ("h", "fux")
                                 , ("t", "tex")
                                 , ("s", "sux")
                                 ]
                       , yoon = "t"
                       , shortcut = "h"
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
                                 , ("z", "z")
                                 ] <>
                                 map (first (<> "v"))
                                 [ ("b", "bux")
                                 , ("m", "mux")
                                 , ("v", "v")
                                 , ("w", "ux")
                                 , ("z", "zux")
                                 ]
                       , yoon = "w"
                       , shortcut = "m"
                       , asLevelKeys = [ "b"
                                       , "m"
                                       , "w"
                                       , "v"
                                       , "z"
                                       ]
                       }
            ]

basicVowel :: (T.Text, T.Text) -> [(T.Text, T.Text)]
basicVowel (yuu, you) = [ ("'", "ai")
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
                        ] <>
                        [ (yuu, "ixyuu")
                        , (you, "ixyou")
                        ]

yoonVowel :: [T.Text] -> [(T.Text, T.Text)]
yoonVowel keys = [ ("'", "ixyai")
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
                 ] <>
                 zip keys [ "ixyatu"
                          , "ixyaku"
                          , "ixyoku"
                          , "ixyuku"
                          , "ixyutu"
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
