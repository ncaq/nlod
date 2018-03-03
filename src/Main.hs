{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import           ClassyPrelude hiding (keys)
import           Data.Char
import           NLP.Romkan

main :: IO ()
main = mapM_ putStrLn makeMozc

makeMozc :: [Text]
makeMozc = map (uncurry (\s h -> s <> "\t" <> h)) romaKana

romaKana :: [(Text, Text)]
romaKana = ordNubBy fst (==) $ filter notElemUntransformed $ map (toDwimKana <$>) seqRoma
  where notElemUntransformed :: (Text, Text) -> Bool
        notElemUntransformed (_, kana) = not $ any (\c -> isLatin1 c && isAlpha c) kana

toDwimKana :: Text -> Text
toDwimKana roma | headEx roma == 'v' = toKatakana roma -- "ヴ"
                | otherwise = toHiragana roma

seqRoma :: [(Text, Text)]
seqRoma = (manual <>) $
          filter removeConflict $ concat
          [ single -- 1 sequence
          , concatMap (\x -> [ c <> v | c <- start x, v <- basicVowel (de $ asLevelKeys x)]) consonant -- 2 sequence basic consonant + vowel
          , concatMap (\x -> [ (cf <> yoon x <> vf, cs <> vs) | (cf, cs) <- start x, (vf, vs) <- yoonVowel (asLevelKeys x)]) consonant -- 3 sequence yoon and shortcut
          , concatMap (\x -> [ (cf <> shortcut x <> vf, cs <> vs) | (cf, cs) <- start x, (vf, vs) <- shortcutVowel]) consonant -- 3 sequence shortcut sokuon and etc
          ]
  where de xs = (headEx xs, lastEx xs)
        removeConflict (s, _) = not $ or
            [ "ww" `isPrefixOf` s -- grow
            , "lf" `isPrefixOf` s
            , "lg" `isPrefixOf` s
            , "lc" `isPrefixOf` s
            , "lr" `isPrefixOf` s
            , "ll" `isPrefixOf` s
            ]

manual :: [(Text, Text)]
manual = [ ("-", "ー")
         , ("nn", "n'")
         , ("we", "ゑ")
         , ("wi", "ゐ")
         , ("l/", "·")
         , ("lb", "⇔")
         , ("lc", "ヵ")
         , ("ld", "∈")
         , ("lf", "∋")
         , ("lg", "↖")
         , ("lh", "←")
         , ("ll", "↗")
         , ("lm", "↙")
         , ("ln", "↓")
         , ("lr", "々")
         , ("ls", "→")
         , ("lt", "↑")
         , ("lva", "xya")
         , ("lvo", "xyo")
         , ("lvu", "xyu")
         , ("lw", "ʬ")
         , ("lz", "↘")
         , ("/,", "、")
         , ("/.", "。")
         , ("/a", "∧")
         , ("/e", "∃")
         , ("/n", "¬")
         , ("/o", "∨")
         , ("/u", "∀")
         ]

single :: [(Text, Text)]
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

data Consonant = Consonant{ start       :: [(Text, Text)]
                          , yoon        :: Text
                          , shortcut    :: Text
                          , asLevelKeys :: [Text]
                          }

consonant :: [Consonant]
consonant = [ Consonant{ start = [ ("f", "p")
                                 , ("g", "g")
                                 , ("c", "k")
                                 , ("r", "r")
                                 , ("l", "x")
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
                                 [ ("d", "dex")
                                 , ("h", "hux")
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

basicVowel :: (Text, Text) -> [(Text, Text)]
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

yoonVowel :: [Text] -> [(Text, Text)]
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

shortcutVowel :: [(Text, Text)]
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
