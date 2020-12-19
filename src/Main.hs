{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | 基礎的な流れ
-- テーブルを元にNLP.Romkan向けのローマ字を出力して変換する
module Main ( main ) where

import           ClassyPrelude hiding (keys)
import           Data.Char
import           Data.Text     (replace)
import           NLP.Romkan

-- | 標準出力にMozcのキーマップを出力します
main :: IO ()
main = mapM_ putStrLn makeMozc

-- | 各項目がMozcのキーマップ一行となるリストを生成します
makeMozc :: [Text]
makeMozc = map (\(s, h) -> s <> "\t" <> h) romaKana

-- | ローマ字、かなのペアのリストを生成します
-- かなに未変換のローマ字がある項目は排除します
romaKana :: [(Text, Text)]
romaKana = ordNubBy fst (==) $ filter notElemUntransformed $ map (toHiraganaCompatibleForGoogle <$>) seqRoma
 where notElemUntransformed :: (Text, Text) -> Bool
       notElemUntransformed (_, kana) = not $ any (\c -> isLatin1 c && isAlpha c) kana

-- | NLP.Romkanを使ってローマ字を平仮名に変換します
-- `toHiragana`は う゛ と出力してしまうため ゔ に直す処理を含めました
-- see [読みに「う゛」を含む単語を辞書登録できない - Gboard Community](https://support.google.com/gboard/thread/12248624?hl=ja)
-- ゖとかの小小書きは対応してなかったので雑にラップします
toHiraganaCompatibleForGoogle :: Text -> Text
toHiraganaCompatibleForGoogle = completeKogaki . replace "う゛" "ゔ" . toHiragana
  where completeKogaki = replace "xか" "ゕ" . replace "xけ" "ゖ"

-- | ローマ字、かなのペアのリストを基礎的なテーブルデータから生成します
seqRoma :: [(Text, Text)]
seqRoma = (manual <>) $ filter removeConflict $
  -- 小文字にするキーを付与したバージョンも作る
  concatMap (\x -> [x, bimap ("l" <>) ("x" <>) x]) $
  concat
  -- 単体
  [ single
  -- 2シーケンスの変換(hs -> ひょうなど)
  , concatMap
    (\x -> [ c <> v | c <- start x, v <- basicVowel (de $ asLevelKeys x) ])
    consonant
  -- 拗音3シーケンスの変換(stn -> しゅくなど)
  , concatMap
    (\x ->
        [ (cf <> yoon x <> vf, cs <> vs)
        | (cf, cs) <- start x
        , (vf, vs) <- yoonVowel (asLevelKeys x)
        ]
    )
    consonant
  -- 促音3シーケンスの変換
  , concatMap
    (\x ->
       [ (cf <> sokuon x <> vf, cs <> vs)
       | (cf, cs) <- start x
       , (vf, vs) <- sokuonVowel
       ]
    )
    consonant
  ]
 where de xs = (headEx xs, lastEx xs)
       -- 他のテーブルと競合するものを排除
       removeConflict (s, _) = not
         (("ww" `isPrefixOf` s) || -- 草を生やすため
          ("we" `isPrefixOf` s) ||
          ("wi" `isPrefixOf` s))

-- | 手動で入れるしかない特殊変換
manual :: [(Text, Text)]
manual =
  [ ("nn" , "n'")
  , ("we" , "ゑ")
  , ("wi" , "ゐ")
  , ("/a" , "∧") -- andから連想
  , ("/o" , "∨") -- orから連想
  , ("/e" , "∃") -- existから連想
  , ("/u" , "∀") -- andが埋まっていたのでeの隣に置きました
  , ("/b" , "⇔") -- bothから連想
  , ("/d" , "∈") -- Dvorakだと∋と対になっていて丁度いい
  , ("/f" , "∋") -- Dvorakだと∈と対になっていて丁度いい
  , ("/c" , "々") -- 踊り字は連想出来ないので雑に配置
  , ("/r" , "ゝ") -- 踊り字は連想出来ないので雑に配置
  , ("/w" , "ʬ")  -- wの特殊文字なので当然wに配置
  , ("/h" , "←")
  , ("/t" , "↑")
  , ("/n" , "↓")
  , ("/s" , "→")
  , ("/g" , "↖")
  , ("/l" , "↗")
  , ("/m" , "↙")
  , ("/z" , "↘")
  ]

-- | 単体で読みを構成するもの
single :: [(Text, Text)]
single =
  [ ("'", "xtu")
  , ("-", "ー")
  , ("p", "…")
  , ("y", "・")
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

-- | テーブルを構成するための配置データを構成する
data Consonant
  = Consonant
  { start       :: ![(Text, Text)] -- ^ 打ち始めの文字
  , yoon        :: !Text           -- ^ 拗音を開始するための文字
  , sokuon      :: !Text           -- ^ 促音を開始するための文字
  , asLevelKeys :: ![Text]         -- ^ 同じキーボードの段にある文字
  } deriving (Eq, Ord, Show, Read)

-- | 3段マップデータ
consonant :: [Consonant]
consonant =
  [ Consonant
    { start       = [("f", "p"), ("g", "g"), ("c", "k"), ("r", "r")] <>
      [("fr", "pux"), ("gr", "gux"), ("cr", "kux"), ("rr", "rux")]
    , yoon        = "c"
    , sokuon      = "g"
    , asLevelKeys = ["f", "g", "c", "r", "l"]
    }
  , Consonant
    { start       = [("d", "d"), ("h", "h"), ("t", "t"), ("n", "n"), ("s", "s")] <>
      [("dn", "dex"), ("hn", "hux"), ("tn", "tex"), ("sn", "sux")]
    , yoon        = "t"
    , sokuon      = "h"
    , asLevelKeys = ["d", "h", "t", "n", "s"]
    }
  , Consonant
    { start       = [("b", "b"), ("m", "m"), ("w", "w"), ("v", "y"), ("z", "z")] <>
      [("bv", "bux"), ("mv", "mux"), ("vv", "v"), ("wv", "ux"), ("zv", "zux")]
    , yoon        = "w"
    , sokuon      = "m"
    , asLevelKeys = ["b", "m", "w", "v", "z"]
    }
  ]

-- | 基礎的な変換テーブル
basicVowel :: (Text, Text) -> [(Text, Text)]
basicVowel (yuu, you) =
  [ ("'", "ai")
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
  <> [(yuu, "ixyuu"), (you, "ixyou")] -- 2キーショートカット

-- | 拗音を含む出力をするためのテーブル
yoonVowel :: [Text] -> [(Text, Text)]
yoonVowel keys =
  [ ("'", "ixyai")
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
  <> zip keys ["ixyatu", "ixyaku", "ixyoku", "ixyuku", "ixyutu"] -- 3キーショートカット

-- | 促音を含む出力をするためのテーブル
sokuonVowel :: [(Text, Text)]
sokuonVowel =
  [ ("'", "ixyaxtu")
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
