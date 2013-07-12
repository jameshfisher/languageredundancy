{-# LANGUAGE OverloadedStrings #-}
module Main where

import Shelly
import Data.Text.Lazy
import Data.Text.Lazy.IO as TextIO
import Data.Monoid
import Text.Printf
import System.Directory

languages = [
  ( "JavaScript",
    ["js"],
    [ ("twitter", "bootstrap")
    , ("joyent", "node")
    , ("jquery", "jquery")
    ]),
  ( "Ruby",
    ["rb"],
    [ ("rails", "rails")
    , ("FortAwesome", "Font-Awesome")
    , ("mxcl", "homebrew")
    ]),
  ( "Java",
    ["java"],
    [ ("nathanmarz", "storm")
    , ("elasticsearch", "elasticsearch")
    , ("JakeWharton", "ActionBarSherlock")
    ]),
  ( "Haskell",
    ["hs"],
    [ ("jgm", "pandoc")
    , ("yesodweb", "yesod")
    , ("jgm", "gitit")
    ]),
  ( "Python",
    ["py"],
    [ ("django", "django")
    , ("mitsuhiko", "flask")
    , ("kennethreitz", "requests")
    ]),
  ( "PHP",
    ["php"],
    [ ("symfony", "symfony")
    , ("EllisLab", "CodeIgniter")
    , ("laravel", "laravel")
    ]),
  ( "C#",
    ["cs"],
    [ ("SignalR", "SignalR")
    , ("hbons", "SparkleShare")
    , ("ServiceStack", "ServiceStack")
    ]),
  ( "C",
    ["c", "h"],
    [ ("torvalds","linux")
    , ("antirez","redis")
    , ("git","git")
    ])
  ]

repo :: [Text] -> (Text, Text) -> IO Double
repo exts (user, name) = shelly $ silently $ do
  exists <- liftIO $ doesDirectoryExist (unpack name)
  when (not exists) $ run_ "git" ["clone", "https://github.com/" <> user <> "/" <> name]

  let pred = intercalate " -o " $ Prelude.map (\ext -> "-name '*." <> ext <> "'") exts
  let fullText = "find " <> name <> " -type f " <> pred <> " | xargs cat "

  uncompressed <- escaping False $ run (fromText $ fullText <> "         | wc -c") []
  compressed   <- escaping False $ run (fromText $ fullText <> " | xz -9 | wc -c") []

  return $ parse compressed / parse uncompressed

  where parse s = read . unpack . strip $ s

language :: (Text, [Text], [(Text, Text)]) -> IO ()
language (name, exts, repos) = do
  TextIO.putStrLn $ name <> ":"
  rats <- flip mapM repos $ \r -> do
    ratio <- repo exts r
    TextIO.putStrLn $ "  " <> fst r <> "/" <> snd r <> ": " <> pc ratio
    return ratio
  TextIO.putStrLn $ "  average: " <> pc (avg rats)
  where
    pc ratio = pack (show $ round $ ratio * 100) <> "%"
    avg xs = toRational (sum xs) / toRational (Prelude.length xs)

main = mapM_ language languages