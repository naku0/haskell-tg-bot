{-# LANGUAGE OverloadedStrings #-}

module Main where
import TelegramModule (run, BotConfig(..))
import Env (extractEnv)
import Telegram.Bot.API (Token(..))
import qualified Data.Text as T


main :: IO()
main = do
  content <- readFile "./.env"
  let tgKeyRaw = extractEnv content "TG_API"
      tgKey = Token (T.pack tgKeyRaw)
      conf = BotConfig{ token = tgKey }
  putStrLn "Бот запущен!"
  run conf