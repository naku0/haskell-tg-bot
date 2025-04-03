{-# LANGUAGE OverloadedStrings #-}

module TelegramModule (run, BotConfig(..)) where

import Control.Applicative
import Control.Monad (unless, when)
import Data.Text (Text, isInfixOf, toLower)
import qualified Data.Text as T
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

data Model = Model
    { -- later
    }

data Action
    = Start
    | Help
    | HandleMessage Text

initialModel :: Model
initialModel = Model { -- later
    }

tgBot :: BotApp Model Action
tgBot = BotApp
 {   botInitialModel = initialModel
 ,   botAction = flip updateToAction
 ,   botHandler = handleAction
 ,   botJobs = []
 }
 where
    updateToAction :: Model -> Update -> Maybe Action
    updateToAction _ = parseUpdate $
            Start         <$   command "start"
        <|> Help          <$   command "help"
        <|> HandleMessage <$> plainText

    handleAction :: Action -> Model -> Eff Action Model
    handleAction action model = case action of 
        Start -> model <# do
            reply (toReplyMessage startMessage)
        Help -> model  <# do
            reply (toReplyMessage helpMessage)
        HandleMessage msg -> model <# do
            when (T.isInfixOf "java" (T.toLower msg) || T.isInfixOf "джава" (T.toLower msg)) $ do
                reply (toReplyMessage "Фу!")
        
    startMessage = T.unlines
        ["Динаху"]
    helpMessage = T.unlines
        ["Пока тут ничего нет"]

data BotConfig = BotConfig
    { token               :: Token
    , botWelcomeMessage   :: Text
    , botHelpMessage      :: Text
    }

run :: BotConfig -> IO ()
run conf = do
  let botToken = token conf  
  env <- defaultTelegramClientEnv botToken 
  startBot_ (conversationBot updateChatId tgBot) env