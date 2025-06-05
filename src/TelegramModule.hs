{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TelegramModule (run, BotConfig(..)) where

import Control.Applicative
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, isInfixOf, toLower, pack, unpack)
import qualified Data.Text as T
import System.Random (randomRIO)
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser
import Telegram.Bot.Simple.Reply (reply, replyText)

randomResponses :: [Text]
randomResponses = 
    [ "А вот это совсем нехорошо."
    , "Консультация"
    , "Кажется, вас можно уже за аватарку забанить."
    , "Спасибо что напомнили"
    , "Подробности?"
    ]

getRandomResponse :: IO Text
getRandomResponse = do
    idx <- randomRIO (0, length randomResponses - 1)
    return $ randomResponses !! idx

data ResponseType 
    = JavaResponse
    | QuestionResponse
    | WhyResponse
    | WhereResponce
    | ArtemResponse
    | PvcResponce
    | CanHandleMessage
    | WhenResponce
    | NoResponse

classifyMessage :: Text -> ResponseType
classifyMessage msg
    | any (`T.isInfixOf` lowerMsg) ["java", "джава", "жава"]            = JavaResponse
    | "?" `T.isInfixOf` msg                                             = QuestionResponse
    | "почему" `T.isInfixOf` lowerMsg                                   = WhyResponse
    | "когда"  `T.isInfixOf` lowerMsg                                   = WhenResponce
    | "где"    `T.isInfixOf` lowerMsg                                   = WhereResponce
    | any (`T.isInfixOf` lowerMsg) ["артем", "артём", "тема", "тёма"]   = ArtemResponse
    | any (`T.isInfixOf` lowerMsg) ["айша", "аиша", "пвц", "pvc"]       = PvcResponce
    | any (`T.isInfixOf` lowerMsg) ["что делать", "подскажи", "помоги", "подскажите", "помогите"] = CanHandleMessage
    | otherwise                                                         = NoResponse
  where
    lowerMsg = T.toLower msg

getResponseText :: ResponseType -> Text
getResponseText = \case
    JavaResponse     -> "нет"
    QuestionResponse -> "консультация"
    WhyResponse      -> "Думайте над вопросом почему. Тогда будет понимание, а не ответ из книжки. На консультации если спросите — расскажу."
    WhenResponce     -> "в обозримом будущем."
    WhereResponce    -> "в лекциях."
    ArtemResponse    -> "Если \"Артем\" не признается кто он такой тут и не загрузит эту информацию в бота сегодня — оценка за его доклад будет утеряна безвозвратно и без возможности пересдачи."
    PvcResponce      -> "Айша, досвидания"
    CanHandleMessage -> "вы справитесь."
    NoResponse       -> ""

data Model = Model
    { 
    }

data Action
    = Start
    | Help
    | HandleMessage Text

initialModel :: Model
initialModel = Model {}

tgBot :: BotApp Model Action
tgBot = BotApp
    { botInitialModel = initialModel
    , botAction = flip updateToAction
    , botHandler = handleAction
    , botJobs = []
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
            replyText startMessage
        
        Help -> model <# do
            replyText helpMessage
        
        HandleMessage msg -> model <# do
            case getResponseText (classifyMessage msg) of
                "" -> do
                    shouldReply <- liftIO $ (randomRIO (2 :: Int, 100 :: Int) >>= return . (< 10))      
                    when shouldReply $ do
                        randomPhrase <- liftIO getRandomResponse
                        replyText randomPhrase
                response -> replyText response

    startMessage = "Добрый вечер"
    
    helpMessage = T.unlines
        [ "/help - список доступных команд"
        ]

data BotConfig = BotConfig
    { 
        token :: Token
    }

run :: BotConfig -> IO ()
run conf = do
  env <- defaultTelegramClientEnv (token conf)
  startBot_ (conversationBot updateChatId tgBot) env