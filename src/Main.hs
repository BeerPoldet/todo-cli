{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson hiding (Options)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.List.Safe ((!!))
import           Data.String.Utils
import           Data.Time
import qualified Data.Yaml as Yaml
import           GHC.Generics
import           Prelude hiding ((!!))
import           Options.Applicative hiding (infoParser)
import           System.Directory
import           System.IO.Error

type ItemIndex = Int
type ItemTitle = String
type ItemDescription = Maybe String
type ItemPriority = Maybe Priority
type ItemDueBy = Maybe LocalTime

data Priority = Low | Normal | High deriving (Generic, Show)
instance ToJSON Priority
instance FromJSON Priority

data TodoList = TodoList [Item] deriving (Generic, Show)
instance ToJSON TodoList
instance FromJSON TodoList

data Item = Item
  { title:: ItemTitle
  , description:: ItemDescription
  , priority:: ItemPriority
  , dueBy:: ItemDueBy
  } deriving (Generic, Show)
instance ToJSON Item
instance FromJSON Item

data ItemUpdate = ItemUpdate
  { titleUpdate :: Maybe ItemTitle
  , descriptionUpdate :: Maybe ItemDescription
  , priorityUpdate :: Maybe ItemPriority
  , dueByUpdate :: Maybe ItemDueBy
  } deriving Show

data Options = Options FilePath Command deriving Show

data Command =
  Info
  | Init
  | List
  | Add Item
  | View ItemIndex
  | Update ItemIndex ItemUpdate
  | Remove ItemIndex
  deriving Show

infoParser :: Parser Command
infoParser = pure Info

initParser :: Parser Command
initParser = pure Init

listParser :: Parser Command
listParser = pure List

addParser :: Parser Command
addParser = Add <$> addItemParser

addItemParser :: Parser Item
addItemParser = Item
  <$> argument str (metavar "TITLE" <> help "title")
  <*> optional itemDescriptionValueParser
  <*> optional itemPriorityValueParser
  <*> optional itemDueByValueParser

viewParser :: Parser Command
viewParser = View <$> itemIndexParser

updateParser :: Parser Command
updateParser = Update <$> itemIndexParser <*> updateItemParser

removeParser :: Parser Command
removeParser = Remove <$> itemIndexParser

updateItemParser :: Parser ItemUpdate
updateItemParser = ItemUpdate
  <$> optional updateItemTitleParser
  <*> optional updateItemDescriptionParser
  <*> optional updateItemPriorityParser
  <*> optional updateItemDueByParser

updateItemTitleParser :: Parser ItemTitle
updateItemTitleParser = itemTitleValueParser

updateItemDescriptionParser :: Parser ItemDescription
updateItemDescriptionParser = 
  Just <$> itemDescriptionValueParser
  <|> flag' Nothing (long "clear-desc")

updateItemPriorityParser :: Parser ItemPriority
updateItemPriorityParser = 
  Just <$> itemPriorityValueParser
  <|> flag' Nothing (long "clear-priority")

updateItemDueByParser :: Parser ItemDueBy
updateItemDueByParser = 
  Just <$> itemDueByValueParser
  <|> flag' Nothing (long "clear-due-by")

commandParser :: Parser Command
commandParser = subparser $ mconcat
  [ command "info" (info infoParser $ progDesc "Show info")
  , command "init" (info initParser $ progDesc "Initialize items")
  , command "list" (info listParser $ progDesc "List all items")
  , command "add" (info addParser $ progDesc "Add item")
  , command "view" (info viewParser $ progDesc "View item")
  , command "update" (info updateParser $ progDesc "Update item")
  , command "remove" (info removeParser $ progDesc "Remove item")
  ]

defaultDataPath :: FilePath
defaultDataPath = "~/.to-do.yaml"

optionsParser :: Parser Options
optionsParser = Options
  <$> dataPathParser
  <*> commandParser

dataPathParser :: Parser FilePath
dataPathParser = strOption $
  value defaultDataPath
  <> long "data-path"
  <> short 'p'
  <> metavar "DATAPATH"
  <> help ("path to data file (default " ++ defaultDataPath ++ ")")

itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto (metavar "ITEMINDEX" <> help "index of item")

itemTitleValueParser :: Parser String
itemTitleValueParser = strOption $
  long "title"
  <> short 't'
  <> metavar "TITLE"
  <> help "title"

itemDescriptionValueParser :: Parser String
itemDescriptionValueParser = strOption $
  long "desc"
  <> short 'd'
  <> metavar "DESCRIPTION"
  <> help "description"

itemPriorityValueParser :: Parser Priority
itemPriorityValueParser = option readPriority (long "priority"
  <> short 'p'
  <> metavar "PRIORITY"
  <> help "priority")
  where readPriority = eitherReader $ \arg ->
          case arg of
            "1" -> Right Low
            "2" -> Right Normal
            "3" -> Right High
            _ -> Left $ "Invalid priority value " ++ arg

itemDueByValueParser :: Parser LocalTime
itemDueByValueParser = 
  option readDateTime (long "due-by" <> short 'b' <> metavar "DUEBY" <> help "due-by")
  where
    readDateTime = eitherReader $ \arg ->
      case parseDateTimeMaybe arg of
        (Just dateTime) -> Right dateTime
        Nothing -> Left $ "Date/Time string must be in " ++ dateTimeFormat ++ " format"
    parseDateTimeMaybe = parseTimeM False defaultTimeLocale dateTimeFormat
    dateTimeFormat = "%Y/%m/%d %H:%M:%S"

main :: IO ()
main = (execParser $ info optionsParser $ progDesc "Todo list") 
  >>= \(Options dataPath command) -> getHomeDirectory 
  >>= \homeDir -> 
    let expendedDataPath = replace "~" homeDir dataPath
        dueBy = LocalTime (ModifiedJulianDay 0) (TimeOfDay 0 0 0)
    in runReaderT (run command) expendedDataPath 

run :: Command -> ReaderT FilePath IO ()
run Info = showInfo
run Init = initItems
run List = viewItems
run (Add item) = addItem item
run (View idx) = viewItem idx
run (Update idx itemUpdate) = updateItem idx itemUpdate
run (Remove idx) = removeItem idx

showItem :: ItemIndex -> Item -> IO ()
showItem idx (Item title mbDescription mbPriority mbDueBy) = do
  putStrLn $ "[" ++ show idx ++ "]: " ++ title
  putStr "  Description: "
  putStrLn $ showField id mbDescription
  putStr "  Priority: "
  putStrLn $ showField show mbPriority
  putStr "  Due by: "
  putStrLn $ showField (formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S") mbDueBy

showField :: (a -> String) -> Maybe a -> String
showField f (Just x) = f x
showField _ Nothing = "(not set)"

removeAt :: [a] -> Int -> Maybe [a]
removeAt xs idx =
  if idx < 0 || idx >= length xs
  then Nothing  
  else
    let (before, after) = splitAt idx xs
        (_: after') = after
        xs' = before ++ after'
    in Just xs'  

updateAt :: [a] -> Int -> (a -> a) -> Maybe [a]
updateAt xs idx f =
    if idx < 0 || idx >= length xs
    then Nothing
    else
        let (before, after) = splitAt idx xs
            element : after' = after
            xs' = before ++ f element : after'
        in Just xs'

showInfo :: ReaderT FilePath IO ()
showInfo = ReaderT $ \dataPath -> do
    putStrLn $ "Data file path: " ++ dataPath
    exists <- doesFileExist dataPath
    if exists
    then do
        s <- BS.readFile dataPath
        let mbTodoList = either (const Nothing) Just $ Yaml.decodeEither' s
        case mbTodoList of
            Nothing -> putStrLn $ "Status: file is invalid"
            Just (TodoList items) -> putStrLn $ "Status: contains " ++ show (length items) ++ " items"
    else putStrLn $ "Status: file does not exist"

initItems :: ReaderT FilePath IO ()
initItems = writeTodoList $ TodoList []

viewItems :: ReaderT FilePath IO ()
viewItems = readTodoList >>= \(TodoList items) ->
  liftIO $ forM_ (zip [0..] items) (\(idx, item) -> showItem idx item)

addItem :: Item -> ReaderT FilePath IO ()
addItem item = (readTodoList >>= \(TodoList items) ->
  writeTodoList (TodoList $ item : items)) >>
    (liftIO $ putStrLn "Add successfully")
    -- (ReaderT $ \_ -> putStrLn "Add successfully")

viewItem :: ItemIndex -> ReaderT FilePath IO ()
viewItem idx = readTodoList >>= \(TodoList items) ->
  case items !! idx of
    Nothing -> liftIO $ putStrLn $ "Item at " ++ show idx ++ " not found"
    Just item -> liftIO $ showItem idx item

removeItem :: ItemIndex -> ReaderT FilePath IO ()
removeItem idx = readTodoList >>= \(TodoList items) ->
  case items `removeAt` idx of
    Nothing -> liftIO . putStrLn $ "Item at " ++ show idx ++ " not found"
    Just items -> writeTodoList (TodoList items) >>
      (liftIO . putStrLn $ "Remove successfully")

updateItem :: ItemIndex -> ItemUpdate -> ReaderT FilePath IO ()
updateItem idx (ItemUpdate mbTitle mbDescription mbPriority mbDueBy) = 
  readTodoList >>= \(TodoList items) ->
    let update (Item title description priority dueBy) = Item
            (updateField mbTitle title)
            (updateField mbDescription description)
            (updateField mbPriority priority)
            (updateField mbDueBy dueBy)
        updateField (Just value) _ = value
        updateField Nothing value = value
        mbItems = updateAt items idx update
    in case mbItems of
        Nothing -> liftIO . putStrLn $ "Invalid item index"
        Just items' -> 
          (writeTodoList $ TodoList items') >>
           (liftIO . putStrLn) "Update successfully"

writeTodoList :: TodoList -> ReaderT FilePath IO ()
writeTodoList todoList = ReaderT $ \dataPath -> 
  BS.writeFile dataPath $ Yaml.encode todoList

readTodoList :: ReaderT FilePath IO TodoList
readTodoList = ReaderT $ \dataPath -> (catchJust
  (\e -> if isDoesNotExistError e then Just () else Nothing)
  (BS.readFile dataPath >>= return . (either (const Nothing) Just) . Yaml.decodeEither')
  (const $ return $ Just $ TodoList []))
  >>= maybe (error "YAML file is corrupt") return
