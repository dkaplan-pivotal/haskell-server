{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Database.PostgreSQL.Simple
import Data.Monoid (mconcat)
import Data.Text.Lazy (pack, unpack, Text)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)

main = do
    connection <- getConnection
    scotty 3000 $
        listResource connection

listResource :: Connection -> ScottyM ()
listResource connection = get "/" $ do
    xs <- liftIO $ getList connection
    html $ mconcat . stringToText . makeHtml . textToString $ xs
    where
        textToString :: [Text] -> [String]
        textToString = Prelude.map unpack
        makeHtml :: [String] -> [String]
        makeHtml d = ["<h1>"] ++ [(intercalate "</h1><h1>" d)] ++ ["</h1>"]
        stringToText :: [String] -> [Text]
        stringToText = Prelude.map pack

getList :: Connection -> IO [Text]
getList conn = do
    onlys <- query_ conn "select text from haskell"
    return $ Prelude.map (\(Only x) -> x) onlys

getConnection :: IO Connection
getConnection = do
    connectPostgreSQL "host=localhost dbname=haskell"
