{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Database.PostgreSQL.Simple
import Data.Monoid (mconcat)
import Data.Text.Lazy

main = do
    connection <- getConnection
    text <- getList connection
    scotty 3000 $
        listResource text

listResource :: [Text] -> ScottyM ()
listResource xs = get "/" $ do
    html $ mconcat (["<h1>"] ++  [s]  ++ ["</h1>"])
    where
        s = intercalate "</h1><h1>" xs

getList :: Connection -> IO [Text]
getList conn = do
    onlys <- query_ conn "select text from haskell"
    return $ Prelude.map (\(Only x) -> x) onlys

getConnection :: IO Connection
getConnection = do
    connectPostgreSQL "host=localhost dbname=haskell"
