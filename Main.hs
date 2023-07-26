{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShowId)
import Data.IORef

type API = "articles" :> QueryParam "apiKey" String :> Get '[JSON] [Article]
      :<|> "search" :> QueryParam "apiKey" String :> QueryParam "author" String :> QueryParam "title" String :> QueryParams "keyword" String :> Get '[JSON] [Article]

data Resp = Resp
    {
        articles :: [Article]

    } deriving (Eq, Show, Generic)


instance FromJSON Resp
instance ToJSON Resp
type CacheStore = IORef ([Article])

data Source = Source
    {
        id :: Maybe String,
        name :: String
    } deriving (Eq, Show, Generic)

instance FromJSON Source
instance ToJSON Source

data Article = Article
  { source :: Source,
  title :: String,
  author :: Maybe String,
  description :: Maybe String,
  url :: String,
  content :: Maybe String
  } deriving (Eq, Show, Generic)

instance FromJSON Article
instance ToJSON Article

server :: CacheStore -> Server API
server cachedData = 
    getArticles cachedData :<|> searchArticles cachedData

getArticles :: CacheStore -> Maybe String -> Handler [Article]
getArticles cachedData apiKey = do
  manager <- liftIO $ newManager tlsManagerSettings
  articles <- liftIO $ fetchArticles manager apiKey
  liftIO $ atomicWriteIORef cachedData articles
  return articles

searchArticles :: CacheStore -> Maybe String -> Maybe String -> Maybe String ->  [String] -> Handler [Article]
searchArticles cachedData apiKey author title keywords =
  if keywords == [] then
    do
        articlesFromCache <- liftIO $ fetchArticlesFromCacheStore cachedData author title
        if articlesFromCache == [] then fetchfromAPI else pure articlesFromCache
  else fetchfromAPI
    where
        fetchfromAPI = do
            manager <- liftIO $ newManager tlsManagerSettings
            articles <- liftIO $ searchArticlesByKeywords manager apiKey author title keywords
            return articles

fetchArticles :: Manager -> Maybe String -> IO [Article]
fetchArticles manager apiKey = do
  request <- case apiKey of
    Just key -> parseRequest $ "https://newsapi.org/v2/top-headlines?language=en&apiKey=" ++ key
    Nothing -> parseRequest "https://newsapi.org/v2/top-headlines"
  response <- httpLbs (request
        { requestHeaders = [("User-Agent", "KSVNEWS")]
        } ) manager
  let jsonBody = responseBody response
  case eitherDecode jsonBody of
    Left err -> do
      putStrLn $ "Error parsing response1: " ++ err
      return []
    Right article -> return (articles article)

searchArticlesByKeywords :: Manager -> Maybe String -> Maybe String -> Maybe String -> [String]  -> IO [Article]
searchArticlesByKeywords _ Nothing Nothing Nothing [] = do
    putStrLn $ "Error parsing query params please enter atleast one among author title or keywords "
    return []
searchArticlesByKeywords manager apiKey author' title' keywords = do
  let keywordsQueryString = traceShowId $ foldl(\acc a -> acc ++ "&q=" ++ a) "" keywords
  request <- case apiKey of
    Just key -> parseRequest $ "https://newsapi.org/v2/top-headlines?everything&apiKey=" ++ key ++ keywordsQueryString
    Nothing -> parseRequest $ "https://newsapi.org/v2/everything"
  response <- httpLbs (request
        { requestHeaders = [("User-Agent", "KSVNEWS")]
        } ) manager
  let jsonBody = responseBody response
  case eitherDecode (traceShowId jsonBody) of
    Left err -> do
      putStrLn $ "Error parsing response: " ++ err
      return []
    Right article -> do
        let condition = case (author', title') of
                            (Just auth, Just  titl) -> (\a b -> a == Just auth && b== titl)
                            (Just auth , _) -> (\a _ -> a == Just auth)
                            (_, Just titl) -> (\_ b -> b == titl)
                            (_, _) -> (\_ _ -> True)
        return $ filter (\a -> condition (author a) (title a)) $ articles article

api :: Proxy API
api = Proxy

app :: CacheStore -> Application
app cachedData = serve api (server cachedData)

main :: IO ()
main = do
  putStrLn "Running API on http://localhost:8080"
  cachedData <- cacheStore
  run 8080 (app cachedData)


cacheStore :: IO CacheStore
cacheStore = newIORef []


fetchArticlesFromCacheStore :: CacheStore -> Maybe String -> Maybe String -> IO [Article]
fetchArticlesFromCacheStore _ Nothing Nothing = pure []
fetchArticlesFromCacheStore cacheStore author' title' = do
    cachedData <- readIORef cacheStore
    let condition = case (author', title') of
                            (Just auth, Just  titl) -> (\a b -> a == Just auth && b== titl)
                            (Just auth , _) -> (\a _ -> a == Just auth)
                            (_, Just titl) -> (\_ b -> b == titl)
                            (_, _) -> (\_ _ -> True)
    return $ filter (\a -> condition (author a) (title a)) cachedData