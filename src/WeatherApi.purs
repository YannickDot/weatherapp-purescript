module WeatherApi where

import Prelude

import Data.Either (Either(..))
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Foreign (ForeignError)

import Affjax as Ajax
import Affjax.ResponseFormat as ResponseFormat
import Simple.JSON as JSON

type Url
  = String

type Token
  = String

type WeatherData
  = {id :: Int, main :: String, description :: String, icon :: String}

type WindData
  = {speed :: Number, deg :: Maybe Int}

type Coords
  = {lon :: Number, lat :: Number}

type SysData
  = {"type" :: Int, id :: Int, message :: Number, country :: String, sunrise :: Int, sunset :: Int}

type MainWeatherData
  = {temp :: Number, pressure :: Int, humidity :: Int, temp_min :: Number, temp_max :: Number}

type WeatherResult
  = {id :: Int, name :: String, cod :: Int, coord :: Coords, main :: MainWeatherData, base :: String, weather :: Array WeatherData, visibility :: Int, wind :: WindData, sys :: SysData, dt :: Int}

type City
  = {coord :: Coords, country :: String, id :: Int, name :: String, population :: Int}

type ForecastData
  = {clouds :: {all :: Int}, dt :: Int, dt_txt :: String, main :: {grnd_level :: Number, humidity :: Int, pressure :: Number, sea_level :: Number, temp :: Number, temp_kf :: Number, temp_max :: Number, temp_min :: Number}, sys :: {pod :: String}, weather :: Array WeatherData, wind :: {speed :: Number, deg :: Maybe Number}}

type ForecastResult
  = {city :: City, country :: Maybe String, cod :: String, message :: Number, cnt :: Int, list :: Array ForecastData}

data DownloadStatus a
  = Downloaded a
  | DownloadFailed
  | JsonParsingFailed (NonEmptyList ForeignError)

data ResourceType
  = Forecast
  | Weather

baseUrl :: Url
baseUrl = "http://api.openweathermap.org/data/2.5/"

type JsonParser a
  = String -> Either (NonEmptyList ForeignError) a

buildUrl :: ResourceType -> Token -> String -> Url
buildUrl resType apiKey location = baseUrl <> path <> "?q=" <> location <> "&apikey=" <> apiKey
  where
  path ::
    String
  path = case resType of
    Forecast -> "forecast"
    Weather -> "weather"

makeRequest :: forall a. JsonParser a -> Url -> Aff (DownloadStatus a)
makeRequest parser url = do
  response <- Ajax.get ResponseFormat.string url
  case response.body of
    Left e -> pure DownloadFailed
    Right str -> case parser str of
      Left parseErrors -> pure $ JsonParsingFailed parseErrors
      Right json -> pure $ Downloaded json

getWeather :: Token -> String -> Aff (DownloadStatus WeatherResult)
getWeather apiKey location = makeRequest parser url
  where
    parser = JSON.readJSON
    url ::
      Url
    url = buildUrl Weather apiKey location

getForecast :: Token -> String -> Aff (DownloadStatus ForecastResult)
getForecast apiKey location = makeRequest parser url
  where
    parser = JSON.readJSON
    url ::
      Url
    url = buildUrl Forecast apiKey location
