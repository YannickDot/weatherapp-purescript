module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, Fiber, launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import WeatherApi (DownloadStatus(..))
import WeatherApi as WeatherApi

apiKey :: String
apiKey = ""

logAff :: String -> Aff (Unit)
logAff s = liftEffect $ log s

main :: Effect (Fiber Unit)
main = launchAff do
  weatherJson <- WeatherApi.getWeather apiKey "london"
  forecastJson <- WeatherApi.getForecast apiKey "london,UK"

  case weatherJson of
    Downloaded weather -> logAff $ show weather
    DownloadFailed -> logAff "DownloadFailed"
    JsonParsingFailed e -> logAff $ (show e) <> " JsonParsingFailed"

  case forecastJson of
    Downloaded forecast -> logAff $ show forecast
    DownloadFailed -> logAff "DownloadFailed"
    JsonParsingFailed e -> logAff $ (show e) <> " JsonParsingFailed"
