{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}

module CMLocus (main) where

import Data.Monoid ((<>))
import System.IO.Unsafe (unsafePerformIO)

import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Control.Lens ((%~), lens)
import Control.Monad.Trans.Except (runExcept)
import Data.Default (def)
import Data.Map (Map)
import Network.HTTP.Types (Query, simpleQueryToQuery, status200, status400, status404, status501)
import Network.Wai (Middleware, Request(requestMethod), Response, pathInfo, responseBuilder)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (gzip)
import Network.Wai.Middleware.RequestLogger
    ( IPAddrSource(FromFallback)
    , OutputFormat(Apache)
    , logStdoutDev
    , mkRequestLogger
    , outputFormat
    )
import Network.Wai.Parse (defaultParseRequestBodyOptions, parseRequestBodyEx)
import qualified Data.ByteString.Char8 as B (ByteString, unpack)
import qualified Data.Map as M (toList, filter)
import qualified Data.Text as T (unpack)
import qualified Data.Text.Encoding as T (decodeUtf8)
import "hcoord" LatLng (LatLng, mkLatLng, distance)
import "hcoord" Datum (wgs84Datum)

import CMLocus.Options (Opts(..), getOpts)
import qualified CMLocus.CriticalMaps as CM (Location(..), Request(..), Reply(..), send)
import qualified CMLocus.KML as KML (Point(..), Coord(..), pointsToKML)

main :: IO ()
main = do
    Opts{..} <- getOpts
    run port . logger dev . gzip def $ app

logger :: Bool -> Middleware
logger True = logStdoutDev
logger False = unsafePerformIO $ mkRequestLogger def{ outputFormat = Apache FromFallback }

app :: Request -> (Response -> IO b) -> IO b
app req respond = case (requestMethod req, pathInfo req) of
    ("POST", []) -> respond =<< handlePost =<< parseBody req
    ("POST", _) -> respond $ responseNotFound
    ("GET", _) -> respond $ responseNotFound
    (_, _) -> respond $ responseNotImplemented

handlePost :: Query -> IO Response
handlePost = getParamsM [["lat"], ["lon"], ["device"]] $ \inp -> do
    let req = mkCMRequest inp
    let locationsLens = lens CM.locations (\rep@CM.Reply{..} l -> rep{ CM.locations = l })
    let closeLocations' = locationsLens %~ closeLocations closeLimitKm (CM.location req)
    responseKml . KML.pointsToKML . pointsFromCMReply . closeLocations' <$> CM.send req
  where
    closeLimitKm = 100

mkCMRequest :: [B.ByteString] -> CM.Request
mkCMRequest inp = req
  where
    [lat, lon, dev] = inp
    req = CM.Request{ location = loc, device = T.decodeUtf8 dev }
    loc = CM.Location{ latitude = lat', longitude = lon', timestamp = Nothing }
    lat' = conv . read . B.unpack $ lat
    lon' = conv . read . B.unpack $ lon

    conv :: Double -> Int
    conv l = round (l * 1000000)

pointsFromCMReply :: CM.Reply -> [KML.Point]
pointsFromCMReply CM.Reply{..} =
    [ mkPoint dev loc | (dev, loc) <- M.toList locations ]
  where
    mkPoint dev loc =
        KML.Point{ name = T.unpack dev, desc = "", coord = mkCoord loc }
    mkCoord CM.Location{..} =
        KML.Coord{ latitude = conv latitude, longitude = conv longitude }

    conv :: Int -> Double
    conv l = fromIntegral l / 1000000

closeLocations :: Double -> CM.Location -> Map a CM.Location -> Map a CM.Location
closeLocations limit myLoc = M.filter $ \l -> distance myLoc' (locToLatLng l) < limit
  where
    myLoc' = locToLatLng myLoc

    locToLatLng :: CM.Location -> LatLng
    locToLatLng CM.Location{..} =
        let Right l = runExcept $ mkLatLng (conv latitude) (conv longitude) 0 wgs84Datum in l

    conv :: Int -> Double
    conv l = fromIntegral l / 1000000

parseBody :: Request -> IO Query
parseBody =
    fmap (simpleQueryToQuery . fst) . parseRequestBodyEx defaultParseRequestBodyOptions file
  where
    file _ _ _ = error "file uploads forbidden"

responseKml :: String -> Response
responseKml = responseBuilder status200 headers . fromString
  where
    headers = [("Content-Type", "application/vnd.google-earth.kml+xml")]

responseBadReq :: String -> Response
responseBadReq s = responseBuilder status400 headers $ fromString s
  where
    headers = [("Content-Type", "text/plain")]

responseNotFound :: Response
responseNotFound = responseBuilder status404 headers $ fromString "not found"
  where
    headers = [("Content-Type", "text/plain")]

responseNotImplemented :: Response
responseNotImplemented = responseBuilder status501 headers $ fromString "not implemented"
  where
    headers = [("Content-Type", "text/plain")]

getParamsM :: (Monad m) => [[B.ByteString]] -> ([B.ByteString] -> m Response) -> Query -> m Response
getParamsM ps c q =
    case sequence $ map getPar ps of
        Right ps' -> c ps'
        Left err -> pure $ responseBadReq err
  where
    getPar [] = error "getPar []"
    getPar (p:ds) =
        case lookup p q of
            Just (Just v) -> Right v
            _ -> case ds of
                [] -> Left $ "missing param: " <> B.unpack p
                [d] -> Right d
                _ -> error "getPar [,,]"
