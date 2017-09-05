{-# LANGUAGE RecordWildCards #-}

module CMLocus.KML
    ( Point(..)
    , Coord(..)
    , pointsToKML
    )
  where

import Data.Monoid ((<>))

import Text.XML.Light
    ( Attr(Attr)
    , Node
    , Element
    , showTopElement
    , unode
    , unqual
    )

data Point = Point
    { name :: String
    , desc :: String
    , coord :: Coord
    }
  deriving (Show, Eq, Ord)

data Coord = Coord
    { latitude :: Double
    , longitude :: Double
    }
  deriving (Show, Eq, Ord)

pointsToKML :: Point -> [Point] -> String
pointsToKML ownPoint otherPoints =
    showTopElement $ kmlTop $ icons <> [ownPlacemark] <> otherPlacemarks
  where
    ownPlacemark = kmlPlacemark "map_marker_own" ownPoint
    otherPlacemarks = map (kmlPlacemark "map_marker") otherPoints

kmlTop :: Node t => t -> Element
kmlTop content = unode "kml" (xmlns, unode "Document" content)
  where
    xmlns = Attr (unqual "xmlns") "http://www.opengis.net/kml/2.2"

kmlPlacemark :: String -> Point -> Element
kmlPlacemark icon Point{..} = unode "Placemark"
    [ unode "name" name
    , unode "description" desc
    , unode "styleUrl" ("#" <> icon)
    , unode "Point" $ unode "coordinates" $ kmlCoord coord
    ]

kmlCoord :: Coord -> String
kmlCoord Coord{..} = show longitude <> "," <> show latitude <> ",0"

kmlStyleIcon :: String -> String -> Element
kmlStyleIcon styleId iconHref = unode "Style" (idAttr, iconStyle)
  where
    idAttr = Attr (unqual "id") styleId
    iconStyle = unode "IconStyle" $ unode "Icon" $ unode "href" iconHref

icons :: [Element]
icons =
    [ kmlStyleIcon "map_marker" "https://raw.githubusercontent.com/criticalmaps/criticalmaps-android/2296a8b9/app/src/main/res/drawable-nodpi/map_marker.png"
    , kmlStyleIcon "map_marker_own" "https://raw.githubusercontent.com/criticalmaps/criticalmaps-android/2296a8b9/app/src/main/res/drawable-nodpi/map_marker_own.png"
    ]
