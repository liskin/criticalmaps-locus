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

pointsToKML :: [Point] -> String
pointsToKML = showTopElement . kmlTop . map kmlPlacemark

kmlTop :: Node t => t -> Element
kmlTop content = unode "kml" (xmlns, unode "Document" content)
    where
        xmlns = Attr (unqual "xmlns") "http://www.opengis.net/kml/2.2"

kmlPlacemark :: Point -> Element
kmlPlacemark Point{..} = unode "Placemark"
    [ unode "name" name
    , unode "description" desc
    , unode "Point" $ unode "coordinates" $ kmlCoord coord
    ]

kmlCoord :: Coord -> String
kmlCoord Coord{..} = show longitude <> "," <> show latitude <> ",0"
