-- Tipos.hs


module Tipos
  ( Venta(..)
  , Rechazo(..)
  , EstadoApp(..)
  ) where

import Data.Time (Day)

-- | Representa una venta cargada desde el archivo JSON.
data Venta = Venta
  { idVenta        :: Int
  , fecha          :: Day
  , idProducto     :: Int
  , nombreProducto :: String
  , categoria      :: String
  , cantidad       :: Maybe Int
  , precioUnitario :: Maybe Double
  , total          :: Double
  } deriving (Show, Eq)

-- | Registra los errores ocurridos durante la importación.
data Rechazo = Rechazo
  { indice :: Int
  , causa  :: String
  } deriving (Show, Eq)


data EstadoApp = EstadoApp
  { ventas  :: [Venta]
  , errores :: [Rechazo]
  } deriving (Show, Eq)
