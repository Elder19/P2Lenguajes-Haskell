{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Datos
  ( Venta(..)
  , EstadoApp(..)
  , Rechazo(..)
  , leerVentasJSON
  ) where

import GHC.Generics (Generic)
import Data.Aeson   (FromJSON, eitherDecodeFileStrict')
import Data.Time    (Day)

-- ====== Tipos ======
data Rechazo = Rechazo Int String
  deriving (Show, Eq)

data Venta = Venta
  { venta_id        :: !Int
  , fecha           :: !Day
  , producto_id     :: !Int
  , producto_nombre :: !String
  , categoria       :: !String
  , cantidad        :: !(Maybe Double)
  , precio_unitario :: !(Maybe Double)
  , total           :: !(Maybe Double)
  } deriving (Show, Eq, Generic)

instance FromJSON Venta

data EstadoApp = EstadoApp
  { ventas  :: [Venta]
  , errores :: [Rechazo]
  } deriving (Show, Eq)

-- ====== Lector ======
leerVentasJSON :: FilePath -> IO (Either String [Venta])
leerVentasJSON ruta = eitherDecodeFileStrict' ruta
