{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Datos
  ( Venta(..)
  , EstadoApp(..)
  , Rechazo(..)
  , leerVentasJSON
  , ventaId              
  ) where

import GHC.Generics (Generic)
import Data.Aeson   (ToJSON, FromJSON, eitherDecodeFileStrict')
import Data.Time    (Day)

-- ====== Tipos ======
data Rechazo = Rechazo Int String
  deriving (Show, Eq, Generic)
instance ToJSON   Rechazo
instance FromJSON Rechazo

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
instance ToJSON   Venta
instance FromJSON Venta

data EstadoApp = EstadoApp
  { ventas  :: [Venta]
  , errores :: [Rechazo]
  } deriving (Show, Eq, Generic)
instance ToJSON   EstadoApp
instance FromJSON EstadoApp

-- ====== Helpers ======
ventaId :: Venta -> Int
ventaId = venta_id

-- ====== Lector ======
leerVentasJSON :: FilePath -> IO (Either String [Venta])
leerVentasJSON ruta = eitherDecodeFileStrict' ruta
