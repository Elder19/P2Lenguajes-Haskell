{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Datos
  ( Venta(..)
  , EstadoApp(..)
  , Rechazo(..)
  , leerVentasJSON
  , ventaId       -- helper
  , categoria     -- helper (si lo usas en análisis)
  , fecha         -- reexportar, ya lo tienes en el record
  , total         -- idem
  ) where

import GHC.Generics (Generic)
import Data.Aeson   (FromJSON, ToJSON, eitherDecodeFileStrict')
import Data.Time    (Day)

-- ====== Tipos ======
data Rechazo = Rechazo Int String
  deriving (Show, Eq, Generic)
instance ToJSON Rechazo
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

instance FromJSON Venta
instance ToJSON   Venta

data EstadoApp = EstadoApp
  { ventas  :: [Venta]
  , errores :: [Rechazo]
  } deriving (Show, Eq, Generic)
instance ToJSON   EstadoApp
instance FromJSON EstadoApp

-- ====== Helpers ======
ventaId :: Venta -> Int
ventaId = venta_id

-- reexportar campos que ya existen en el record (opcional si los usas cualificados)
-- categoria, fecha, total ya están como campos del record, los “reexportamos” con el nombre.

-- ====== Lector ======
leerVentasJSON :: FilePath -> IO (Either String [Venta])
leerVentasJSON ruta = eitherDecodeFileStrict' ruta
