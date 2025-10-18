{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Módulo: Datos
-- Descripción: Define los tipos principales y funciones básicas para manejar la información del sistema.
--
-- Contiene las estructuras de datos para las ventas, el estado general de la aplicación
-- y los registros rechazados, además de una función para cargar los datos desde un archivo JSON.

module Datos
  ( Venta(..)
  , EstadoApp(..)
  , Rechazo(..)
  , leerVentasJSON
  , ventaId
  , categoria
  , fecha
  , total
  ) where

import GHC.Generics (Generic)
import Data.Aeson   (FromJSON, ToJSON, eitherDecodeFileStrict')
import Data.Time    (Day)

-- | Representa un registro rechazado con su identificador y motivo.
data Rechazo = Rechazo Int String
  deriving (Show, Eq, Generic)
instance ToJSON Rechazo
instance FromJSON Rechazo

-- | Define la estructura de una venta, incluyendo posibles valores nulos
-- en cantidad, precio o total.
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

-- | Estado general del sistema con las ventas cargadas y los rechazos detectados.
data EstadoApp = EstadoApp
  { ventas  :: [Venta]
  , errores :: [Rechazo]
  } deriving (Show, Eq, Generic)
instance ToJSON   EstadoApp
instance FromJSON EstadoApp

-- | Obtiene el identificador de una venta.
ventaId :: Venta -> Int
ventaId = venta_id

-- | Lee un archivo JSON y devuelve las ventas decodificadas.
leerVentasJSON :: FilePath -> IO (Either String [Venta])
leerVentasJSON ruta = eitherDecodeFileStrict' ruta
