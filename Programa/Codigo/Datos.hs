{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Datos
  ( Venta(..)
  , leerVentasJSON
  ) where

import GHC.Generics (Generic)
import Data.Aeson   (FromJSON, eitherDecodeFileStrict')
import Data.Time    (Day)
import System.Directory
  ( doesFileExist, listDirectory
  , getCurrentDirectory, canonicalizePath
  )
import System.FilePath
  ( (</>), takeDirectory, takeFileName )

-- ====== Tipo ======
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

-- ====== Lector ======
leerVentasJSON :: FilePath -> IO (Either String [Venta])
leerVentasJSON ruta = eitherDecodeFileStrict' ruta
