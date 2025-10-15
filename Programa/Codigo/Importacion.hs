{-# LANGUAGE OverloadedStrings #-}
-- Importacion.hs
-- Módulo encargado de importar los datos de ventas desde un archivo JSON.

module Importacion (menuImportacion, cargarArchivoJSON) where

import System.IO (hFlush, stdout)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.Time (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Control.Monad (forM_)
import Datos (EstadoApp(..), Rechazo(..), Venta(..))

data VentaArchivo = VentaArchivo
  { vVentaId    :: Maybe Int
  , vFechaTxt   :: Maybe String
  , vProductoId :: Maybe Int
  , vProdNombre :: Maybe String
  , vCategoria  :: Maybe String
  , vCantidad   :: Maybe Int
  , vPrecioUnit :: Maybe Double
  , vTotal      :: Maybe Double
  } deriving (Show)

instance FromJSON VentaArchivo where
  parseJSON = withObject "Venta" $ \o -> do
    VentaArchivo
      <$> o .:? "venta_id"
      <*> o .:? "fecha"
      <*> o .:? "producto_id"
      <*> o .:? "producto_nombre"
      <*> o .:? "categoria"
      <*> o .:? "cantidad"
      <*> o .:? "precio_unitario"
      <*> o .:? "total"

convertirVenta :: Int -> VentaArchivo -> Either Rechazo Venta
convertirVenta indiceReg w = do
  idV      <- oblig "venta_id"         vVentaId
  fechaTxt <- oblig "fecha"            vFechaTxt
  fechaOk  <- convertirFecha fechaTxt
  idP      <- oblig "producto_id"      vProductoId
  nomP     <- oblig "producto_nombre"  vProdNombre
  cat      <- oblig "categoria"        vCategoria
  tot      <- oblig "total"            vTotal
  pure $ Venta idV fechaOk idP nomP cat (fmap fromIntegral (vCantidad w))
                                       (vPrecioUnit w)
                                       (Just tot)
  where
    oblig :: String -> (VentaArchivo -> Maybe a) -> Either Rechazo a
    oblig campo getter =
      maybe (Left (Rechazo indiceReg ("Falta el campo obligatorio: " ++ campo)))
            Right
            (getter w)

    convertirFecha :: String -> Either Rechazo Day
    convertirFecha texto =
      case parseTimeM True defaultTimeLocale "%F" texto of
        Just d  -> Right d
        Nothing -> Left (Rechazo indiceReg "Formato de fecha inválido (use yyyy-mm-dd)")

menuImportacion :: EstadoApp -> IO EstadoApp
menuImportacion estado = do
  putStr "Ingrese la ruta del archivo JSON a importar: "
  hFlush stdout
  ruta <- getLine
  putStrLn ""
  cargarArchivoJSON ruta estado

cargarArchivoJSON :: FilePath -> EstadoApp -> IO EstadoApp
cargarArchivoJSON ruta estado = do
  contenido <- BL.readFile ruta
  case eitherDecode contenido :: Either String [VentaArchivo] of
    Left errorJSON -> do
      putStrLn (" Error al leer el archivo JSON: " ++ errorJSON)
      pure estado
    Right lista -> do
      let resultados              = zipWith convertirVenta [0..] lista
          (rechazados, validados) = separarResultados resultados
          nuevoEstado             = estado
            { ventas  = ventas estado ++ validados
            , errores = rechazados
            }
      putStrLn ("Registros cargados correctamente: " ++ show (length validados))
      putStrLn ("Registros rechazados: " ++ show (length rechazados))
      forM_ rechazados $ \(Rechazo i c) ->
        putStrLn ("   - Registro " ++ show i ++ ": " ++ c)
      pure nuevoEstado
  where
    separarResultados :: [Either a b] -> ([a],[b])
    separarResultados =
      foldr
        (\x (izq, der) -> either (\l -> (l:izq, der)) (\r -> (izq, r:der)) x)
        ([],[])
