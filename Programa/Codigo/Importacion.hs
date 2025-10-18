{-# LANGUAGE OverloadedStrings #-}
-- Codigo/Importacion.hs
-- Módulo encargado de importar los datos de ventas desde un archivo JSON.

module Importacion (menuImportacion, cargarArchivoJSON) where

import System.IO (hFlush, stdout)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.Time (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Control.Monad (forM_)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Datos (EstadoApp(..), Rechazo(..), Venta(..))
import qualified Datos as D
import Persistencia (guardarEstado)

-- | Estructura intermedia para leer el JSON tal cual llega del archivo.
--   Permite validar y transformar antes de construir 'Venta'.
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

-- | Decodificación JSON de un elemento del archivo.
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

-- | Valida y convierte un 'VentaArchivo' en 'Venta'.
--   Si falta un campo obligatorio o la fecha es inválida, devuelve 'Rechazo'.
convertirVenta :: Int -> VentaArchivo -> Either Rechazo Venta
convertirVenta indiceReg w = do
  idV      <- oblig "venta_id"         vVentaId
  fechaTxt <- oblig "fecha"            vFechaTxt
  fechaOk  <- convertirFecha fechaTxt
  idP      <- oblig "producto_id"      vProductoId
  nomP     <- oblig "producto_nombre"  vProdNombre
  cat      <- oblig "categoria"        vCategoria
  let cant   = fmap fromIntegral (vCantidad w)
      precio = vPrecioUnit w
      tot    = vTotal w
  pure $ Venta idV fechaOk idP nomP cat cant precio tot
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

-- | Pausa simple para la interfaz de consola.
pause :: IO ()
pause = do
  putStrLn "Presione ENTER para continuar..."
  _ <- getLine
  putStrLn ""

-- | Elimina duplicados dentro del lote actual, conservando la última aparición por 'venta_id'.
dedupLote :: [Venta] -> [Venta]
dedupLote = M.elems . M.fromList . fmap (\v -> (D.ventaId v, v))

-- | Menú de importación: pide la ruta y despacha la carga del archivo.
menuImportacion :: EstadoApp -> IO EstadoApp
menuImportacion estado = do
  putStr "Ingrese la ruta del archivo JSON a importar: "
  hFlush stdout
  ruta <- getLine
  putStrLn ""
  cargarArchivoJSON ruta estado

-- | Carga un archivo JSON, valida registros, separa rechazados y aplica la política:
--   - Primera importación (estado vacío): se permite guardar duplicados del archivo.

cargarArchivoJSON :: FilePath -> EstadoApp -> IO EstadoApp
cargarArchivoJSON ruta estado = do
  contenido <- BL.readFile ruta
  case eitherDecode contenido :: Either String [VentaArchivo] of
    Left errorJSON -> do
      putStrLn (" Error al leer el archivo JSON: " ++ errorJSON)
      pure estado

    Right lista -> do
      let resultados               = zipWith convertirVenta [0..] lista
          (rechazados, validados)  = separarResultados resultados
          esPrimera                = null (ventas estado)

          (ventasFinales, resumenInsercion) =
            if esPrimera
              then
                -- Primera importación: se aceptan duplicados dentro del archivo.
                ( ventas estado ++ validados
                , "(primera importación) agregados: " ++ show (length validados)
                ++ ", duplicados intra-lote permitidos."
                )
              else
                -- No es primera: dedup intra-lote + filtro contra IDs ya existentes.
                let idsExistentes    = S.fromList (map D.ventaId (ventas estado))
                    validadosUnicos  = dedupLote validados
                    noRepetidos      = filter (\v -> D.ventaId v `S.notMember` idsExistentes)
                                               validadosUnicos
                    agregados        = length noRepetidos
                    ignoradosEstado  = length validadosUnicos - agregados
                    ignoradosIntra   = length validados - length validadosUnicos
                in ( ventas estado ++ noRepetidos
                   , "agregados: " ++ show agregados
                     ++ ", ignorados por duplicado (estado): " ++ show ignoradosEstado
                     ++ ", ignorados por duplicado (): " ++ show ignoradosIntra
                   )

          nuevoEstado = estado
            { ventas  = ventasFinales
            , errores = errores estado ++ rechazados
            }

      putStrLn ("Registros válidos en archivo: " ++ show (length validados))
      putStrLn ("Registros rechazados por validación: " ++ show (length rechazados))
      putStrLn ("Resumen de inserción: " ++ resumenInsercion)
      forM_ rechazados $ \(Rechazo i c) ->
        putStrLn ("   - Registro " ++ show i ++ ": " ++ c)

      guardarEstado nuevoEstado
      putStrLn "Estado guardado en 'estado.json'."
      pause
      pure nuevoEstado
  where
    -- | Parte una lista de 'Either' en rechazos y valores válidos.
    separarResultados :: [Either a b] -> ([a],[b])
    separarResultados =
      foldr (\x (izq, der) -> either (\l -> (l:izq, der)) (\r -> (izq, r:der)) x) ([],[])
