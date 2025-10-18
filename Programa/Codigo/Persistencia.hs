

module Persistencia
  ( rutaEstado
  , cargarEstado
  , guardarEstado
  ) where

import qualified Datos as D
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (eitherDecode, encode)
import System.Directory (doesFileExist)

-- | Ruta del archivo donde se guarda el estado de la aplicación.
rutaEstado :: FilePath
rutaEstado = "estado.json"

-- | Carga el estado desde el archivo 'estado.json' si existe y es válido.

cargarEstado :: IO (Maybe D.EstadoApp)
cargarEstado = do
  existe <- doesFileExist rutaEstado
  if not existe
    then pure Nothing
    else do
      bs <- BL.readFile rutaEstado
      case eitherDecode bs of
        Left _err -> pure Nothing
        Right st  -> pure (Just st)

-- | Guarda el estado actual de la aplicación en formato JSON.
guardarEstado :: D.EstadoApp -> IO ()
guardarEstado st = BL.writeFile rutaEstado (encode st)

