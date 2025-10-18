
module Persistencia
  ( rutaEstado
  , cargarEstado
  , guardarEstado
  ) where

import qualified Datos as D
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (eitherDecode, encode)
import System.Directory (doesFileExist)

rutaEstado :: FilePath
rutaEstado = "estado.json"

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

guardarEstado :: D.EstadoApp -> IO ()
guardarEstado st = BL.writeFile rutaEstado (encode st)
