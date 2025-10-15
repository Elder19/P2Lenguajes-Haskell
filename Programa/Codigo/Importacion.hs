-- Codigo/Importacion.hs
module Importacion
  ( menuImportacion
  , imp
  ) where


menuImportacion :: s -> IO s
menuImportacion st = do
  putStrLn ">>> Presione ENTER para continuar..."
  _ <- getLine
  pure st


imp :: IO ()
imp = putStrLn ">>> [Importacion.imp] Hola desde Importacion.imp (stub)!"
