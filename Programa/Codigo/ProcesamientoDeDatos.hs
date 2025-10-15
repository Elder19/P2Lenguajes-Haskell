module ProcesamientoDeDatos (menuProcesamiento) where

import qualified Datos as D
import System.IO (hFlush, stdout)

menuProcesamiento :: D.EstadoApp -> IO D.EstadoApp
menuProcesamiento estado0 = loopPrincipal estado0
  where
    -- Menú principal de procesamiento
    loopPrincipal est = do
      putStrLn "------------------ PROCESAMIENTO DE DATOS ------------------"
      putStrLn "1) Completar datos faltantes"
      putStrLn "2) Eliminar duplicados"
      putStrLn "3) Volver"
      putStr   "Seleccione una opción: "
      hFlush stdout
      op <- getLine
      case op of
        "1" -> do
          est' <- submenuCompletar est
          loopPrincipal est'
        "2" -> do
          putStrLn ">>> Opción 'Eliminar duplicados' aún no implementada."
          pause
          loopPrincipal est
        "3" -> return est
        _   -> do
          putStrLn "Opción no válida."
          pause
          loopPrincipal est

    
    submenuCompletar est = do
      putStrLn "------------- COMPLETAR DATOS FALTANTES -------------"
      putStrLn "Elija técnica de imputación:"
      putStrLn "1) Moda"
      putStrLn "2) Media (promedio)"
      putStrLn "3) Mediana"
      putStrLn "4) Volver"
      putStr   "Seleccione una opción: "
      hFlush stdout
      t <- getLine
      case t of
        "1" -> do
          putStrLn ">>> Completar faltantes con 'Moda' aún no implementado."
          pause
          submenuCompletar est
        "2" -> do
          putStrLn ">>> Completar faltantes con 'Media' aún no implementado."
          pause
          submenuCompletar est
        "3" -> do
          putStrLn ">>> Completar faltantes con 'Mediana' aún no implementado."
          pause
          submenuCompletar est
        "4" -> return est
        _   -> do
          putStrLn "Opción no válida."
          pause
          submenuCompletar est

    pause = do
      putStr "Presione ENTER para continuar..."
      _ <- getLine
      putStrLn ""
