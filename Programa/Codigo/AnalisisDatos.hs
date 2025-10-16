{-# LANGUAGE ScopedTypeVariables #-}
module AnalisisDatos (menuAnalisisDatos) where

import qualified Datos as D
import System.IO (hFlush, stdout)

pause :: IO ()
pause = do
  putStr "Presione ENTER para continuar..."
  _ <- getLine
  putStrLn ""

menuAnalisisDatos :: D.EstadoApp -> IO D.EstadoApp
menuAnalisisDatos estado0 = loop estado0
  where
    loop :: D.EstadoApp -> IO D.EstadoApp
    loop est = do
      putStrLn "------------------ ANÁLISIS DE DATOS ------------------"
      putStrLn "1) Total de ventas (suma de importes)"
      putStrLn "2) Total de ventas mensuales y anuales"
      putStrLn "3) Promedio de ventas por categoría por año"
      putStrLn "4) Volver"
      putStr   "Seleccione una opción: "
      hFlush stdout
      op <- getLine
      case op of
        "1" -> do
          putStrLn ">>> [Pendiente] Total de ventas (suma de los importes)."
          pause
          loop est
        "2" -> do
          putStrLn ">>> [Pendiente] Total de ventas mensuales y anuales."
          pause
          loop est
        "3" -> do
          putStrLn ">>> [Pendiente] Promedio de ventas por categoría por año."
          pause
          loop est
        "4" -> return est
        _   -> do
          putStrLn "Opción no válida."
          pause
          loop est
