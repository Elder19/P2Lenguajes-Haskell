{-# LANGUAGE OverloadedStrings #-}
module Estadisticas (menuEstadisticas) where        
import qualified Datos as D
import System.IO (hFlush, stdout)



-- ===== Utilidades =====
pausa :: IO ()
pausa = do
  putStrLn "\nPresione ENTER para continuar..."
  _ <- getLine
  return ()



-- ===== Menú =====
menuEstadisticas :: D.EstadoApp -> IO D.EstadoApp
menuEstadisticas estado = loop
  where
    loop = do
      putStrLn "=================================================="
      putStrLn "               MENÚ DE ESTADÍSTICAS               "
      putStrLn "=================================================="
      putStrLn "1) Top 5 productos más vendidos"
      putStrLn "2) Producto más vendido por categoría"
      putStrLn "3) Categoría con menor participación (Cantidad)"
      putStrLn "4) Resumen general"
      putStrLn "0) Volver"
      putStr   "Seleccione una opción: "
      hFlush stdout
      op <- getLine
      putStrLn ""
      case op of
        "1" -> putStrLn "1"            >> pausa >> loop
        "2" -> putStrLn "2"           >> pausa >> loop
        "3" -> putStrLn "3"           >> pausa >> loop
        "4" -> putStrLn "4"           >> pausa >> loop
        "0" -> return estado
        _   -> putStrLn "Opción no válida." >> pausa >> loop
