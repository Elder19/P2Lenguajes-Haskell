{-# LANGUAGE OverloadedStrings #-}
module Estadisticas (menuEstadisticas) where        
import qualified Datos as D
import System.IO (hFlush, stdout)


-- | Menú de estadísticas
menuEstadisticas :: D.EstadoApp -> IO D.EstadoApp
menuEstadisticas estado = do
    putStrLn "=================================================="                 
    putStrLn "               MENÚ DE ESTADÍSTICAS                "      
    putStrLn "=================================================="
    putStrLn "1) Top 5 productos más vendidos"
    putStrLn "2) Producto mas vendido por categoría"
    putStrLn "3) Categoria con menor participación (Cantidad)"
    putStrLn "4) Resumen general"
    putStrLn "0) Volver"

