{-# LANGUAGE OverloadedStrings #-}
module Estadisticas (menuEstadisticas) where        
import qualified Datos as D
import System.IO (hFlush, stdout)
import Data.List (sortOn, groupBy)
import Data.Function (on)
import qualified Data.Map.Strict as M


-- ===== Utilidades =====
pausa :: IO ()
pausa = do
  putStrLn "\nPresione ENTER para continuar..."
  _ <- getLine
  return ()



convertidor :: Maybe Double -> Double
convertidor = maybe 0 id

-- | Imprime una tabla simple en consola (sin cálculos de ancho).
imprimirTabla :: [String] -> [[String]] -> IO ()
imprimirTabla headers rows = do
  putStrLn $ unwords headers
  putStrLn "------------------------------------------"
  mapM_ (putStrLn . unwords) rows

-- | Agrupa y suma cantidades por clave
sumarPor :: (Ord k) => (D.Venta -> k) -> [D.Venta] -> M.Map k Double
sumarPor f vs =
  let add m v = M.insertWith (+) (f v) (convertidor (D.cantidad v)) m
  in foldl add M.empty vs

-- === Top 5 categorías más vendidas ===
top5CategoriasMasVendidas :: D.EstadoApp -> IO ()
top5CategoriasMasVendidas estado = do
  let mc = sumarPor D.categoria (D.ventas estado)
      xs = take 5 $ reverse $ sortOn snd (M.toList mc)
  putStrLn "Top 5 categorías más vendidas:\n"
  imprimirTabla ["#", "Categoría", "Cantidad"]
    [ [show i, categoria, show q] | (i, (categoria, q)) <- zip [1..] xs ]



-- ===== Menú =====
menuEstadisticas :: D.EstadoApp -> IO D.EstadoApp
menuEstadisticas estado = loop
  where
    loop = do
      putStrLn "=================================================="
      putStrLn "               MENÚ DE ESTADÍSTICAS               "
      putStrLn "=================================================="
      putStrLn "1) Top 5 Categorías más vendidas"
      putStrLn "2) Producto más vendido"
      putStrLn "3) Categoría con menor participación (Cantidad)"
      putStrLn "4) Resumen general"
      putStrLn "0) Volver"
      putStr   "Seleccione una opción: "
      hFlush stdout
      op <- getLine
      putStrLn ""
      case op of
        "1" -> top5CategoriasMasVendidas estado >> pausa >> loop
        "2" -> putStrLn "2"   estado >> pausa >> loop
        "3" -> putStrLn "3"   estado >> pausa >> loop
        "4" -> putStrLn "4"           >> pausa >> loop
        "0" -> return estado
        _   -> putStrLn "Opción no válida." >> pausa >> loop


