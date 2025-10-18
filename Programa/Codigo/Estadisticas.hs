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

contar :: (Ord k) => (D.Venta -> k) -> [D.Venta] -> M.Map k Int
contar f vs =
  let add m v = M.insertWith (+) (f v) 1 m
  in foldl add M.empty vs

sumarMonto :: (Ord k) => (D.Venta -> k) -> [D.Venta] -> M.Map k Double
sumarMonto f vs =
  let add m v = M.insertWith (+) (f v) (convertidor (D.total v)) m
  in foldl add M.empty vs

-- === Top 5 categorías más vendidas ===
top5CategoriasMasVendidas :: D.EstadoApp -> IO ()
top5CategoriasMasVendidas estado = do
  let mc = sumarPor D.categoria (D.ventas estado)
      lista = take 5 $ reverse $ sortOn snd (M.toList mc)
  putStrLn "Top 5 categorías más vendidas:\n"
  imprimirTabla ["#", "Categoría", "Cantidad"]
    [ [show i, categoria, show cantidad] | (i, (categoria, cantidad)) <- zip [1..] lista ]

-- === Producto más vendido ===
productoMasVendido :: D.EstadoApp -> IO ()
productoMasVendido estado = do
  let mp = sumarPor D.producto_nombre (D.ventas estado)
      lista = take 1 $ reverse $ sortOn snd (M.toList mp)
  putStrLn "Producto más vendido:\n"
  imprimirTabla ["Producto", "Cantidad"]
    [ [producto, show cantidad] | (i, (producto, cantidad)) <- zip [1..] lista ]

-- == Categoría con menor participación (Cantidad) ===
categoriaMenorParticipacion :: D.EstadoApp -> IO ()
categoriaMenorParticipacion estado = do
  let mc = sumarPor D.categoria (D.ventas estado)
      lista = take 1 $ sortOn snd (M.toList mc)
  putStrLn "Categoría con menor participación:\n"
  imprimirTabla ["Categoría", "Cantidad"]
    [ [categoria, show cantidad] | (i, (categoria, cantidad)) <- zip [1..] lista ]




imprimirVenta :: D.Venta -> IO ()
imprimirVenta v = do
  putStrLn $ "ID: "++ show (D.venta_id v)
  putStrLn $ "Fecha: "++ show (D.fecha v)
  putStrLn $ "Producto: "++ D.producto_nombre v
  putStrLn $ "Categoría: "++ D.categoria v
  putStrLn $ "Cantidad: "++ show (convertidor (D.cantidad v))
  putStrLn $ "Precio Unitario: "++ show (convertidor (D.precio_unitario v))
  putStrLn $ "Total: "++ show (convertidor (D.total v))


ventaAltaBaja :: D.EstadoApp -> IO ()
ventaAltaBaja estado = do
  let mc = sumarMonto D.venta_id (D.ventas estado)
      lista = take 1 $ reverse $ sortOn snd (M.toList mc)
      lista2 = take 1 $ sortOn snd (M.toList mc)
  let [(idMax, _)] = lista
  let [(idMin, _)] = lista2
  let ventas = D.ventas estado
  let filasMayor = head[ v | v <- ventas, D.venta_id v == idMax]
  let filasMenor = head[ v | v <- ventas, D.venta_id v == idMin]
  putStrLn "\n"
  putStrLn "Venta con mayor monto total:\n"
  imprimirVenta filasMayor
  putStrLn "\nVenta con menor monto total:\n"
  imprimirVenta filasMenor


cantidadVentasCategoria :: D.EstadoApp -> IO ()
cantidadVentasCategoria estado = do
  let mc = contar D.categoria (D.ventas estado)
      lista = sortOn snd (M.toList mc)
  putStrLn "Cantidad de ventas por categoría:\n"
  imprimirTabla ["Categoría", "Cantidad de ventas"]
    [ [categoria, show cantidad] | (i, (categoria, cantidad)) <- zip [1..] lista ]

-- === Resumen general ===
resumenGeneral :: D.EstadoApp -> IO ()
resumenGeneral estado = do
  putStrLn "Resumen general de ventas\n"
  cantidadVentasCategoria estado
  ventaAltaBaja estado



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
        "2" -> productoMasVendido estado >> pausa >> loop
        "3" -> categoriaMenorParticipacion estado >> pausa >> loop
        "4" -> resumenGeneral estado >> pausa >> loop
        "0" -> return estado
        _   -> putStrLn "Opción no válida." >> pausa >> loop


