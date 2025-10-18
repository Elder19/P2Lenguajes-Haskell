{-# LANGUAGE OverloadedStrings #-}
module Estadisticas (menuEstadisticas) where

import qualified Datos as D
import System.IO (hFlush, stdout)
import Data.List (sortOn)
import Data.Function (on)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)

-- ===== funciones Auxiliares =====
pausa :: IO ()
pausa = do
  putStrLn "\nPresione ENTER para continuar..."
  _ <- getLine
  return ()

-- | Convierte un Maybe Double a Double, usando 0 para Nothing.
convertidor :: Maybe Double -> Double
convertidor = maybe 0 id

--- | Imprime los detalles de una venta.
imprimirVenta :: D.Venta -> IO ()
imprimirVenta v = do
  putStrLn $ "ID: "++ show (D.venta_id v)
  putStrLn $ "Fecha: "++ show (D.fecha v)
  putStrLn $ "Producto: "++ D.producto_nombre v
  putStrLn $ "Categoría: "++ D.categoria v
  putStrLn $ "Cantidad: "++ show (convertidor (D.cantidad v))
  putStrLn $ "Precio Unitario: "++ show (convertidor (D.precio_unitario v))
  putStrLn $ "Total: "++ show (convertidor (D.total v))

-- | Imprime en consola.
imprimirTabla :: [String] -> [[String]] -> IO ()
imprimirTabla headers rows = do
  putStrLn $ unwords headers
  putStrLn "------------------------------------------"
  mapM_ (putStrLn . unwords) rows

-- ===== Exportar CSV =====

carpetaReportes :: FilePath
carpetaReportes = "REPORTES"

asegurarCarpetaReportes :: IO ()
asegurarCarpetaReportes = createDirectoryIfMissing True carpetaReportes

timestampAhora :: IO String
timestampAhora = do
  zt <- getZonedTime
  return (formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" zt)

-- Escapa comillas y encierra el campo entre comillas si hay coma, comilla o salto de línea.
csvEscape :: String -> String
csvEscape s =
  let needsQuotes = any (`elem` s) [',','"','\n','\r']
      escaped = concatMap (\c -> if c == '"' then "\"\"" else [c]) s
  in if needsQuotes then "\"" ++ escaped ++ "\"" else escaped

-- | exportarReporteCSV "Top5Categorias" ["#","Categoria","Cantidad"] filas
exportarReporteCSV :: String -> [String] -> [[String]] -> IO FilePath
exportarReporteCSV nombreReporte headers rows = do
  asegurarCarpetaReportes
  ts <- timestampAhora
  let file = carpetaReportes </> (ts ++ "_" ++ nombreReporte ++ ".csv")
      linea :: [String] -> String
      linea cols = concat $ zipWith (\i c -> (if i>0 then "," else "") ++ csvEscape c) [0..] cols
      contenido = unlines $
          [linea headers] ++ map linea rows
  writeFile file contenido
  putStrLn $ " Reporte CSV generado: " ++ file
  return file

-- ===== helpers de agregación =====

-- | Agrupa (Categoría o Producto) y suma la cantidad o el total.
sumarPor :: (Ord k) => (D.Venta -> k) -> [D.Venta] -> M.Map k Double
sumarPor f vs =
  let add m v = M.insertWith (+) (f v) (convertidor (D.cantidad v)) m
  in foldl add M.empty vs

-- | Cuenta ocurrencias de un campo (Categoría).
contar :: (Ord k) => (D.Venta -> k) -> [D.Venta] -> M.Map k Int
contar f vs =
  let add m v = M.insertWith (+) (f v) 1 m
  in foldl add M.empty vs

-- | Suma el monto total por clave (ID de venta).
sumarMonto :: (Ord k) => (D.Venta -> k) -> [D.Venta] -> M.Map k Double
sumarMonto f vs =
  let add m v = M.insertWith (+) (f v) (convertidor (D.total v)) m
  in foldl add M.empty vs

-- | Calcula la variedad de productos por categoría.
variedadPorCategoria :: [D.Venta] -> M.Map String (S.Set Int)
variedadPorCategoria ventas =
  let agregar mapa venta = M.insertWith S.union (D.categoria venta) (S.singleton (D.producto_id venta)) mapa
  in foldl agregar M.empty ventas

-- === Top 5 categorías más vendidas ===
top5CategoriasMasVendidas :: D.EstadoApp -> IO ()
top5CategoriasMasVendidas estado = do
  let mc = sumarPor D.categoria (D.ventas estado)
      lista = take 5 $ reverse $ sortOn snd (M.toList mc) 
  putStrLn "Top 5 categorías más vendidas:\n"
  let headers = ["#", "Categoría", "Cantidad"]
      rows = [ [show i, categoria, show cantidad] | (i, (categoria, cantidad)) <- zip [1..] lista ]
  imprimirTabla headers rows
  _ <- exportarReporteCSV "Top5CategoriasMasVendidas" headers rows
  return ()

-- === Producto más vendido ===
productoMasVendido :: D.EstadoApp -> IO ()
productoMasVendido estado = do
  let mp = sumarPor D.producto_nombre (D.ventas estado)
      lista = take 1 $ reverse $ sortOn snd (M.toList mp) 
  putStrLn "Producto más vendido:\n"
  let headers = ["Producto", "Cantidad"]
      rows = [ [producto, show cantidad] | (producto, cantidad) <- map id lista ]
  imprimirTabla headers rows
  _ <- exportarReporteCSV "ProductoMasVendido" headers rows
  return ()

-- == Categoría con menor participación (Cantidad) ===
categoriaMenorParticipacion :: D.EstadoApp -> IO ()
categoriaMenorParticipacion estado = do
  let mc = sumarPor D.categoria (D.ventas estado)
      lista = take 1 $ sortOn snd (M.toList mc) 
  putStrLn "Categoría con menor participación:\n"
  let headers = ["Categoría", "Cantidad"]
      rows = [ [categoria, show cantidad] | (categoria, cantidad) <- map id lista ]
  imprimirTabla headers rows
  _ <- exportarReporteCSV "CategoriaMenorParticipacion" headers rows
  return ()

-- === Venta con mayor y menor monto total ===
ventaAltaBaja :: D.EstadoApp -> IO ()
ventaAltaBaja estado = do
  let mc = sumarMonto D.venta_id (D.ventas estado)
      listaMax = take 1 $ reverse $ sortOn snd (M.toList mc)
      listaMin = take 1 $ sortOn snd (M.toList mc)          
  let [(idMax, _)] = listaMax
  let [(idMin, _)] = listaMin
  let ventas = D.ventas estado
  let vMax = head [ v | v <- ventas, D.venta_id v == idMax]
  let vMin = head [ v | v <- ventas, D.venta_id v == idMin]
  putStrLn "\nVenta con mayor monto total:\n"
  imprimirVenta vMax
  putStrLn "\nVenta con menor monto total:\n"
  imprimirVenta vMin

  -- exportar CSV con ambas filas
  let headers = ["Tipo","ID","Fecha","Producto","Categoría","Cantidad","PrecioUnitario","Total"]
      fila v t = [ t
                 , show (D.venta_id v)
                 , show (D.fecha v)
                 , D.producto_nombre v
                 , D.categoria v
                 , show (convertidor (D.cantidad v))
                 , show (convertidor (D.precio_unitario v))
                 , show (convertidor (D.total v))
                 ]
      rows = [ fila vMax "Mayor", fila vMin "Menor" ]
  _ <- exportarReporteCSV "VentaAltaBaja" headers rows
  return ()

-- === Cantidad de ventas por categoría ===
cantidadVentasCategoria :: D.EstadoApp -> IO ()
cantidadVentasCategoria estado = do
  let mc = contar D.categoria (D.ventas estado)
      lista = sortOn snd (M.toList mc) 
  putStrLn "Cantidad de ventas por categoría:\n"
  let headers = ["Categoría", "Cantidad de ventas"]
      rows = [ [categoria, show cantidad] | (categoria, cantidad) <- map id lista ]
  imprimirTabla headers rows
  _ <- exportarReporteCSV "CantidadVentasPorCategoria" headers rows
  return ()

-- === Categoría con mayor variedad de productos vendidos ===
categoriaMayorVariedad :: D.EstadoApp -> IO ()
categoriaMayorVariedad estado = do
  let lista  = variedadPorCategoria (D.ventas estado)         
      cuenta = M.map S.size lista                            
      listaOrdenada  = reverse $ sortOn snd (M.toList cuenta) 
  putStrLn "Categoría con mayor variedad de productos vendidos:\n"
  let headers = ["#", "Categoría", "VariedadProductos"]
      rows = [[show i, categoria, show cantidad] | (i, (categoria, cantidad)) <- zip [1..] listaOrdenada ]
  imprimirTabla headers rows
  _ <- exportarReporteCSV "CategoriaMayorVariedad" headers rows
  return ()

-- === Resumen general ===
resumenGeneral :: D.EstadoApp -> IO ()
resumenGeneral estado = do
  putStrLn "Resumen general de ventas\n"
  cantidadVentasCategoria estado
  ventaAltaBaja estado
  categoriaMayorVariedad estado

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
