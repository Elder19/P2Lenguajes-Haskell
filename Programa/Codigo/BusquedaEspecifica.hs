{-# LANGUAGE OverloadedStrings #-}
module BusquedaEspecifica (menuBusquedaEspecifica) where
import qualified Datos as D
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import System.IO (hFlush, stdout)

-- | Función principal para buscar ventas por rango de fechas
menuBusquedaEspecifica :: D.EstadoApp -> IO D.EstadoApp
menuBusquedaEspecifica estado = do
  putStrLn "Ingrese la fecha de inicio (yyyy-mm-dd):"
  hFlush stdout
  fechaInicioStr <- getLine
  putStrLn "Ingrese la fecha de fin (yyyy-mm-dd):"
  hFlush stdout
  fechaFinStr <- getLine

  case (parseFecha fechaInicioStr, parseFecha fechaFinStr) of
    (Just fi, Just ff) -> do
        let ventasFiltradas = filtrarPorRango fi ff (D.ventas estado)
        if null ventasFiltradas
          then putStrLn "No hay ventas en el rango de fechas indicado."
          else mapM_ mostrarVenta ventasFiltradas
    _ -> putStrLn "Formato de fecha incorrecto. Use yyyy-mm-dd."

  putStrLn "\nPresione ENTER para continuar..."
  _ <- getLine
  return estado

-- | Convierte un String en Day
parseFecha :: String -> Maybe Day
parseFecha = parseTimeM True defaultTimeLocale "%Y-%m-%d"

-- | Filtra las ventas que estén dentro del rango de fechas
filtrarPorRango :: Day -> Day -> [D.Venta] -> [D.Venta]
filtrarPorRango inicio fin = filter (\v -> let f = D.fecha v in f >= inicio && f <= fin)

-- | Muestra la información de una venta
mostrarVenta :: D.Venta -> IO ()
mostrarVenta v = putStrLn $
    "ID: " ++ show (D.venta_id v) ++
    ", Fecha: " ++ show (D.fecha v) ++
    ", Producto: " ++ D.producto_nombre v ++
    ", Cantidad: " ++ show (D.cantidad v) ++
    ", Precio unitario: " ++ show (D.precio_unitario v) ++
    ", Total: " ++ show (D.total v)
