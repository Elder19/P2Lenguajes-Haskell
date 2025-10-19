{-# LANGUAGE OverloadedStrings #-}
module AnalisisTemporal (menuAnalisisTemporal) where
import qualified Datos as D
import Data.Maybe (fromMaybe)
import Data.Time (Day, toGregorian, dayOfWeek, DayOfWeek(..))
import qualified Data.Map.Strict as M
import Data.List (sortOn)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

-- Auxiliares --
-- Devuelve el trimestre según el mes
trimestre :: Int -> Int
trimestre mes
  | mes <= 3  = 1
  | mes <= 6  = 2
  | mes <= 9  = 3
  | otherwise = 4

-- Extrae año y mes de una fecha
mesYAnio :: Day -> (Integer, Int)
mesYAnio fecha =
  let (anio, mes, _) = toGregorian fecha
  in (anio, mes)

diaSemanaEsp :: DayOfWeek -> String
diaSemanaEsp dia = case dia of
  Monday    -> "Lunes"
  Tuesday   -> "Martes"
  Wednesday -> "Miércoles"
  Thursday  -> "Jueves"
  Friday    -> "Viernes"
  Saturday  -> "Sábado"
  Sunday    -> "Domingo"
-- Auxiliares --
--Funcionalidad de mes --

-- Agrupa ventas por mes
ventasPorMes :: [D.Venta] -> M.Map (Integer, Int) Double
ventasPorMes = foldl agregar M.empty
  where
    agregar mapa v =
      case D.total v of
        Just t  -> M.insertWith (+) (mesYAnio (D.fecha v)) t mapa
        Nothing -> mapa

mesConMayorVenta :: D.EstadoApp -> IO ()
mesConMayorVenta estado = do
  let ventas = D.ventas estado
      totalesMes = ventasPorMes ventas
      listaOrdenada = reverse $ sortOn snd (M.toList totalesMes)
  case listaOrdenada of
    [] -> putStrLn "No hay ventas registradas."
    (( (anio, mes), total) : _) -> do
      putStrLn $ "Mes con mayor venta total: " ++ show mes ++ "/" ++ show anio
      putStrLn $ "Total vendido: " ++ show total

-- Funcionalidad de mes --
-- Funcionalidad dia de la semana --

-- Calcula la cantidad de ventas por día de la semana
ventasPorDiaSemana :: [D.Venta] -> M.Map DayOfWeek Int
ventasPorDiaSemana = foldl agregar M.empty
  where
    agregar mapa v =
      let dia = dayOfWeek (D.fecha v)
      in M.insertWith (+) dia 1 mapa

-- Muestra el día más activo en español
diaMasActivo :: D.EstadoApp -> IO ()
diaMasActivo estado = do
  let conteo = ventasPorDiaSemana (D.ventas estado)
      lista = reverse $ sortOn snd (M.toList conteo)
  case lista of
    [] -> putStrLn "No hay ventas registradas."
    ((dia, cant) : _) ->
      putStrLn $ "Día más activo: " ++ diaSemanaEsp dia ++ " con " ++ show cant ++ " transacciones."

-- Funcionalidad dia de la semana --
