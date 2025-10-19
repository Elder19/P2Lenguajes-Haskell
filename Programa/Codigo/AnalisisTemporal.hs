{-# LANGUAGE OverloadedStrings #-}
module AnalisisTemporal (menuAnalisisTemporal) where

import qualified Datos as D
import Data.Maybe (fromMaybe)
import Data.Time (Day, toGregorian, dayOfWeek, DayOfWeek(..))
import qualified Data.Map.Strict as M
import Data.List (sortOn)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

-- =====================================
-- AUXILIARES
-- =====================================

-- Determina el trimestre según el mes
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

-- Traduce los días de la semana al español
diaSemanaEsp :: DayOfWeek -> String
diaSemanaEsp dia = case dia of
  Monday    -> "Lunes"
  Tuesday   -> "Martes"
  Wednesday -> "Miércoles"
  Thursday  -> "Jueves"
  Friday    -> "Viernes"
  Saturday  -> "Sábado"
  Sunday    -> "Domingo"

-- Funcionalidad de mes --

-- Agrupa ventas por mes
ventasPorMes :: [D.Venta] -> M.Map (Integer, Int) Double
ventasPorMes = foldl agregar M.empty
  where
    agregar mapa v =
      case D.total v of
        Just t  -> M.insertWith (+) (mesYAnio (D.fecha v)) t mapa
        Nothing -> mapa

-- Funcionalidad del dia de la semana

-- Calcula cantidad de ventas por día de la semana
ventasPorDiaSemana :: [D.Venta] -> M.Map DayOfWeek Int
ventasPorDiaSemana = foldl agregar M.empty
  where
    agregar mapa v =
      let dia = dayOfWeek (D.fecha v)
      in M.insertWith (+) dia 1 mapa

-- Funcionalidad de mes y dia activo

mesYDiaMasActivo :: D.EstadoApp -> IO ()
mesYDiaMasActivo estado = do
  let ventas = D.ventas estado
      totalesMes = ventasPorMes ventas
      listaMeses = reverse $ sortOn snd (M.toList totalesMes)
      conteoDias = ventasPorDiaSemana ventas
      listaDias = reverse $ sortOn snd (M.toList conteoDias)

  case (listaMeses, listaDias) of
    ([], _) -> putStrLn "No hay ventas registradas."
    (_, []) -> putStrLn "No hay ventas registradas."
    (((anio, mes), total) : _, (dia, cant) : _) -> do
      putStrLn $ "Mes con mayor venta total: " ++ show mes ++ "/" ++ show anio
      putStrLn $ "Total vendido: " ++ show total
      putStrLn $ "Día más activo: " ++ diaSemanaEsp dia ++ " con " ++ show cant ++ " transacciones."

-- Funcionalidad de Trimestres --

ventasPorTrimestre :: [D.Venta] -> M.Map (Integer, Int) Double
ventasPorTrimestre = foldl agregar M.empty
  where
    agregar mapa v =
      case D.total v of
        Just t  ->
          let (anio, mes, _) = toGregorian (D.fecha v)
              tri = trimestre mes
          in M.insertWith (+) (anio, tri) t mapa
        Nothing -> mapa

-- Calcula tasas de crecimiento entre trimestres consecutivos
calcularTasas :: [(Int, Double)] -> IO ()
calcularTasas [] = return ()
calcularTasas [_] = return ()
calcularTasas ((t1,v1):(t2,v2):rest) = do
  if v1 == 0 && v2 == 0 then
    putStrLn $ "Trimestre " ++ show t1 ++ " → " ++ show t2 ++ ": No hay ventas en ambos trimestres"
  else if v2 == 0 then
    putStrLn $ "Trimestre " ++ show t1 ++ " → " ++ show t2 ++ ": No hay ventas en trimestre " ++ show t2
  else if v1 == 0 then
    putStrLn $ "Trimestre " ++ show t1 ++ " → " ++ show t2 ++ ": No se puede calcular tasa (ventas trimestre " ++ show t1 ++ " = 0)"
  else
    putStrLn $ "Trimestre " ++ show t1 ++ " → " ++ show t2 ++ ": " ++ show (((v2 - v1) / v1) * 100) ++ "%"
  calcularTasas ((t2,v2):rest)

-- Función principal para análisis trimestral
tasaCrecimientoTrimestral :: D.EstadoApp -> Integer -> IO ()
tasaCrecimientoTrimestral estado anio = do
  let mapa = ventasPorTrimestre (D.ventas estado)
      trimestres = [1..4]
      ventasAnio = [(t, M.findWithDefault 0 (anio,t) mapa) | t <- trimestres]

  if all ((==0) . snd) ventasAnio
    then putStrLn $ "No hay ventas registradas para el año " ++ show anio
    else do
      putStrLn $ "\nVentas por trimestre para el año " ++ show anio ++ ":"
      mapM_ (\(t,v) -> putStrLn $ "Trimestre " ++ show t ++ ": " ++ show v) ventasAnio

      putStrLn "\nTasas de crecimiento entre trimestres:"
      calcularTasas ventasAnio

-- Resumen por trimestre
resumenTrimestral :: D.EstadoApp -> IO ()
resumenTrimestral estado = do
  let mapa = ventasPorTrimestre (D.ventas estado)
      lista = sortOn fst (M.toList mapa)
  putStrLn "Resumen de ventas por trimestre:\n"
  mapM_ (\((a,t),v) -> putStrLn $ show a ++ " - Trimestre " ++ show t ++ ": " ++ show v) lista

-- Menu --
menuAnalisisTemporal :: D.EstadoApp -> IO D.EstadoApp
menuAnalisisTemporal estado = loop
  where
    loop = do
      putStrLn "==============================================="
      putStrLn "          MENÚ DE ANÁLISIS TEMPORAL            "
      putStrLn "==============================================="
      putStrLn "1) Mes con mayor venta total y día más activo"
      putStrLn "2) Tasa de crecimiento trimestral"
      putStrLn "3) Resumen por trimestre"
      putStrLn "0) Volver"
      putStr   "Seleccione una opción: "
      hFlush stdout
      op <- getLine
      case op of
        "1" -> mesYDiaMasActivo estado >> pausa >> loop
        "2" -> do
          putStr "Ingrese el año a analizar: "
          hFlush stdout
          input <- getLine
          case readMaybe input :: Maybe Integer of
            Just y  -> tasaCrecimientoTrimestral estado y
                      >> pausa
                      >> loop
            Nothing -> putStrLn "Entrada inválida. Por favor ingrese un número válido."
                      >> pausa
                      >> loop
        "3" -> resumenTrimestral estado >> pausa >> loop
        "0" -> return estado
        _   -> putStrLn "Opción no válida." >> pausa >> loop


pausa :: IO ()
pausa = putStrLn "\nPresione ENTER para continuar..." >> getLine >> return ()
