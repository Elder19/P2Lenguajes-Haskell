{-# LANGUAGE ScopedTypeVariables #-}
module AnalisisDatos (menuAnalisisDatos) where

import qualified Datos as D
import System.IO (hFlush, stdout)
import System.Process (callCommand)

import Data.Maybe (catMaybes)
import Data.List  (sortOn)
import qualified Data.Map.Strict as M

import Data.Foldable (foldl')
import Data.Time.Calendar (toGregorian, Day)
import Text.Printf (printf)

-- | Pausa breve para la interfaz de consola.
pause :: IO ()
pause = do
  putStrLn "Presione ENTER para continuar..."
  _ <- getLine
  putStrLn ""
  putStrLn ""
  putStrLn ""

-- | Limpia la pantalla (usa 'clear').
limpiarPantalla :: IO ()
limpiarPantalla = callCommand "clear"

-- | Suma los importes declarados en 'total' (omite 'Nothing').
totalVentas :: [D.Venta] -> Double
totalVentas ventas = sum . catMaybes $ map D.total ventas

-- | Extrae (año, mes) desde la fecha de la venta.
anioMes :: D.Venta -> (Integer, Int)
anioMes v =
  let (y, m, _d) = toGregorian (D.fecha v)
  in (y, fromIntegral m)

-- | Extrae solo el año de la venta.
anio :: D.Venta -> Integer
anio v = let (y, _m, _d) = toGregorian (D.fecha v) in y

-- | Totales por (año, mes), en orden cronológico.
totalesMensuales :: [D.Venta] -> [((Integer, Int), Double)]
totalesMensuales vs =
  let step acc v = case D.total v of
        Just t  -> M.insertWith (+) (anioMes v) t acc
        Nothing -> acc
      mp = foldl' step M.empty vs
  in sortOn fst (M.toList mp)

-- | Totales por año, en orden ascendente.
totalesAnuales :: [D.Venta] -> [(Integer, Double)]
totalesAnuales vs =
  let step acc v = case D.total v of
        Just t  -> M.insertWith (+) (anio v) t acc
        Nothing -> acc
      mp = foldl' step M.empty vs
  in sortOn fst (M.toList mp)

-- | Imprime tabla simple de totales mensuales.
mostrarTotalesMensuales :: [((Integer, Int), Double)] -> IO ()
mostrarTotalesMensuales xs = do
  putStrLn "=== Totales mensuales ==="
  mapM_ (\((y,m),t) -> putStrLn (printf "%04d-%02d  $%.2f" y m t)) xs
  putStrLn ""

-- | Imprime tabla simple de totales anuales.
mostrarTotalesAnuales :: [(Integer, Double)] -> IO ()
mostrarTotalesAnuales xs = do
  putStrLn "=== Totales anuales ==="
  mapM_ (\(y,t) -> putStrLn (printf "%04d      $%.2f" y t)) xs
  putStrLn ""

-- | Promedio del 'total' por (año, categoría).
promedioPorCategoriaAnio :: [D.Venta] -> [((Integer, String), Double)]
promedioPorCategoriaAnio vs =
  let
    step :: M.Map (Integer, String) (Double, Int) -> D.Venta -> M.Map (Integer, String) (Double, Int)
    step acc v = case D.total v of
      Just t  ->
        let y   = anio v
            cat = D.categoria v
        in M.insertWith (\(s1,c1) (s2,c2) -> (s1+s2, c1+c2))
                        (y, cat) (t, 1) acc
      Nothing -> acc

    mp = foldl' step M.empty vs
    toAvg ((y,cat),(s,c)) = ((y,cat), s / fromIntegral c)
  in sortOn fst $ map toAvg (M.toList mp)

-- | Imprime promedio por categoría y año.
mostrarPromedioPorCategoriaAnio :: [((Integer, String), Double)] -> IO ()
mostrarPromedioPorCategoriaAnio xs = do
  putStrLn "=== Promedio de ventas por categoría por año ==="
  mapM_ (\((y,cat),avg) -> putStrLn (printf "%04d  %-20s  $%.2f" y cat avg)) xs
  putStrLn ""

-- | Menú de análisis: totales, agregados por tiempo y promedios por categoría.
menuAnalisisDatos :: D.EstadoApp -> IO D.EstadoApp
menuAnalisisDatos estado0 = loop estado0
  where
    loop :: D.EstadoApp -> IO D.EstadoApp
    loop est = do
      limpiarPantalla
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
          let ventas = D.ventas est
              total  = totalVentas ventas
          putStrLn $ ">>> Total de ventas: $" ++ printf "%.2f" total
          pause
          loop est

        "2" -> do
          let ventas = D.ventas est
          if null ventas
            then putStrLn ">>> No hay ventas cargadas."
            else do
              let mensuales = totalesMensuales ventas
                  anuales   = totalesAnuales   ventas
              mostrarTotalesMensuales mensuales
              mostrarTotalesAnuales   anuales
          pause
          loop est

        "3" -> do
          let ventas = D.ventas est
          if null ventas
            then putStrLn ">>> No hay ventas cargadas."
            else do
              let proms = promedioPorCategoriaAnio ventas
              if null proms
                then putStrLn ">>> No hay datos suficientes para calcular promedios."
                else mostrarPromedioPorCategoriaAnio proms
          pause
          loop est

        "4" -> return est

        _   -> do
          putStrLn "Opción no válida."
          pause
          loop est
