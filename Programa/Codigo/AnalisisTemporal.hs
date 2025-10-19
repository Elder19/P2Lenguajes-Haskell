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
