-- Codigo/Menu.hs
module Menu (menuPrincipal) where

import System.IO (hFlush, stdout)
import qualified Importacion as Imp
import qualified ProcesamientoDeDatos as Procesamiento
import qualified AnalisisDatos as AD
import qualified Estadisticas as Estadisticas
import qualified Datos as D
import System.Process (callCommand)

-- 🔹 Limpia la consola
limpiarPantalla :: IO ()
limpiarPantalla = callCommand "clear"

-- 🔹 Pausa genérica
pause :: IO ()
pause = do
  putStrLn "Presione ENTER para continuar..."
  _ <- getLine
  putStrLn ""
  putStrLn ""

-- 🔹 Estado inicial vacío
estadoInicial :: D.EstadoApp
estadoInicial = D.EstadoApp { D.ventas = [], D.errores = [] }

-- 🔹 Helper: exige ventas cargadas para continuar
conVentas :: D.EstadoApp -> IO D.EstadoApp -> IO D.EstadoApp
conVentas est accion =
  if null (D.ventas est)
    then do
      putStrLn ">>> No hay ventas cargadas. Primero importe datos (Menú principal -> opción 1)."
      pause
      return est
    else accion

-- | Menú principal del sistema
menuPrincipal :: IO ()
menuPrincipal = ciclo estadoInicial
  where
    ciclo :: D.EstadoApp -> IO ()
    ciclo estado = do
      limpiarPantalla
      -- 🔸 Resumen actual
      putStrLn $ "Datos cargados: " ++ show (length (D.ventas estado)) ++
                 " | Registros con error: " ++ show (length (D.errores estado))
      putStrLn ""
      -- 🔸 Menú principal
      putStrLn "=================================================="
      putStrLn "     SISTEMA DE ANÁLISIS DE DATOS DE VENTAS       "
      putStrLn "=================================================="
      putStrLn "1) Importación de datos"
      putStrLn "2) Procesamiento de datos"
      putStrLn "3) Análisis de datos"
      putStrLn "4) Análisis temporal"
      putStrLn "5) Búsqueda específica"
      putStrLn "6) Estadísticas"
      putStrLn "7) Salir"
      putStrLn "--------------------------------------------------"
      putStr   "Seleccione una opción: "
      hFlush stdout
      opcion <- getLine
      putStrLn ""

      case opcion of
      
        "1" -> do
          est' <- Imp.menuImportacion estado
          ciclo est'

      
        "2" -> conVentas estado (Procesamiento.menuProcesamiento estado) >>= ciclo

     
        "3" -> conVentas estado (AD.menuAnalisisDatos estado) >>= ciclo

        
        "4" -> conVentas estado (mensajePendiente "Análisis temporal" >> return estado) >>= ciclo

       
        "5" -> conVentas estado (mensajePendiente "Búsqueda específica" >> return estado) >>= ciclo

        "6" -> conVentas estado (Estadisticas.menuEstadisticas estado) >>= ciclo

     
        "7" -> putStrLn "Saliendo del sistema... ¡Gracias por usar el programa!"

      
        _   -> do
          putStrLn "Opción no válida. Intente nuevamente."
          pause
          ciclo estado

-- | Mensaje temporal para opciones aún no implementadas
mensajePendiente :: String -> IO ()
mensajePendiente nombre = do
  putStrLn (">>> Opción '" ++ nombre ++ "' aún no implementada.")
  pause
