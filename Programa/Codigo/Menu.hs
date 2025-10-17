-- Codigo/Menu.hs
module Menu (menuPrincipal) where

import System.IO (hFlush, stdout)
import qualified Importacion as Imp
import qualified ProcesamientoDeDatos as Procesamiento
import qualified AnalisisDatos as AD
import qualified Estadisticas as Estadisticas


import qualified Datos as D 
import System.Process (callCommand)   
----se llama para limpiar consola.
limpiarPantalla :: IO ()
limpiarPantalla = callCommand "clear"


estadoInicial :: D.EstadoApp
estadoInicial = D.EstadoApp { D.ventas = [], D.errores = [] }

-- | Menú principal del sistema
menuPrincipal :: IO ()
menuPrincipal = ciclo estadoInicial
  where
    ciclo :: D.EstadoApp -> IO ()
    ciclo estado = do
      limpiarPantalla
      -- 🔹 Mostrar resumen actual
      putStrLn $ "Datos cargados: " ++ show (length (D.ventas estado)) ++
                " | Registros con error: " ++ show (length (D.errores estado))
      putStrLn ""
      -- 🔹 Menú principal
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
        "1" -> Imp.menuImportacion estado >>= ciclo
        "2" -> Procesamiento.menuProcesamiento estado >>= ciclo
        "3" -> AD.menuAnalisisDatos estado >>= ciclo
        "4" -> Estadisticas.menuEstadisticas estado >>= ciclo
        "5" -> mensajePendiente "Búsqueda específica"    >> ciclo estado
        "6" -> mensajePendiente "Estadísticas"           >> ciclo estado
        "7" -> putStrLn "Saliendo del sistema... ¡Gracias por usar el programa!"
        _   -> putStrLn "Opción no válida. Intente nuevamente." >> ciclo estado

     
-- | Mensaje temporal para opciones aún no implementadas
mensajePendiente :: String -> IO ()
mensajePendiente nombre = do
  putStrLn (">>> Opción '" ++ nombre ++ "' aún no implementada.")
  putStrLn "Presione ENTER para volver al menú..."
  _ <- getLine
  putStrLn ""
