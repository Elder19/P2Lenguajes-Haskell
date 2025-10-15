-- Codigo/Menu.hs
module Menu (menuPrincipal) where

import System.IO (hFlush, stdout)
import qualified Importacion as Imp

-- Estado mínimo local para que compile
data EstadoApp = EstadoApp
  { estadoVentas :: [()]    
  , estadoLogs   :: [String]
  } deriving (Show)

estadoInicial :: EstadoApp
estadoInicial = EstadoApp [] []

-- | Menú principal del sistema
menuPrincipal :: IO ()
menuPrincipal = ciclo estadoInicial
  where
    ciclo estado = do
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
        "2" -> mensajePendiente "Procesamiento de datos" >> ciclo estado
        "3" -> mensajePendiente "Análisis de datos"      >> ciclo estado
        "4" -> mensajePendiente "Análisis temporal"      >> ciclo estado
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
