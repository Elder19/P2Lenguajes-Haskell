-- Menu.hs
module Menu (mainMenu) where

import System.IO (hFlush, stdout)

-- | Función principal del menú
mainMenu :: IO ()
mainMenu = do
    putStrLn "=================================================="
    putStrLn "     SISTEMA DE ANÁLISIS DE DATOS DE VENTAS       "
    putStrLn "=================================================="
    putStrLn "1 Importación de datos"
    putStrLn "2 Procesamiento de datos"
    putStrLn "3 Análisis de datos"
    putStrLn "4 Análisis temporal"
    putStrLn "5 Búsqueda específica"
    putStrLn "6 Estadísticas"
    putStrLn "7 Salir"
    putStrLn "--------------------------------------------------"
    putStr   "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine
    putStrLn ""  
    ejecutarOpcion opcion

-- | Función que ejecuta la opción seleccionada
ejecutarOpcion :: String -> IO ()
ejecutarOpcion opcion =
    case opcion of
        "1" -> do
            putStrLn ">>> Importación de datos seleccionada."
           
            volverAlMenu

        "2" -> do
            putStrLn ">>> Procesamiento de datos seleccionado."
            
            volverAlMenu

        "3" -> do
            putStrLn ">>> Análisis de datos seleccionado."
            
            volverAlMenu

        "4" -> do
            putStrLn ">>> Análisis temporal seleccionado."
          
            volverAlMenu

        "5" -> do
            putStrLn ">>> Búsqueda específica seleccionada."
            
            volverAlMenu

        "6" -> do
            putStrLn ">>> Estadísticas seleccionadas."
          
            volverAlMenu

        "7" -> putStrLn "Saliendo del sistema... ¡Gracias por usar el programa!"

        _   -> do
            putStrLn "Opción no válida. Intente de nuevo."
            volverAlMenu

-- | Función auxiliar que vuelve al menú principal
volverAlMenu :: IO ()
volverAlMenu = do
    putStrLn ""
    putStrLn "Presione ENTER para volver al menú principal..."
    _ <- getLine
    putStrLn ""
    mainMenu
