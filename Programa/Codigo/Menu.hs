-- |
-- Módulo: Menu
-- Descripción: Punto de entrada del programa y menú principal de navegación.

module Menu (menuPrincipal) where

import System.IO (hFlush, stdout)
import qualified Importacion as Imp
import qualified ProcesamientoDeDatos as Procesamiento
import qualified AnalisisDatos as AD
import qualified Estadisticas as Estadisticas
import qualified Datos as D
import System.Process (callCommand)
import Persistencia (cargarEstado, guardarEstado)

-- | Limpia la consola (usa 'clear').
limpiarPantalla :: IO ()
limpiarPantalla = callCommand "clear"

-- | Pausa simple esperando ENTER.
pause :: IO ()
pause = do
  putStrLn "Presione ENTER para continuar..."
  _ <- getLine
  putStrLn ""
  putStrLn ""

-- | Estado inicial vacío (sin ventas ni errores).
estadoInicial :: D.EstadoApp
estadoInicial = D.EstadoApp { D.ventas = [], D.errores = [] }

-- | Ejecuta una acción que requiere ventas cargadas.
--   Si no hay datos, muestra aviso y retorna el estado sin cambios.
conVentas :: D.EstadoApp -> IO D.EstadoApp -> IO D.EstadoApp
conVentas est accion =
  if null (D.ventas est)
    then do
      putStrLn ">>> No hay ventas cargadas. Primero importe datos (Menú principal -> opción 1)."
      pause
      return est
    else accion

-- | Bucle principal del sistema:
--   carga el estado persistido, muestra el menú y dirige a cada opción.
--   Guarda el estado tras operaciones que lo modifican.
menuPrincipal :: IO ()
menuPrincipal = do
  mSt <- cargarEstado
  let st0 = maybe estadoInicial id mSt
  ciclo st0
  where
    -- | Loop del menú: renderiza opciones y despacha a cada submódulo.
    ciclo :: D.EstadoApp -> IO ()
    ciclo estado = do
      limpiarPantalla
      putStrLn $ "Datos cargados: " ++ show (length (D.ventas estado))
      putStrLn ""
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
        -- Importación (actualiza estado y persiste)
        "1" -> do
          est' <- Imp.menuImportacion estado
          guardarEstado est'
          ciclo est'

        -- Procesamiento (requiere ventas), persiste tras cambios
        "2" -> conVentas estado (Procesamiento.menuProcesamiento estado) >>= \est' -> do
                 guardarEstado est'
                 ciclo est'

        -- Análisis (solo lectura); no persiste cambios
        "3" -> conVentas estado (AD.menuAnalisisDatos estado) >>= ciclo

        -- Placeholders de futuras funciones
        "4" -> conVentas estado (mensajePendiente "Análisis temporal" >> return estado) >>= ciclo
        "5" -> conVentas estado (mensajePendiente "Búsqueda específica" >> return estado) >>= ciclo

        -- Estadísticas (puede modificar estado), persiste
        "6" -> conVentas estado (Estadisticas.menuEstadisticas estado) >>= \est' -> do
                 guardarEstado est'
                 ciclo est'

        -- Salida: guarda y termina
        "7" -> do
          guardarEstado estado
          putStrLn "Saliendo del sistema... ¡Estado guardado en 'estado.json'. Gracias!"

        -- Entrada inválida
        _   -> do
          putStrLn "Opción no válida. Intente nuevamente."
          pause
          ciclo estado

-- | Mensaje genérico para opciones aún no implementadas.
mensajePendiente :: String -> IO ()
mensajePendiente nombre = do
  putStrLn (">>> Opción '" ++ nombre ++ "' aún no implementada.")
  pause

