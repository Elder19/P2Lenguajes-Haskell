{-# LANGUAGE ScopedTypeVariables #-}
module ProcesamientoDeDatos (menuProcesamiento) where

import qualified Datos as D
import System.IO (hFlush, stdout)
import Data.List (group, sort, sortOn, nubBy, (\\))
import Data.Maybe (catMaybes)


-- | Técnica de imputación disponible.
data Tecnica = Moda | Media | Mediana
  deriving (Eq, Show)

-- | Función auxiliar para pausar la consola.
pause :: IO ()
pause = do
  putStrLn "Presione ENTER para continuar..."
  _ <- getLine
  putStrLn ""
  putStrLn ""


getIdVenta :: D.Venta -> Int
getIdVenta (D.Venta idV _ _ _ _ _ _ _) = idV

getCantidad :: D.Venta -> Maybe Double
getCantidad (D.Venta _ _ _ _ _ cant _ _) = cant

setCantidad :: Maybe Double -> D.Venta -> D.Venta
setCantidad nueva (D.Venta idV f pid pn cat _ p t) =
  D.Venta idV f pid pn cat nueva p t

getPrecio :: D.Venta -> Maybe Double
getPrecio (D.Venta _ _ _ _ _ _ p _) = p

setPrecio :: Maybe Double -> D.Venta -> D.Venta
setPrecio nuevo (D.Venta idV f pid pn cat c _ t) =
  D.Venta idV f pid pn cat c nuevo t

recalcularTotal :: D.Venta -> D.Venta
recalcularTotal v =
  case (getCantidad v, getPrecio v) of
    (Just c, Just p) -> setTotal (Just (c * p)) v
    _                -> v
  where
    setTotal :: Maybe Double -> D.Venta -> D.Venta
    setTotal nt (D.Venta idV f pid pn cat c p _ ) =
      D.Venta idV f pid pn cat c p nt

--------------------------------------------------------------------------------
-- Medidas: moda / media / mediana y selector por tcnica
--------------------------------------------------------------------------------

-- Cuenta ocurrencias y devuelve la moda (desempata por valor mayor).
moda :: (Ord a) => [a] -> Maybe a
moda [] = Nothing
moda xs =
  let grupos = map (\g -> (head g, length g)) . group . sort $ xs  -- [(valor, frecuencia)]
  in Just . fst . last $ sortOn (\(v,n) -> (n, v)) grupos

-- Media 
media :: (Real a, Fractional b) => [a] -> Maybe b
media [] = Nothing
media xs = Just $ realToFrac (sum xs) / realToFrac (length xs)

-- Mediana 
mediana :: (Ord a, Fractional a) => [a] -> Maybe a
mediana [] = Nothing
mediana xs =
  let ys = sort xs
      n  = length ys
  in if odd n
        then Just (ys !! (n `div` 2))
        else let i = n `div` 2
              in Just ((ys !! (i-1) + ys !! i) / 2)

-- | Eleccion de medida segn tcnica 
tipoCambio :: Tecnica -> [Double] -> Maybe Double
tipoCambio Moda    xs = fmap realToFrac (moda xs)
tipoCambio Media   xs = media xs
tipoCambio Mediana xs = mediana xs

--------------------------------------------------------------------------------
-- Imputación genérica y especializada
--------------------------------------------------------------------------------

-- | Imputa un campo numérico (Maybe a) en una lista de ventas.
imputarCampo
  :: forall a
   . (a -> Double)                    
  -> (Double -> a)                   
  -> (D.Venta -> Maybe a)             -- obtCampo
  -> (Maybe a -> D.Venta -> D.Venta)  -- setCampo
  -> Tecnica
  -> [D.Venta]
  -> ([D.Venta], [Int])
imputarCampo aDouble desdeDouble obtCampo setCampo tecnica ventas =
  case tipoCambio tecnica valoresDeReferencia of
    Nothing    -> (ventas, [])
    Just vRefD ->
      let valorFinal = desdeDouble vRefD
          paso :: D.Venta -> ([D.Venta], [Int]) -> ([D.Venta], [Int])
          paso v (accV, accIds) =
            case obtCampo v of
              Just _  ->
                let v' = recalcularTotal v
                in (v':accV, accIds)
              Nothing ->
                let vIm = setCampo (Just valorFinal) v
                    v'  = recalcularTotal vIm
                in (v':accV, getIdVenta v : accIds)
          (ventas', ids) = foldr paso ([], []) ventas
      in (ventas', ids)
  where
  
    valoresDeReferencia :: [Double]
    valoresDeReferencia = map aDouble . catMaybes $ map obtCampo ventas

--   Devuelve (ventas nuevas, ids modificados en cantidad, ids modificados en precio).
imputarCantidadYPrecio
  :: Tecnica -> [D.Venta] -> ([D.Venta], [Int], [Int])
imputarCantidadYPrecio tecnica ventas0 =
  let (ventas1, idsCant) =
        imputarCampo
          id              
          id              
          getCantidad
          setCantidad
          tecnica
          ventas0

      (ventas2, idsPrecio) =
        imputarCampo
          id
          id
          getPrecio
          setPrecio
          tecnica
          ventas1
  in (ventas2, idsCant, idsPrecio)

--------------------------------------------------------------------------------
-- Eliminación de duplicados
--------------------------------------------------------------------------------

-- | Elimina duplicados por venta_id conservando la primera aparición.

eliminarDuplicadosPorId :: [D.Venta] -> ([D.Venta], [Int])
eliminarDuplicadosPorId xs =
  let aparecePrimero v1 v2 = getIdVenta v1 == getIdVenta v2
      sinDup         = nubBy aparecePrimero xs
      idsOriginal    = map getIdVenta xs
      idsConservados = map getIdVenta sinDup
      idsEliminados  = idsOriginal \\ idsConservados
  in (sinDup, idsEliminados)

--------------------------------------------------------------------------------
-- Menu
--------------------------------------------------------------------------------

menuProcesamiento :: D.EstadoApp -> IO D.EstadoApp
menuProcesamiento estado0 = loopPrincipal estado0
  where
    loopPrincipal :: D.EstadoApp -> IO D.EstadoApp
    loopPrincipal est = do
      putStrLn "------------------ PROCESAMIENTO DE DATOS ------------------"
      putStrLn "1) Completar datos faltantes"
      putStrLn "2) Eliminar duplicados"
      putStrLn "3) Volver"
      putStr   "Seleccione una opción: "
      hFlush stdout
      op <- getLine
      case op of
        "1" -> do
          est' <- submenuCompletar est
          loopPrincipal est'
        "2" -> do
          let (ventasLimpias, idsEliminados) = eliminarDuplicadosPorId (D.ventas est)
              est' = est { D.ventas = ventasLimpias }
          putStrLn $ "Duplicados eliminados: " ++ show (length idsEliminados)
          mapM_ (\i -> putStrLn $ "   - venta_id " ++ show i) idsEliminados
          pause
          loopPrincipal est'
        "3" -> return est
        _   -> do
          putStrLn "Opción no válida."
          pause
          loopPrincipal est

    submenuCompletar :: D.EstadoApp -> IO D.EstadoApp
    submenuCompletar est = do
      putStrLn "------------- COMPLETAR DATOS FALTANTES -------------"
      putStrLn "Elija técnica de imputación:"
      putStrLn "1) Moda"
      putStrLn "2) Media (promedio)"
      putStrLn "3) Mediana"
      putStrLn "4) Volver"
      putStr   "Seleccione una opción: "
      hFlush stdout
      t <- getLine
      case t of
        "1" -> aplicarImputacion Moda est
        "2" -> aplicarImputacion Media est
        "3" -> aplicarImputacion Mediana est
        "4" -> return est
        _   -> do
          putStrLn "Opción no válida."
          pause
          submenuCompletar est

    aplicarImputacion :: Tecnica -> D.EstadoApp -> IO D.EstadoApp
    aplicarImputacion tecnica est = do
      let ventasAct = D.ventas est

      -- Calcular valor de referencia por técnica para cantidad y precio
      let valoresCant   = catMaybes (map getCantidad ventasAct)
          valoresPrecio = catMaybes (map getPrecio ventasAct)
          refCant       = tipoCambio tecnica valoresCant
          refPrecio     = tipoCambio tecnica valoresPrecio

     
      putStrLn $ case tecnica of
        Moda    -> "Moda cantidad = Q"    ++ maybe "N/A" show refCant
        Media   -> "Media cantidad = Q"   ++ maybe "N/A" show refCant
        Mediana -> "Mediana cantidad = Q" ++ maybe "N/A" show refCant
      putStrLn $ case tecnica of
        Moda    -> "Moda precio = Q"    ++ maybe "N/A" show refPrecio
        Media   -> "Media precio = Q"   ++ maybe "N/A" show refPrecio
        Mediana -> "Mediana precio = Q" ++ maybe "N/A" show refPrecio

      
      let (vs', idsCant, idsPrecio) = imputarCantidadYPrecio tecnica ventasAct
          est' = est { D.ventas = vs' }
      case (null idsCant, null idsPrecio) of
        (True, True) -> putStrLn "No había faltantes para imputar."
        _ -> do
          putStrLn $ ">>> Técnica aplicada: " ++ show tecnica
          putStrLn $ "Registros con cantidad imputada: " ++ show (length idsCant)
          mapM_ (\i -> putStrLn $ "   - venta_id (cantidad) " ++ show i) idsCant
          putStrLn $ "Registros con precio imputado: " ++ show (length idsPrecio)
          mapM_ (\i -> putStrLn $ "   - venta_id (precio) " ++ show i) idsPrecio
      pause
      return est'
