{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module SurfaceNetsR where
import           Control.Memory.Region (V)
import qualified Data.Vector.SEXP      as VS
import           Foreign.R             (SEXP, sexp)
import qualified Foreign.R             as R
import           Lib

foreign export ccall surfaceNetsR :: SEXP s 'R.Real -> SEXP s 'R.Real -> SEXP s 'R.Int -> SEXP V 'R.Vector
surfaceNetsR :: SEXP s 'R.Real -> SEXP s 'R.Real -> SEXP s 'R.Int -> SEXP V 'R.Vector
surfaceNetsR potential bounds dims =
  makeOutputList vertices' (length vertices) faces' (length faces)
  where
    (vertices, faces) = surfaceNets potential' dims'' bounds'' 0
    vertices' = VS.toSEXP $ VS.fromList (concat vertices)
    faces' = VS.toSEXP $ VS.fromList $ map fromIntegral (concat faces)
    potential' = VS.toList $ VS.fromSEXP potential
    dims' = map fromIntegral (VS.toList $ VS.fromSEXP dims)
    dims'' = (dims' !! 0, dims' !! 1, dims' !! 2)
    bounds' = VS.toList $ VS.fromSEXP bounds
    bounds'' = ((bounds' !! 0, bounds' !! 1), (bounds' !! 2, bounds' !! 3), (bounds' !! 4, bounds' !! 5))
