{-# LANGUAGE ForeignFunctionInterface #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  $Header$
-- Copyright   :  (c) Kai Zhang
-- License     :  BSD3

-- Maintainer  :  kai@kzhang.org
-- Stability   :  experimental
-- Portability :  portable

-- <module description starting at first column>
--------------------------------------------------------------------------------

module Algorithms.GLasso
    ( glasso
    , glasso'
    ) where

import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector.Storable as V

foreign import ccall "hugeglasso"
    c_glasso :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> CDouble -> IO ()

glasso :: [Double]    -- ^ row-major correlation matrix
       -> Int         -- ^ dimension of the matrix
       -> Double      -- ^ LASSO parameter
       -> ([Double], [Double])     -- ^ estimated covariance matrix and its inverse
glasso s d lambda = unsafePerformIO $ withArray (map realToFrac s) $ \sp -> do
        wp <- mallocArray (d*d)
        copyArray wp sp (d*d)
        tp <- mallocArray (d*d)
        copyArray tp sp (d*d)
        c_glasso sp wp tp (fromIntegral d) (realToFrac lambda)
        w <- peekArray (d*d) wp
        t <- peekArray (d*d) tp
        return (map realToFrac w, map realToFrac t)

glasso' :: V.Vector Double
        -> Int
        -> Double
        -> (V.Vector Double, V.Vector Double)
glasso' s d lambda = unsafePerformIO $ V.unsafeWith (V.map realToFrac s) $ \sp -> do
        wp <- mallocArray (d*d)
        tp <- mallocArray (d*d)
        c_glasso sp wp tp (fromIntegral d) (realToFrac lambda)
        wp' <- newForeignPtr_ wp
        tp' <- newForeignPtr_ tp
        return ( V.map realToFrac $ V.unsafeFromForeignPtr0 wp' (d*d)
               , V.map realToFrac $ V.unsafeFromForeignPtr0 tp' (d*d)
               )
