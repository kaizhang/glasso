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
    ) where

import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall "hugeglasso"
    c_glasso :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> CDouble -> IO ()

glasso :: [Double]    -- ^ row-major correlation matrix
       -> Int         -- ^ dimension of the matrix
       -> Double      -- ^ LASSO parameter
       -> ([Double], [Double])     -- ^ estimated covariance matrix and its inverse
glasso s d lambda = unsafePerformIO $ withArray (map realToFrac s) $ \sp -> do
        wp <- mallocArray (d*d)
        tp <- mallocArray (d*d)
        c_glasso sp wp tp (fromIntegral d) (realToFrac lambda)
        w <- peekArray (d*d) wp
        t <- peekArray (d*d) tp
        return (map realToFrac w, map realToFrac t)
