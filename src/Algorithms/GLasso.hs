{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
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

import qualified Data.Matrix.Storable as M
import qualified Data.Vector.Storable as V
import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall "hugeglasso"
    c_glasso :: Ptr CDouble  -- ^ input covariance matrix
             -> Ptr CDouble  -- ^ estimated covariance matrix will be stored here
             -> Ptr CDouble  -- ^ estimated inverse covariance matrix
             -> CInt         -- ^ dimension of matrix
             -> CDouble      -- ^ lambda, penalty term
             -> IO ()

glasso :: M.Matrix Double
       -> Double
       -> (M.Matrix Double, M.Matrix Double)
glasso s lambda = unsafePerformIO $
    V.unsafeWith (V.map realToFrac . M.flatten $ s) $ \sp -> do
        wp <- mallocArray (d*d)
        copyArray wp sp (d*d)
        tp <- ident d
        c_glasso sp wp tp (fromIntegral d) (realToFrac lambda)
        wp' <- newForeignPtr_ wp
        tp' <- newForeignPtr_ tp
        let cov = M.fromVector (d,d) . V.map realToFrac $ V.unsafeFromForeignPtr0 wp' (d*d)
            icov = M.fromVector (d,d) . V.map realToFrac $ V.unsafeFromForeignPtr0 tp' (d*d)
        return (cov, icov)
  where
    d = M.rows s

glasso' :: [Double]    -- ^ row-major correlation matrix
        -> Int         -- ^ dimension of the matrix
        -> Double      -- ^ LASSO parameter
        -> ([Double], [Double])     -- ^ estimated covariance matrix and its inverse
glasso' s d lambda = unsafePerformIO $ withArray (map realToFrac s) $ \sp -> do
        wp <- mallocArray (d*d)
        copyArray wp sp (d*d)
        tp <- ident d
        c_glasso sp wp tp (fromIntegral d) (realToFrac lambda)
        w <- peekArray (d*d) wp
        t <- peekArray (d*d) tp
        return (map realToFrac w, map realToFrac t)

-- | create an identity matrix
ident :: Int -> IO (Ptr CDouble)
ident d = do
    ptr <- mallocArray d2
    go ptr 0
  where
    go p !i | i >= d2 = return p
            | i `div` d == i `mod` d = pokeElemOff p i 1.0 >> go p (i+1)
            | otherwise = pokeElemOff p i 0.0 >> go p (i+1)
    d2 = d*d
{-# INLINE ident #-}
