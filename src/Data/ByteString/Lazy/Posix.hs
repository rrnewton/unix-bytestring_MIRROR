{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2010.11.10
-- |
-- Module      :  Data.ByteString.Lazy.Posix
-- Copyright   :  Copyright (c) 2010 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides lazy 'ByteString' versions of the "System.Posix.IO"
-- file-descriptor based I/O API.
----------------------------------------------------------------
module Data.ByteString.Lazy.Posix
    (
    -- * I/O with file descriptors
      fdRead
    , fdWrite
    ) where

import qualified Data.ByteString.Posix         as BSP
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import           System.Posix.Types            (Fd, ByteCount)

----------------------------------------------------------------

-- | Read data from an 'Fd' and convert it to a 'BL.ByteString'.
-- Throws an exception if this is an invalid descriptor, or EOF has
-- been reached.
fdRead
    :: Fd
    -> ByteCount                     -- ^ How many bytes to try to read.
    -> IO (BL.ByteString, ByteCount) -- ^ The bytes read, how many
                                     --   bytes were actually read.
fdRead _  0 = return (BL.empty, 0)
fdRead fd n = do
    (s,n') <- BSP.fdRead fd n
    return (BL.fromChunks [s], n')


-- | Write a 'BL.ByteString' to an 'Fd'.
fdWrite :: Fd -> BL.ByteString -> IO ByteCount
fdWrite fd =
    BLI.foldlChunks
        (\ accM s -> do
            acc <- accM
            rc  <- BSP.fdWrite fd s
            -- BUG: detect when (rc /= BL.length s) and respond somehow
            return $! acc+rc)
        (return 0)

----------------------------------------------------------------
----------------------------------------------------------- fin.