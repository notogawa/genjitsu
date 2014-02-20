{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Prelude.Genjitsu
    ( module Prelude
    , MonadIOAllowPutChar(..),     putChar
    , MonadIOAllowPutStr(..),      putStr
    , MonadIOAllowPutStrLn(..),    putStrLn
    , MonadIOAllowPrint(..),       print

    , MonadIOAllowGetChar(..),     getChar
    , MonadIOAllowGetLine(..),     getLine
    , MonadIOAllowGetContents(..), getContents
    , MonadIOAllowInteract(..),    interact

    , MonadIOAllowReadFile(..),    readFile
    , MonadIOAllowWriteFile(..),   writeFile
    , MonadIOAllowAppendFile(..),  appendFile
    , MonadIOAllowReadIO(..),      readIO
    , MonadIOAllowReadLn(..),      readLn

    , MonadIOAllowIoError(..),     ioError
    ) where

import Prelude hiding
    ( putChar, putStr, putStrLn, print
    , getChar, getLine, getContents, interact
    , readFile, writeFile, appendFile, readIO, readLn
    , ioError
    )
import qualified Prelude as P
import Control.Genjitsu.TH ( genjitsu )

$(genjitsu 'P.putChar)
$(genjitsu 'P.putStr)
$(genjitsu 'P.putStrLn)
$(genjitsu 'P.print)


$(genjitsu 'P.getChar)
$(genjitsu 'P.getLine)
$(genjitsu 'P.getContents)
$(genjitsu 'P.interact)

$(genjitsu 'P.readFile)
$(genjitsu 'P.writeFile)
$(genjitsu 'P.appendFile)
$(genjitsu 'P.readIO)
$(genjitsu 'P.readLn)

$(genjitsu 'P.ioError)
