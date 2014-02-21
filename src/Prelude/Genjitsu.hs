{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Prelude.Genjitsu
    ( module Prelude
    , AllowPutChar(..),     putChar
    , AllowPutStr(..),      putStr
    , AllowPutStrLn(..),    putStrLn
    , AllowPrint(..),       print

    , AllowGetChar(..),     getChar
    , AllowGetLine(..),     getLine
    , AllowGetContents(..), getContents
    , AllowInteract(..),    interact

    , AllowReadFile(..),    readFile
    , AllowWriteFile(..),   writeFile
    , AllowAppendFile(..),  appendFile
    , AllowReadIO(..),      readIO
    , AllowReadLn(..),      readLn

    , AllowIoError(..),     ioError
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
