{-# LANGUAGE OverloadedStrings #-}

module Tsuru (haqify) where

import Control.Applicative
import Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString.Lazy as BL

quotePacket = do
  _ <- manyTill AC.anyChar (string "B6034")
  packet <- AL.takeWhile (/= 255)
  return packet

firstPacket contents = case parse quotePacket contents of
  Done _ m -> m
  _ -> "woops!"

haqify file = BL.readFile file >>= return . firstPacket
