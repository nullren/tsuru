{-# LANGUAGE OverloadedStrings #-}

module Tsuru (haqify, QuotePacket) where

import Control.Applicative
import Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.Attoparsec.ByteString.Char8 as AC
import Data.ByteString as B hiding (take)
import qualified Data.ByteString.Lazy as BL

--  ITEM NAME                              len  Remark
--  --------------------------------------+---+---------------
--  Data Type                               2   B6
--  Information Type                        2   03
--  Market Type                             1   4
--  Issue code                             12   ISIN code
--  Issue seq.-no.                          3
--  Market Status Type                      2
--  Total bid quote volume                  7
--  Best bid price(1st)                     5
--  Best bid quantity(1st)                  7
--  Best bid price(2nd)                     5
--  Best bid quantity(2nd)                  7
--  Best bid price(3rd)                     5
--  Best bid quantity(3rd)                  7
--  Best bid price(4th)                     5
--  Best bid quantity(4th)                  7
--  Best bid price(5th)                     5
--  Best bid quantity(5th)                  7
--  Total ask quote volume                  7
--  Best ask price(1st)                     5
--  Best ask quantity(1st)                  7
--  Best ask price(2nd)                     5
--  Best ask quantity(2nd)                  7
--  Best ask price(3rd)                     5
--  Best ask quantity(3rd)                  7
--  Best ask price(4th)                     5
--  Best ask quantity(4th)                  7
--  Best ask price(5th)                     5
--  Best ask quantity(5th)                  7
--  No. of best bid valid quote(total)      5
--  No. of best bid quote(1st)              4
--  No. of best bid quote(2nd)              4
--  No. of best bid quote(3rd)              4
--  No. of best bid quote(4th)              4
--  No. of best bid quote(5th)              4
--  No. of best ask valid quote(total)      5
--  No. of best ask quote(1st)              4
--  No. of best ask quote(2nd)              4
--  No. of best ask quote(3rd)              4
--  No. of best ask quote(4th)              4
--  No. of best ask quote(5th)              4
--  *Quote accept time*                     8  HHMMSSuu
--  End of Message                          1  0xff

data QuotePacket = ErrorPacket | QuotePacket { dataType :: ByteString
                               , informationType :: ByteString
                               , marketType :: ByteString
                               , issueCode :: ByteString }
                   deriving Show

quotePacket = do
  _ <- manyTill AC.anyChar (string "B6034")
  issueCode' <- AL.take 12
  return $ QuotePacket { dataType = "B6"
                       , informationType = "03"
                       , marketType = "4"
                       , issueCode = issueCode' }

firstPacket contents = case parse quotePacket contents of
  Done _ m -> m
  _ -> ErrorPacket

allPackets contents = case parse quotePacket contents of
  Done remaining m -> m : allPackets remaining
  _ -> []

haqify file = BL.readFile file >>= return . Prelude.take 10 . allPackets
