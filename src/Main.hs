{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (pack)
import Hardware.PipelineDSL
import Data.Char (ord)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Control.Monad.Fix

data Re = Exact Char | Class Bool [ClassEntry] | Grp [[QRe]] | Any deriving (Show)
data QRe = QRe Re Quantifier deriving (Show)
data ClassEntry = ExactCE Char | RangeCE Char Char deriving (Show)
data Quantifier = QAtLeast Int | QExact Int Int deriving (Show)

captureGrp = Grp <$> ("(" *> (re1 ")|") <* ")")
anyC = "." *> pure Any
escaped = Exact <$> ("\\" *> anyChar)

notThese p = Exact <$> (satisfy $ notFrom p) where
    notFrom :: [Char] -> Char -> Bool
    notFrom p c = all (/= c) p

-- character class examples: [ab] [0-9a-z] [^a-bc] [a\]]
classC = Class <$> choice [inverted, not_inverted] <*> content <* "]" where
    escaped = choice [notChar ']', "\\" *> anyChar]
    range = RangeCE <$> escaped <*> (char '-' <* anyChar)
    single = ExactCE <$> escaped
    inverted = "[^" *> pure True
    not_inverted = "[" *> pure False
    content = many1 $ choice [range, single]

-- quantifiers * ? {n,} {n,p}
quantifier = choice [s, q, atleast, range] where
    s = "*" *> (pure $ QAtLeast 0)
    q = "?" *> (pure $ QAtLeast 1)
    atleast = QAtLeast <$> ("{" *> decimal <* ",}")
    range = QExact <$> ("{" *> decimal <* ",") <*> (decimal <* "}")

-- try to parse with quantifier, if fails use default {1,1}
withQuantifier r = QRe <$> r <*> choice [quantifier, (pure $ QExact 1 1)]

exactC = notThese ")"

re1 p = sepBy (many1 c) $ "|" where
    c = choice $ map withQuantifier [escaped, classC, captureGrp, anyC, notThese p]
re = re1 "|"

classHWCondition s c inv = inverted inv where
    inverted True = UnaryOp Not $ MultyOp Or $ map cond c
    inverted _ = MultyOp Or $ map cond c
    cond (ExactCE char) = BinaryOp (Cmp Equal) s $ Lit (ord char) 8
    cond (RangeCE charl charh) = MultyOp And [condh, condl] where
        condh = BinaryOp (Cmp LessOrEqual) s $ Lit (ord charh) 8
        condl = BinaryOp (Cmp GreaterOrEqual) s $ Lit (ord charl) 8

type ReHW = ReaderT Signal HW

or' = MultyOp Or
and' = MultyOp And
charL c = Lit (ord c) 8
eq = BinaryOp (Cmp Equal)
not' = UnaryOp Not
neq s r = not' $ eq s r

singleCharMatch :: (Signal -> Signal) -> Quantifier -> Signal -> ReHW Signal
singleCharMatch match (QExact 1 1) prev = do
    s <- ask
    let cond = and' [prev, match s]
    lift $ mkReg [(cond, Lit 1 1), (not' cond, Lit 0 1)]

singleCharMatch match (QAtLeast 0) prev = do
    r <- mfix $ \r -> do
        s <- ask
        let cond = and' [or' [prev, r], match s]
        lift $ mkReg [(cond, Lit 1 1), (not' cond, Lit 0 1)]
    return $ or' [prev, r]

singleCharMatch match (QAtLeast n) prev = do 
    cnt <- mfix $ \cnt -> do
        s <- ask
        let
            go = and' [eq cnt 0, prev, match s]
            continue = and' [neq cnt 0, match s]
            stop = and' [not' $ match s]
        lift $ mkReg [(go, 1), (continue, MultyOp Sum [cnt, 1]), (stop, 0)]
    lift $ sig $ BinaryOp (Cmp GreaterOrEqual) cnt $ fromIntegral n

toHW (QRe (Exact c) q) = singleCharMatch (eq $ charL c) q
toHW (QRe Any q) = singleCharMatch (\_ -> Lit 1 1) q -- this case can be optimized

verilog p = toVerilogHW $ runReaderT p (Alias "in" 8)

main :: IO ()
main = do
    print $ parseOnly re "aa(re.)[^abc-9]|t"

    putStrLn $ verilog $ do
        (toHW (QRe (Exact 'b') (QAtLeast 7))) (Alias "prev" 1)
        (toHW (QRe Any (QAtLeast 7))) (Alias "prev" 1)
    -- print $ parseOnly captureGrp "(re)"
