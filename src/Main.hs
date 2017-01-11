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
quantifier = choice [s, p, q, atleast, range] where
    s = "*" *> (pure $ QAtLeast 0)
    p = "+" *> (pure $ QAtLeast 1)
    q = "?" *> (pure $ QExact 0 1)
    atleast = QAtLeast <$> ("{" *> decimal <* ",}")
    range = QExact <$> ("{" *> decimal <* ",") <*> (decimal <* "}")

-- try to parse with quantifier, if fails use default {1,1}
withQuantifier r = QRe <$> r <*> choice [quantifier, (pure $ QExact 1 1)]

exactC = notThese ")"

re1 p = sepBy (many1 c) $ "|" where
    c = choice $ map withQuantifier [escaped, classC, captureGrp, anyC, notThese p]
re = re1 "|"

classHWCondition s c inv = inverted inv where
    inverted True = not' $ or' $ map cond c
    inverted _ = or' $ map cond c
    cond (ExactCE c) = eq s $ charL c
    cond (RangeCE charl charh) = and' [condh, condl] where
        condh = BinaryOp (Cmp LessOrEqual) s $ charL charh
        condl = BinaryOp (Cmp GreaterOrEqual) s $ charL charl

type ReHW = ReaderT Signal HW

or' = MultyOp Or
and' = MultyOp And
charL c = Lit (ord c) 8
eq = BinaryOp (Cmp Equal)
not' = UnaryOp Not
neq s r = not' $ eq s r

toHW (QRe (Exact c) q) = singleCharMatch q $ eq $ charL c
toHW (QRe Any q) = singleCharMatch q $ \_ -> Lit 1 1 -- this case can be optimized
toHW (QRe (Class i e) q) = singleCharMatch q $ \s -> classHWCondition s e i

singleCharMatch :: Quantifier -> (Signal -> Signal) -> Signal -> ReHW Signal
singleCharMatch (QExact 1 1) match prev = do
    s <- ask
    let cond = and' [prev, match s]
    lift $ mkReg [(cond, Lit 1 1), (not' cond, Lit 0 1)]

singleCharMatch (QAtLeast 0) match prev = do
    r <- mfix $ \r -> do
        s <- ask
        let cond = and' [or' [prev, r], match s]
        lift $ mkReg [(cond, Lit 1 1), (not' cond, Lit 0 1)]
    return $ or' [prev, r]

singleCharMatch (QAtLeast n) match prev = do 
    cnt <- mfix $ \cnt -> do
        s <- ask
        let
            go = and' [eq cnt 0, prev, match s]
            continue = and' [neq cnt 0, match s]
            stop = and' [not' $ match s]
        lift $ mkReg [(go, 1), (continue, MultyOp Sum [cnt, 1]), (stop, 0)]
    lift $ sig $ BinaryOp (Cmp GreaterOrEqual) cnt $ fromIntegral n

-- loop NFA
loop :: (Signal -> ReHW Signal) -> Signal -> ReHW Signal
loop r p = mfix $ \feedback -> r $ or' [feedback, p]

-- chain sequential NFAs
chain :: [(Signal -> ReHW Signal)] -> Signal -> ReHW (Signal, [Signal]) 
chain [] p = pure (p, [])
chain [r] p = do
    m <- r p
    return (m, [m])
chain (r:rs) p = do
    m <- r p
    (m', ms) <- chain rs m
    return (m', m:ms)

--QAtLeast Int | QExact Int Int
applyQuantifier :: Quantifier -> (Signal -> ReHW Signal) -> Signal -> ReHW Signal
applyQuantifier (QExact 1 1) r = r
applyQuantifier (QAtLeast 1) r = loop r 


verilog p = toVerilogHW $ runReaderT p (Alias "in" 8)

main :: IO ()
main = do
    print $ parseOnly re "aa(re.)[^abc-9]|t"

    putStrLn $ verilog $ do
        (toHW (QRe (Class True [RangeCE 'a' 'z']) (QAtLeast 7))) (Alias "prev" 1)
    -- print $ parseOnly captureGrp "(re)"
