{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (pack)
import Hardware.PipelineDSL
import Data.Char (ord)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Control.Monad.Fix
import Control.Monad (forM)

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

classHWCondition c inv s = inverted inv where
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

toHW (Exact c) = singleCharMatch $ eq $ charL c
toHW Any = singleCharMatch $ \_ -> Lit 1 1 -- this case can be optimized
toHW (Class i e) = singleCharMatch $ classHWCondition e i
toHW (Grp m) = toHW' m

toHW' :: [[QRe]] -> Signal -> ReHW Signal
toHW' rs p = do
    ms <- forM rs $ \s -> do
        let quantify (QRe r q) = applyQuantifier q (toHW r)
        (m, _) <- (chain $ map quantify s) p
        return m
    return $ or' ms

singleCharMatch :: (Signal -> Signal) -> Signal -> ReHW Signal
singleCharMatch match prev = do
    s <- ask
    let cond = and' [prev, match s]
    lift $ mkReg [(cond, Lit 1 1), (not' cond, Lit 0 1)]

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

--
applyQuantifier :: Quantifier -> (Signal -> ReHW Signal) -> Signal -> ReHW Signal
applyQuantifier (QExact 1 1) r p = r p
applyQuantifier (QAtLeast 1) r p = loop r p
applyQuantifier (QAtLeast 0) r p = do
    m <- (loop r) p
    return $ or' [m, p]
applyQuantifier (QAtLeast n) r p = do
    (m, _) <- (chain $ (replicate (n - 1) r) ++ [loop r]) p
    return m
applyQuantifier (QExact n h) r p = do
    (m, _) <- (chain $ replicate (n - 1) r) p
    (_, ms) <- (chain $ replicate (h - n) r) m
    return $ or' ms

verilog p = toVerilogHW $ runReaderT p (Alias "in" 8)

main :: IO ()
main = do
    
    let (Right r) = parseOnly re "aa(re.)[^abc-9]|t"
    let
        r1 = Class True [RangeCE 'a' 'z']
        q1 = QAtLeast 8 
    putStrLn $ verilog $ do
        toHW' r (Alias "prev" 1)

    -- print $ parseOnly captureGrp "(re)"
