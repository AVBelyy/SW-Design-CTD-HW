{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow           (second)
import           Control.Lens            (use, (%=), (.=), _1, _2)
import           Control.Monad.Except    (throwError)
import           Control.Monad.State     (StateT (..), evalStateT, get)
import           Data.Char               (isDigit, isSpace)
import           Data.List               (foldl', intercalate, uncons)
import           Data.Maybe              (maybe)
import           System.Console.Readline (addHistory, readline)

data BinOp = Add
           | Subtract
           | Multiply
           | Divide
           | Power
             deriving Show

data InToken = InOp BinOp
             | InVal Double
             | LParen
             | RParen
               deriving Show

data OutToken = OutOp BinOp
              | OutVal Double

data StackElem = StOp BinOp
               | Paren
                 deriving Show

type RPNStack = [OutToken]
type Env = ([OutToken], [StackElem])
type RPNComp = StateT Env (Either String)

instance Show OutToken where
    show (OutOp x)  = snd $ opInfo x
    show (OutVal v) = show v

opInfo = \case
    Power    -> (4, "^")
    Multiply -> (3, "*")
    Divide   -> (3, "/")
    Add      -> (2, "+")
    Subtract -> (2, "-")

prec = fst . opInfo
leftAssoc Power = False
leftAssoc _     = True

--Stateful actions
processToken :: InToken -> RPNComp ()
processToken = \case
    InVal z -> pushVal z
    InOp op -> pushOp op
    LParen  -> pushParen
    RParen  -> pushTillParen

pushTillParen :: RPNComp ()
pushTillParen = use _2 >>= \case
    []     -> throwError "Unmatched right parenthesis"
    (s:st) -> case s of
         StOp o -> _1 %= (OutOp o:) >> _2 %= tail >> pushTillParen
         Paren  -> _2 %= tail

pushOp :: BinOp -> RPNComp ()
pushOp o = use _2 >>= \case
    [] -> _2 .= [StOp o]
    (s:st) -> case s of
        (StOp o2) -> if leftAssoc o && prec o == prec o2
                     || prec o < prec o2
                     then _1 %= (OutOp o2:) >> _2 %= tail >> pushOp o
                     else _2 %= (StOp o:)
        Paren     -> _2 %= (StOp o:)

pushVal :: Double -> RPNComp ()
pushVal n = _1 %= (OutVal n:)

pushParen :: RPNComp ()
pushParen = _2 %= (Paren:)

-- Lexer
lexer :: String -> [String]
lexer "" = []
lexer s@(c:cs)
    | isDigit c = uncurry (:) $ second lexer $ span isDigit s
    | c `elem` ['+','-','*','/','^', '(',')'] = [c] : lexer cs
    | otherwise = lexer cs

-- Parse
readTokens :: String -> Either String [InToken]
readTokens = mapM f . lexer
    where f = let g = return . InOp in \case {
            "^" -> g Power; "*" -> g Multiply; "/" -> g Divide;
            "+" -> g Add;   "-" -> g Subtract; "(" -> return LParen;
            ")" -> return RParen;
            a   -> case reads a of
                []        -> throwError $ "Invalid token `" ++ a ++ "`"
                [(_,[x])] -> throwError $ "Invalid token `" ++ a ++ "`"
                [(v,[])]  -> return $ InVal v }

-- Convert to RPN
toRPN :: [InToken] -> Either String RPNStack
toRPN xs = evalStateT process ([],[])
    where
        process = do
            mapM_ processToken xs
            (outToks, stElems) <- get
            elems <- mapM toOut stElems
            return $ reverse outToks ++ elems
        toOut (StOp o) = return $ OutOp o
        toOut Paren    = throwError "Unmatched left parenthesis"

showRPN :: RPNStack -> String
showRPN = intercalate ", " . map show

evalRPN :: RPNStack -> Maybe Double
evalRPN = fmap fst . uncons . foldl' calc [] where
    calc xs (OutVal n) = n:xs
    calc (y:x:rest) (OutOp op) = f x y:rest where
        f = case op of
              Add      -> (+)
              Subtract -> (-)
              Multiply -> (*)
              Divide   -> (/)
              Power    -> (**)
    calc _ _ = [0.0 / 0.0]

main :: IO ()
main = do
    a <- readline "Enter expression: "
    case a of
      Nothing     -> return ()
      Just "exit" -> return ()
      Just expr -> do
        addHistory expr
        case readTokens expr >>= toRPN of
          Left  err -> do
              putStrLn $ "Error: " ++ show err
              main
          Right rpn -> do
            -- Show RPN
            putStrLn $ "RPN: " ++ showRPN rpn
            -- Show result
            maybe (return ()) print (evalRPN rpn)
            -- Loop
            main
