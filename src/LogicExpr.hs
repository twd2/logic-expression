
import           Text.Parsec
import           Control.Monad.Identity
import           Data.Char (isLetter, isDigit)
import           Data.List (sort, group)
import           Data.Maybe (fromJust)
import qualified Data.Map as M
import           Prelude hiding (and, or)

data Expr 
  = TauimpExpr Expr Expr
  | ImpExpr    Expr Expr
  | OrExpr     Expr Expr
  | XorExpr    Expr Expr
  | AndExpr    Expr Expr
  | NotExpr    Expr
  | Lit        Bool
  | Variable   String
  deriving (Show)

type Parser = ParsecT String () Identity

pretty :: M.Map String Bool -> Bool -> IO ()
pretty env ret = do
  forM_ (M.toList env) $ \(s, v) -> do
    putStr $ "(" ++ s ++ "," ++ convert v ++ ") "
  putStrLn $ "-> " ++ convert ret
  where convert True = "1"
        convert False = "0"

main = forever $ do
  str <- getLine
  case runIdentity (runParserT expr () [] str) of
    Left err -> fail (show err)
    Right expr -> do
      let var = vars expr
      forM_ (envs var) $ \env -> do
        pretty env $ eval expr env       

eval :: Expr -> M.Map String Bool -> Bool
eval exp env = case exp of
  TauimpExpr _ _ -> undefined  -- twd2 is bad bad :(
  ImpExpr    a b -> not (eval a env) || (eval b env)
  OrExpr     a b -> (eval a env) || (eval b env)
  XorExpr    a b -> (eval a env) /= (eval b env)
  AndExpr    a b -> (eval a env) && (eval b env)
  NotExpr    a   -> not (eval a env)
  Lit        a   -> a
  Variable   a   -> fromJust (M.lookup a env)
    
envs :: [String] -> [M.Map String Bool]
envs var = foldM go M.empty var where
  go m v = [M.insert v True m, M.insert v False m]

vars :: Expr -> [String]
vars exp = nub $ vars' exp []
  where
    nub = map head . group . sort
    vars' exp xs = case exp of
      TauimpExpr a b -> go a b
      ImpExpr    a b -> go a b
      OrExpr     a b -> go a b
      XorExpr    a b -> go a b
      AndExpr    a b -> go a b
      NotExpr    a   -> go' a
      Lit        _   -> xs
      Variable   a   -> a:xs
      where go a b = vars' b (vars' a xs)
            go' a = vars' a xs

expr :: Parser Expr
expr = tauimp <* eof

tauimp :: Parser Expr
tauimp = imp `chainl1` tauimpOp where
  tauimpOp = string "<->" >> return TauimpExpr

imp :: Parser Expr
imp = or `chainl1` impOp where
  impOp = string "->" >> return ImpExpr

or :: Parser Expr
or = xor `chainl1` orOp where
  orOp = string "||" >> return OrExpr

xor :: Parser Expr
xor = and `chainl1` xorOp where
  xorOp = string "^^" >> return XorExpr

and :: Parser Expr
and = bitor `chainl1` andOp where
  andOp = string "&&" >> return AndExpr

bitor :: Parser Expr
bitor = bitxor `chainl1` bitorOp where
  bitorOp = string "|" >> return OrExpr

bitxor :: Parser Expr
bitxor = bitand `chainl1` bitxorOp where
  bitxorOp = string "^" >> return XorExpr

bitand :: Parser Expr
bitand = inv `chainl1` bitandOp where
  bitandOp = string "&" >> return AndExpr

inv :: Parser Expr
inv = factor <|> do
  char '!' <|> char '~'
  NotExpr <$> inv

factor :: Parser Expr
factor = lit <|> variable <|> between (char '(') (char ')') tauimp

lit :: Parser Expr
lit = Lit <$> ((char '0' *> return False) <|> (char '1' *> return True))

variable :: Parser Expr
variable =  Variable <$> do
  a <- satisfy isLetter
  b <- many $ satisfy $ \c -> isLetter c || isDigit c
  return $ (a : b)

