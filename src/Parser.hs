module Parser where

import           Common
import           Control.Monad
import           Data.Char

newtype Parser a = Parser (String -> Maybe (a,String))

parse :: Parser a -> String -> Maybe (a,String)
parse (Parser p) inp = p inp

instance Monad Parser where
    return v = Parser (\inp -> Just (v,inp))
    p >>= f = Parser (\inp -> do (v,s) <- parse p inp
                                 parse (f v) s)

instance MonadPlus Parser where
    mzero = Parser (\_ -> Nothing)
    mplus p q = Parser (\inp -> case parse p inp of
                                    Just result -> Just result
                                    Nothing -> parse q inp)

listplus :: [Parser a] -> Parser a
listplus lst = foldr mplus mzero lst

failure :: Parser a
failure = mzero

item :: Parser Char
item = Parser (\inp -> case inp of
                           (c:cs) -> return (c,cs)
                           [] -> Nothing)

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (c:cs) = do char c
                   string cs
                   return (c:cs)

star :: Parser a -> Parser [a]
star p = mplus (plus p) (return [])

plus :: Parser a -> Parser [a]
plus p = do x <- p
            xs <- star p
            return (x:xs)

nat :: Parser Integer
nat = do xs <- plus (sat isDigit)
         return (read xs)

int :: Parser Integer
int = mplus nat (do char '-'
                    x <- nat
                    return (-x))

float :: Parser Float
float = do x <- int
           char '.'
           y <- nat
           return (read ((show x)++"."++(show y)))

whitespace :: Parser ()
whitespace = do star (sat isSpace)
                return ()

unitval :: Parser Val
unitval = do string "nil"
             return UnitVal

boolval :: Parser Val
boolval = do s <- mplus (string "false") (string "true")
             case s of
                 "false" -> return (BoolVal False)
                 "true" -> return (BoolVal True)

intval :: Parser Val
intval = do x <- int
            return (IntVal x)

floatval :: Parser Val
floatval = do x <- float
              return (FloatVal x)

charval :: Parser Val
charval = do char '\''
             c <- item
             char '\''
             return (CharVal c)

consname :: Parser String
consname = do c <- sat isUpper
              cs <- star (mplus (sat isAlphaNum) (char '_'))
              return (c:cs)

adtval :: Parser Val
adtval = do char '('
            whitespace
            cons <- consname
            vals <- star (do whitespace
                             e <- val
                             return e)
            whitespace
            char ')'
            return (ADTVal cons vals)

strval :: Parser Val
strval = do char '"'
            cs <- star (sat (/= '"'))
            char '"'
            let f [] = UnitVal
                f (c:s) = (ADTVal "Cons" [CharVal c,f s])
                in return (f cs)

val :: Parser Val
val = listplus [unitval,boolval,floatval,intval,charval,adtval,strval]

constexp :: Parser Exp
constexp = do v <- val
              return (ConstExp v)

varname :: Parser String
varname = do c <- sat isLower
             cs <- star (mplus (sat isAlphaNum) (char '_'))
             return (c:cs)

varexp :: Parser Exp
varexp = do s <- varname
            return (VarExp s)

lambdaexp :: Parser Exp
lambdaexp = do char '('
               whitespace
               string "lambda"
               whitespace
               p <- pat
               whitespace
               e <- expr
               whitespace
               char ')'
               return (LambdaExp p e)

definedec :: Parser Dec
definedec = do char '('
               whitespace
               string "define"
               whitespace
               p <- pat
               whitespace
               e <- expr
               whitespace
               char ')'
               return (DefineDec p e)

importdec :: Parser Dec
importdec = do char '('
               whitespace
               string "import"
               whitespace
               s <- star (mplus (sat isAlphaNum) (char '.'))
               whitespace
               char ')'
               let f '.' = '/'
                   f c = c
                   in return (ImportDec ((map f s)++".er"))

patexp :: Parser (Pat,Exp)
patexp = do char '('
            whitespace
            p <- pat
            whitespace
            e <- expr
            whitespace
            char ')'
            return (p,e)

patexps :: Parser [(Pat,Exp)]
patexps = do char '('
             pes <- star (do whitespace
                             pe <- patexp
                             return pe)
             whitespace
             char ')'
             return pes

letrecexp :: Parser Exp
letrecexp = do char '('
               whitespace
               string "letrec"
               whitespace
               pes <- patexps
               whitespace
               e <- expr
               whitespace
               char ')'
               return (LetrecExp pes e)

ifexp :: Parser Exp
ifexp = do char '('
           whitespace
           string "if"
           whitespace
           cond_exp <- expr
           whitespace
           then_exp <- expr
           whitespace
           else_exp <- expr
           whitespace
           char ')'
           return (IfExp cond_exp then_exp else_exp)

caseexp :: Parser Exp
caseexp = do char '('
             whitespace
             string "case"
             whitespace
             case_exp <- expr
             whitespace
             pes <- patexps
             whitespace
             char ')'
             return (CaseExp case_exp pes)

appexp :: Parser Exp
appexp = do char '('
            whitespace
            f_exp <- expr
            whitespace
            x_exp <- expr
            whitespace
            char ')'
            return (AppExp f_exp x_exp)

adtexp :: Parser Exp
adtexp = do char '('
            whitespace
            cons <- consname
            exps <- star (do whitespace
                             e <- expr
                             return e)
            whitespace
            char ')'
            return (ADTExp cons exps)

unaryopexp :: Parser Exp
unaryopexp = do char '('
                whitespace
                op <- listplus (map string ["!","print","eval"])
                whitespace
                e <- expr
                whitespace
                char ')'
                return (UnaryOpExp op e)

binaryopexp :: Parser Exp
binaryopexp = do char '('
                 whitespace
                 op <- listplus (map string ["&&","||","==","!=","<=","<",">=",">","+","-","*"])
                 whitespace
                 x <- expr
                 whitespace
                 y <- expr
                 whitespace
                 char ')'
                 return (BinaryOpExp op x y)

listexp :: Parser Exp
listexp = do char '['
             whitespace
             es <- star (do whitespace
                            e <- expr
                            return e)
             whitespace
             char ']'
             let f [] = (ConstExp UnitVal)
                 f (e:es) = (ADTExp "Cons" [e,(f es)])
                 in return (f es)

nilpat :: Parser Pat
nilpat = do char '_'
            return NilPat

constpat :: Parser Pat
constpat = do v <- val
              return (ConstPat v)

varpat :: Parser Pat
varpat = do s <- varname
            return (VarPat s)

adtpat :: Parser Pat
adtpat = do char '('
            whitespace
            cons <- consname
            pats <- star (do whitespace
                             p <- pat
                             return p)
            whitespace
            char ')'
            return (ADTPat cons pats)

pat :: Parser Pat
pat = listplus [nilpat,constpat,varpat,adtpat]

expr :: Parser Exp
expr = listplus [constexp,varexp,unaryopexp,binaryopexp,lambdaexp,letrecexp,ifexp,caseexp,appexp,appexp,adtexp,listexp]
