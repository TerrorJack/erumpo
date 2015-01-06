module Interpreter where

import           Common
import qualified Data.Map    as Map
import           Data.Maybe
import           Debug.Trace
import           Parser

instance Eq Val where
    UnitVal == UnitVal = True
    (BoolVal x) == (BoolVal y) = x == y
    (IntVal x) == (IntVal y) = x == y
    (FloatVal x) == (FloatVal y) = x == y
    (IntVal x) == (FloatVal y) = (fromIntegral x) == y
    (FloatVal x) == (IntVal y) = x == (fromIntegral y)
    (CharVal x) == (CharVal y) = x == y
    (ADTVal x_adt x_val_list) == (ADTVal y_adt y_val_list) = (x_adt == y_adt) && (and [x==y|(x,y)<-zip x_val_list y_val_list])
    _ == _ = False

instance Ord Val where
    compare UnitVal UnitVal = EQ
    compare UnitVal _ = LT
    compare _ UnitVal = GT
    compare (BoolVal x) (BoolVal y) = compare x y
    compare (IntVal x) (IntVal y) = compare x y
    compare (FloatVal x) (FloatVal y) = compare x y
    compare (IntVal x) (FloatVal y) = compare (fromIntegral x) y
    compare (FloatVal x) (IntVal y) = compare x (fromIntegral y)
    compare (CharVal x) (CharVal y) = compare x y
    compare (ADTVal x_cons x_val_list) (ADTVal y_cons y_val_list) =
        if x_cons == y_cons
            then
                case x_val_list of
                    [] ->
                        case y_val_list of
                            [] -> EQ
                            _ -> error "type error"
                    (x:xs) ->
                        case y_val_list of
                            (y:ys) ->
                                case compare x y of
                                    LT -> LT
                                    GT -> GT
                                    EQ -> compare (ADTVal "" xs) (ADTVal "" ys)
                            [] -> error "type error"
            else error "type error"
    compare _ _ = error "type error"

instance Show Val where
    show UnitVal = "nil"
    show (BoolVal False) = "false"
    show (BoolVal True) = "true"
    show (IntVal x) = show x
    show (FloatVal x) = show x
    show (CharVal x) = show x
    show (ClosureVal _ _ _) = "__closure__"
    show (ADTVal cons_name val_list) = case getString (ADTVal cons_name val_list) of
                                           Just s -> "\"" ++ s ++ "\""
                                           Nothing -> case getList (ADTVal cons_name val_list) of
                                                          Just [] -> "nil"
                                                          Just (car:cdr) -> "[" ++ (show car) ++ (foldr (++) "" [" "++(show val)|val<-cdr]) ++ "]"
                                                          Nothing -> "(" ++ cons_name ++ (foldr (++) "" [" "++(show val)|val<-val_list]) ++ ")"

instance Eq Pat where
    NilPat == NilPat = True
    (ConstPat x) == (ConstPat y) = (x == y)
    (VarPat x) == (VarPat y) = (x == y)
    (ADTPat cons0 pats0) == (ADTPat cons1 pats1) = (cons0 == cons1) && (pats0 == pats1)
    _ == _ = False

instance Ord Pat where
    compare NilPat NilPat = EQ
    compare NilPat _ = LT
    compare (ConstPat _) NilPat = GT
    compare (ConstPat x) (ConstPat y) = compare x y
    compare (ConstPat _) _ = LT
    compare (VarPat _) NilPat = GT
    compare (VarPat _) (ConstPat _) = GT
    compare (VarPat x) (VarPat y) = compare x y
    compare (VarPat _) _ = LT
    compare (ADTPat cons0 pats0) (ADTPat cons1 pats1) = compare (cons0,pats0) (cons1,pats1)
    compare (ADTPat _ _) _ = GT

getBool :: Val -> Maybe Bool
getBool (BoolVal x) = return x
getBool _ = Nothing

getClosure :: Val -> Maybe Val
getClosure (ClosureVal pat exp env) = return (ClosureVal pat exp env)
getClosure _ = Nothing

getString :: Val -> Maybe String
getString UnitVal = return ""
getString (ADTVal "Cons" [CharVal car,cdr]) = do cs <- getString cdr
                                                 return (car:cs)
getString _ = Nothing

getList :: Val -> Maybe [Val]
getList UnitVal = return []
getList (ADTVal "Cons" [car,cdr]) = do cdr_list <- getList cdr
                                       return (car:cdr_list)
getList _ = Nothing

match :: Pat -> Val -> Maybe Env
match NilPat _ = Just Map.empty
match (ConstPat pval) val = if pval == val then Just (Map.empty) else Nothing
match (VarPat var) val = Just (Map.singleton var val)
match (ADTPat pname plist) (ADTVal vname vlist) =
    let merge Nothing _ = Nothing
        merge _ Nothing = Nothing
        merge (Just env0) (Just env1) = Just (Map.union env0 env1)
    in if pname == vname then (foldl merge (Just Map.empty) [match p v|(p,v)<-zip plist vlist]) else Nothing

eval :: Exp -> Env -> Maybe Val
eval (ConstExp val) _ = Just val
eval (VarExp var) env = Map.lookup var env
eval (LambdaExp pat exp) env = Just (ClosureVal pat exp env)
eval (LetrecExp pat_exp_list exp) env =
    let new_env = Map.union delta_env env
        Just delta_env = match (ADTPat "" pat_list) (ADTVal "" val_list)
        pat_list = [p|(p,_)<-pat_exp_list]
        val_list = [fromJust (eval e new_env)|(_,e)<-pat_exp_list]
    in eval exp new_env
eval (IfExp cond_exp then_exp else_exp) env =
    do
        cond_val <- eval cond_exp env
        flag <- getBool cond_val
        if flag then eval then_exp env else eval else_exp env
eval (CaseExp exp pat_exp_list) env =
    do
        val <- eval exp env
        let f [] = Nothing
            f ((p,e):p_e_list) =
               case match p val of
                   Just delta_env -> eval e (Map.union delta_env env)
                   Nothing -> f p_e_list
            in f pat_exp_list
eval (AppExp f_exp x_exp) env =
    do
        f_val <- eval f_exp env
        (ClosureVal c_pat c_exp c_env) <- getClosure f_val
        x_val <- eval x_exp env
        delta_env <- match c_pat x_val
        eval c_exp (Map.union delta_env c_env)
eval (ADTExp cons_name exp_list) env =
    let f [] = Just []
        f (e:es) =
            do
                val <- eval e env
                vals <- f es
                return (val:vals)
        in
            do
                vals <- f exp_list
                return (ADTVal cons_name vals)

eval (UnaryOpExp op x_exp) env = do x_val <- eval x_exp env
                                    case op of
                                        "!" -> do flag <- getBool x_val
                                                  return (BoolVal (not flag))
                                        "print" -> let s = case getString x_val of
                                                               Just bs -> bs
                                                               Nothing -> show x_val
                                                   in trace s (return UnitVal)
                                        "eval" -> do s <- getString x_val
                                                     (e,_) <- parse expr s
                                                     eval e env
                                        _ -> Nothing

eval (BinaryOpExp op x_exp y_exp) env =
    do
        x_val <- eval x_exp env
        y_val <- eval y_exp env
        if elem op ["&&","||"]
            then
                do
                    x_flag <- getBool x_val
                    y_flag <- getBool y_val
                    let f = (Map.fromList [("&&",(&&)),("||",(||))]) Map.! op in return (BoolVal (f x_flag y_flag))
            else
                if elem op ["==","!=","<","<=",">",">="]
                    then
                        let f = (Map.fromList [("==",(==)),("!=",(/=)),("<",(<)),("<=",(<=)),(">",(>)),(">=",(>=))]) Map.! op in return (BoolVal (f x_val y_val))
                    else
                        if elem op ["+","-","*"]
                            then
                                case x_val of
                                    (IntVal x_int) -> case y_val of
                                                          (IntVal y_int) -> return (IntVal (((Map.fromList [("+",(+)),("-",(-)),("*",(*))]) Map.! op) x_int y_int))
                                                          (FloatVal y_float) -> return (FloatVal (((Map.fromList [("+",(+)),("-",(-)),("*",(*))]) Map.! op) (fromIntegral x_int) y_float))
                                                          _ -> Nothing
                                    (FloatVal x_float) -> case y_val of
                                                              (IntVal y_int) -> return (FloatVal (((Map.fromList [("+",(+)),("-",(-)),("*",(*))]) Map.! op) x_float (fromIntegral y_int)))
                                                              (FloatVal y_float) -> return (FloatVal (((Map.fromList [("+",(+)),("-",(-)),("*",(*))]) Map.! op) x_float y_float))
                                                              _ -> Nothing
                                    _ -> Nothing
                            else if op == "/"
                                     then case y_val of
                                              (IntVal 0) -> Nothing
                                              (FloatVal 0) -> Nothing
                                              (IntVal y_int) -> case x_val of
                                                                    (IntVal x_int) -> return (FloatVal ((fromIntegral x_int)/(fromIntegral y_int)))
                                                                    (FloatVal x_float) -> return (FloatVal (x_float/(fromIntegral y_int)))
                                                                    _ -> Nothing
                                              (FloatVal y_float) -> case x_val of
                                                                        (IntVal x_int) -> return (FloatVal ((fromIntegral x_int)/y_float))
                                                                        (FloatVal x_float) -> return (FloatVal (x_float/y_float))
                                                                        _ -> Nothing
                                              _ -> Nothing
                                     else Nothing
