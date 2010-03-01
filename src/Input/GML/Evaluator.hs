module Input.GML.Evaluator where
import Input.GML.AST
import Data.Map as M

data EValue     = --Base values
                  EBool Bool
                | EInt Int
                | EReal Double
                | EString String
                --Extended values
                | EClosure Env Code
                | EArray [EValue]
                deriving (Show,Eq)

type Id = String

type Env        = M.Map Id EValue 

type Stack      = [EValue]

type Code       = [TokenGroup]

type MachineState = (Env,Stack,Code)

(?)::(Show a,Ord a) => M.Map a b -> a -> b
m ? idf = case M.lookup idf m of
            Nothing -> error ("Undefined identifier: "++show idf)
            Just a  -> a

thd3::(a,b,c)->c
thd3 (_,_,c) = c

snd3::(a,b,c)->b
snd3 (_,b,_) = b

fst3::(a,b,c)->a
fst3 (a,_,_) = a

evalGML::MachineState -> MachineState
evalGML = until (Prelude.null .thd3) evalStep
        where 

evalStep::MachineState -> MachineState
--Rule 1, constants
evalStep (env, st, TokenS (TokenString str):c')     = (env, EString str:st, c')
evalStep (env, st, TokenS (Number (IntVal i)):c')   = (env, EInt i:st     , c')
evalStep (env, st, TokenS (Number (DoubleVal d)):c')= (env, EReal d:st    , c')
evalStep (env, st, TokenS (Boolean bool):c')        = (env, EBool bool:st , c')
--Rule 2, binding
evalStep (env, (v:st'), TokenS (Binder idf):c') = (M.insert idf v env, st', c')
--Rule 3, reference
evalStep (env, st, TokenS (Identifier idf):c') = (env, (env?idf):st, c')
--Rule 4, create closure
evalStep (env, st, TokenFunction (TokenList cls):c') = (env, EClosure env cls:st, c')
--Rule 5, run closure
evalStep (env, EClosure cenv cc:st, TokenS (Operator "apply"):c') = (env,(snd3.evalGML) (cenv,st,cc), c')
--Rule 6, evaluate array
evalStep (env, st, TokenArray (TokenList arc):c')  = (env, (EArray .snd3.evalGML) (env,[],arc):st, c')
--Rule 7/8, if statement
evalStep (env, EBool True: EClosure cenv1 cc1: EClosure _ _: st, TokenS (Operator "if"):c') =
        (env, (snd3.evalGML) (cenv1,st,cc1), c')
evalStep (env, EBool False: EClosure _ _: EClosure cenv2 cc2: st, TokenS (Operator "if"):c') =
        (env, (snd3.evalGML) (cenv2,st,cc2), c')
evalStep a = error ("Evaluation error: " ++ show a)
