module Lambda where

import Expr
import Data.List

-- TODO 1.1. find free variables of a Expr
free_vars :: Expr -> [String]
free_vars (Variable x) = [x]                                                                    --o variablia simpla este o variabila libera
free_vars (Function x expr) = sort (nub (free_vars expr)) \\ [x]                                --variabilele libere din corpul functiei la care nu coincide numele
free_vars (Application expr1 expr2) = sort (nub (free_vars expr1 ++ free_vars expr2))           --variabilele libere din ambele expresii aplicate

-- Definim o functie generateFreshVar care genereaza un nou nume care nu e prezent in lista pentru o variabila
generateFreshVar :: [String] -> String
generateFreshVar existingVars = head (filter (`notElem` existingVars) freshVars)                --filtram variabilele care sunt deja in lista si luam primul element generat
    where
        freshVars = [c : show i | i <- [1..], c <- ['a'..'z']]                                  --functia genereaza o noua lista de variabile de la 'a' la 'z' cu sufixe numerice

-- Definim o functie replaceVar care inlocuieste aparitiile variabilei 'var' cu noua variabila 'newVar'
replaceVar :: Expr -> String -> String -> Expr
replaceVar expr var newVar = case expr of
    Variable v -> if v == var
                  then Variable newVar
                  else Variable v
    Function y e -> Function y (replaceVar e var newVar)
    Application e1 e2 -> Application (replaceVar e1 var newVar) (replaceVar e2 var newVar)

-- TODO 1.2. reduce a redex
reduce :: Expr -> String -> Expr -> Expr
reduce (Variable var) x e2 = if var == x
                             then e2
                             else Variable var
reduce (Function y expr) x e2
    | y == x = Function y expr                                                                  --daca numele functiei este acelasi cu variabila care trebuie inlocuita o lasam asa
    | y `elem` free_vars e2 = let freshVar = generateFreshVar (free_vars expr ++ free_vars e2)  --daca numele functiei apare in lista de variabile libere din e2, generam o noua variabila
                              in Function freshVar (reduce (replaceVar expr y freshVar) x e2)   --si inlocuim aparitiile numelui functiei originale in expresia finala cu variabila noua
    | otherwise = Function y (reduce expr x e2)                                                 --altfel aplicam reducerea recursiv pe corpul functiei
reduce (Application expr1 expr2) x e2 = Application (reduce expr1 x e2) (reduce expr2 x e2)     --aplicam recursiv pe ambele expresii

-- Normal Evaluation
-- Definim o functie isReducible care determina daca o expresie poate fi sau nu redusa
isReducible :: Expr -> Bool
isReducible (Application (Function _ _) _) = True                                               --daca avem o aplicatie
isReducible (Application func arg) = isReducible func || isReducible arg                        --daca functia | argumentul e reductibil
isReducible (Function _ body) = isReducible body                                                --daca body-ul e reductibil
isReducible _ = False                                                                           --pentru variabile nu e reductibil

-- TODO 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN (Application (Function var body) arg) = reduce body var arg                               --facem o reducere substituind argumentul in body-ul functiei
stepN (Application func arg) = if isReducible func
                               then Application (stepN func) arg                                --daca functia e reductibila apelam stepN pe functie
                               else Application func (stepN arg)                                --daca argumentul e reductibil apelam stepN pe argument
stepN (Function var body) = Function var (stepN body)
stepN expr = expr

-- TODO 1.4. perform Normal Evaluation
reduceN :: Expr -> Expr
reduceN expr = if not (isReducible expr)
               then expr
               else reduceN (stepN expr)

reduceAllN :: Expr -> [Expr]
reduceAllN expr = if not (isReducible expr)
                  then [expr]
                  else expr : reduceAllN (stepN expr)

-- Applicative Evaluation
-- TODO 1.5. perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA (Application (Function var body) arg) = if not (isReducible arg)                          --daca e o aplicatie de o functie cu un argument in forma normala
                                              then reduce body var arg                          --substituieste argumentul in corpul functiei
                                              else Application (Function var body) (stepA arg)  --altfel avanseaza recursiv in argument
stepA (Application func arg) = if isReducible func                                              --daca functia din o aplicatie este reductibila
                               then Application (stepA func) arg                                --avanseaza recursiv in corpul ei
                               else Application func (stepA arg)                                --altfel avanseaza recursiv in argumentul functiei
stepA (Function var body) = Function var (stepA body)                                           --apelam recursiv stepA pe corpul expresiei
stepA expr = expr

-- TODO 1.6. perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA expr = if not (isReducible expr)
               then expr
               else reduceA (stepA expr)

reduceAllA :: Expr -> [Expr]
reduceAllA expr = if not (isReducible expr)
                  then [expr]
                  else expr : reduceAllA (stepA expr)

-- Am definit o functie auxiliara care detecteaza daca avem macrouri care se extind la alte macrouri
evalMacrosAux :: [(String, Expr)] -> Expr -> [String] -> Expr
evalMacrosAux dict (Variable x) _ = Variable x
evalMacrosAux dict (Function var body) visited = Function var (evalMacrosAux dict body visited)
evalMacrosAux dict (Application expr1 expr2) visited = Application (evalMacrosAux dict expr1 visited) (evalMacrosAux dict expr2 visited)
evalMacrosAux dict (Macro name) visited =
    case lookup name dict of
        Just expr -> if name `elem` visited                                                     --verificam daca macroul a mai fost vizitat pentru a detecta ciclurile
                     then Macro name                                                            --returnam macroul in sine
                     else evalMacrosAux dict expr (name : visited)                              --continuam expansiunea macroului
        Nothing -> Macro name                                                                   --daca nu gasim macroul in dictionar - returnam macroul

-- TODO 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros dict expr = evalMacrosAux dict expr []

-- TODO 4.1. evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode strategy codeLines = map strategy (evalCodeWithContext [] codeLines)                   --aplica functia pe fiecare linie de cod
  where
    evalCodeWithContext :: [(String, Expr)] -> [Code] -> [Expr]
    evalCodeWithContext _ [] = []                                                               --daca nu mai sunt linii
    evalCodeWithContext context (line:lines) = case line of
        Assign name expr -> evalCodeWithContext ((name, expr):context) lines                    --daca e Assign adaugam macrourile la context
        Evaluate expr -> (evalMacros context expr) : evalCodeWithContext context lines          --daca e Evaluate substituim macrourile si evaluam expresia cu strategia data
