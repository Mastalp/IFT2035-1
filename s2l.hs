-- aux functions

{-
s2slist :: Sexp -> [Sexp]
s2slist Snil = []
s2slist (Snum i) = [Snum i]
s2slist (Ssym v) = [Ssym v]
s2slist (Scons e1 e2) = s2slist e1 ++ [e2] -- ! changed
-}
-- ! OG METHOD
s2slist :: Sexp -> [Sexp]
s2slist Snil = [] -- base case
s2slist (Snum i) = [Snum i]
s2slist (Ssym v) = [Ssym v]
s2slist (Scons e1 e2) = s2slist e1 ++ s2slist e2


sList2Ltype :: [Sexp] -> Ltype
sList2Ltype [Ssym "Int"] = Lint
sList2Ltype [Ssym "->", Ssym "Int"] = Larw Lint Lint
sList2Ltype (x:xs) = Larw (s2t x) (sList2Ltype xs)
--sList2Ltype _ = s2t (Ssym "error")

-- G Petey's solution :
{-
sList2Ltype :: [Sexp] -> Ltype
sList2Ltype [Ssym "Int"] = Lint
sList2Ltype [Ssym "->", Ssym "Int"] = Larw Lint Lint
sList2Ltype [Ssym "->", arg, rest] = Larw (s2t arg) (sList2Ltype [rest])
sList2Ltype _ = error "Invalid type expression"
-}

{-aux2 :: [Sexp] -> Sexp
aux2 (x:xs)
  | l <= 2 = Scons Snil x
  | otherwise = Scons Snil (aux2 xs)
  where l = length (x:xs)-}


-- s2t -- DONE -- 
s2t :: Sexp -> Ltype
s2t (Ssym "Int") = Lint
-- ¡¡COMPLÉTER ICI!! 
s2t (Scons Snil int) = s2t int
--s2t se = sList2Ltype (tail (s2slist se))
s2t se
  | not (null t) && aux3 t 0 = sList2Ltype (tail t)
  where t = s2slist se
s2t se = error ("Type Psil inconnu: " ++ (showSexp se))

aux3 :: [Sexp] -> Int -> Bool
aux3 [] 0 = True
aux3 [] 1 = True
aux3 (x:xs) i
  | x == Ssym "->" = let j = i + 1 in aux3 xs j
  | x == Ssym "Int" = aux3 xs i
  | otherwise = False
aux3 _ _ = False


-- s2l -- ! wip ! -- 

{-
-- Auxiliary function to convert [Sexp] back to Sexp
sList2Sexp :: [Sexp] -> Sexp
sList2Sexp [] = Snil
sList2Sexp (x:xs) = Scons x (sList2Sexp xs)
-}

-- AUX 

s2slistNested :: Sexp -> [[Sexp]]
s2slistNested Snil = []
s2slistNested (Snum i) = [[Snum i]]
s2slistNested (Ssym v) = [[Ssym v]]
s2slistNested (Scons e1 e2) = case e1 of
                                Scons _ _ -> (s2slist e1 ++ [e2]) : s2slistNested e2
                                _         -> [[e1, e2]]



s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s
-- ¡¡COMPLÉTER ICI!! 

s2l Snil = error "NIL"

s2l (Scons e1 e2) = 
  let sexpList = s2slist (Scons e1 e2)
  in processSexpList sexpList

processSexpList :: [Sexp] -> Lexp
processSexpList [] = error "wut"
processSexpList [x] = s2l x -- base case 
processSexpList sexps =
  case sexps of
    -- 
    -- let -- CHAT GPT
    (Ssym "let":Ssym var:e:exp:[]) -> Llet var (s2l e) (s2l exp)
    (Ssym "let":Ssym var:e:rest) | not (null rest) -> Llet var (s2l e) (processSexpList rest)

    -- Philippe Gabriel 2021
    (Ssym "fun" : v : e : []) -> case s2l v of
      Lvar x -> Lfun x (s2l e)
      _ -> error "(argsMatchError se)"
    (Ssym "fun" : v : vs) -> case s2l v of
      Lvar x -> Lfun x (processSexpList ([Ssym "fun"] ++ vs))
      _ -> error "(argsMatchError se)"

  
    -- type 
    (Ssym "hastype" : Ssym var : e : t : []) -> Lhastype (Lvar var) (s2t t) -- strToLtype is a function that you need to implement to convert strings to Ltype
    -- vars 
    (Ssym s : args) -> foldl1 Lapp (map s2l (Ssym s : args))
    --(Ssym s:args) -> foldl Lapp (s2l $ Ssym s) (map s2l args)
    
    _ -> error "Expression Psil inconnue!"  



     



-- s2d -- DONE -- 
s2d :: Sexp -> Ldec
s2d (Scons (Scons (Scons Snil (Ssym "def")) (Ssym v)) e) = Ldef v (s2l e)
-- ¡¡COMPLÉTER ICI!!
s2d (Scons (Scons (Scons Snil (Ssym "dec")) (Ssym v)) t) = Ldec v (s2t t)
s2d se = error ("Déclaration Psil inconnue: " ++ showSexp se)