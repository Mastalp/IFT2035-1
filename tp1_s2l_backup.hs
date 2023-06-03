--
-- Pattern matching

--s2l (Scons (Scons (Scons Snil e) (Ssym ":")) t) = Lhastype (s2l e) (s2t t)

-- LET x = y in f 
--s2l (Scons (Scons (Scons (Scons Snil (Ssym "let")) (Ssym v)) e1) e2) = Llet v (s2l e1) (s2l e2)
--s2l (Scons (Scons (Scons Snil (Ssym "let")) )
-- (Scons Snil (Ssym "let")) (Scons Snil (Scons (Scons Snil (Ssym "x")) (Snum 5)))) (Scons (Scons (Scons Snil (Ssym "*")) (Ssym "x")) (Snum 4)))
{-
s2l (Scons (Scons (Scons Snil (Ssym "fun")) (Ssym v)) e) = Lfun v (s2l e)
-}
--s2l se = aux (s2slist se)

-- pairs of exp -- ! TO DO ! --
-- FUNCTION CALLS (LAPP) -- (f a b c) et va retourner : 
-- Lapp (Lapp (Lapp (Lvar "f") (Lvar "a")) (Lvar "b")) (Lvar "c")
{-
s2l (Scons Snil e1) = s2l e1 -- base case
s2l (Scons e1 e2) = s2l' (s2l e1) e2
  where 
    s2l' exp1 (Scons exp2 exp3) = s2l' (Lapp exp1 (s2l exp2)) exp3
    s2l' exp1 exp2 = Lapp exp1 (s2l exp2)
-}

{-
s2l se
  | not (null t) && aux4 t = aux t
  where t = s2slist se
s2l se = error ("Expression Psil inconnue: " ++ (showSexp se))
-}
{-
s2l (Scons (Scons (Scons Snil (Ssym ":")) e) t) = Lhastype (s2l e) (s2t t)

s2l (Scons (Scons (Scons Snil (Ssym "+")) e1) e2) = Lapp (Lapp (Lvar "+") (s2l e1)) (s2l e2)
s2l (Scons (Scons (Scons Snil (Ssym "*")) (Snum x)) (Snum y)) = Lapp (Lapp (Lvar "*") (Lnum x)) (Lnum y)
-}
{-
s2l (Scons e1 e2) = case s2slist (Scons e1 e2) of

    [Snum i, Ssym ":", _] -> Lhastype (Lnum i) Lint
    [Ssym v, Ssym ":", t] -> Lhastype (Lvar v) (s2t t)

    [Ssym "let", Ssym v, e1', e2'] -> Llet v (s2l e1') (s2l e2')
    [Ssym "fun", Ssym v, e] -> Lfun v (s2l e)

    --[e1', e2'] -> Lapp (s2l e1') (s2l e2')
    _ -> error ("Expression Psil inconnue: " ++ showSexp (Scons e1 e2))
-}


{-
aux :: [Sexp] -> Lexp
aux (Ssym "->":xs) = Lapp (s2l (head xs)) (s2l (last xs))
aux (x:xs) = Lapp (s2l x) (aux xs)
-}

aux :: [Sexp] -> Lexp
aux = aux' . reverse
  where
    aux' [x] = s2l x
    aux' (x:y:ys) = Lapp (aux' (y:ys)) (s2l x)

aux4 :: [Sexp] -> Bool
aux4 [_] = True
aux4 ((Ssym _):xs) = aux4 xs
aux4 _ = False



-----------------------------
-- operators
    --(Ssym op:exps) | op `elem` ["+", "-", "*", "/"] && length exps == 2 -> Lapp (Lapp (Lvar op) (s2l (exps !! 0))) (s2l (exps !! 1))
    --(Ssym op:exp:[]) | op `elem` ["+", "-", "*", "/"] -> Lapp (Lvar op) (s2l exp)
    {-
    (Ssym op:exps) | op `elem` ["+", "-", "*", "/"] -> 
      case exps of
        (x:y:xs) -> 
          let le1 = s2l x
              le2 = s2l y
          in Lapp (Lapp (Lvar op) le1) le2
        (e1:e2:rest) -> 
          let le1 = s2l e1
              --le2 = s2l e2
          in Lapp (Lapp (Lvar op) le1) (processSexpList (Ssym op : e2 : rest))
    -}