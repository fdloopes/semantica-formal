------------------------------------------------------------
-- Semântica Formal (2018/02)
-- Nome: Felipe Lopes
--
-- Trabalho 2: Completar a implementacao da semantica Small-Step
-- e adicionar os comandos For, Repeat Until e Dupla Atribuicao
--
------------------------------------------------------------

import Estado

 -- Expressões aritméticas
data AExp = Num Int
     |Var String
     |Soma AExp AExp
     |Sub AExp AExp
     |Mult AExp AExp
  deriving(Show)

-- Expressões booleanas
data BExp = TRUE
     | FALSE
     | Not BExp
     | And BExp BExp
     | Or  BExp BExp
     | Ig  AExp AExp
   deriving(Show)

-- Comandos
data CExp = While BExp CExp
     | If BExp CExp CExp
     | Seq CExp CExp
     | Atrib AExp AExp
     | RepeatUntil CExp BExp
     | For CExp BExp CExp
     | DAtrib AExp AExp AExp AExp
     | Skip
   deriving(Show)

-- Expressões aritméticas SmallStep
aSmallStep :: (AExp,Estado) -> (AExp,Estado)
aSmallStep (Var x,s) = (Num (procuraVar s x),s)

--soma
-- soma3
aSmallStep (Soma (Num x) (Num y), s) = (Num (x+y),s)
-- soma2
aSmallStep (Soma (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s) in (Soma (Num x) ef,s)
-- soma1
aSmallStep (Soma e1 e2,s)  = let (ef,_) = aSmallStep (e1, s) in (Soma ef e2,s)

--subtracao
-- sub3
aSmallStep (Sub (Num x) (Num y), s) = (Num (x-y),s)
-- sub2
aSmallStep (Sub (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s)in (Sub (Num x) ef,s)
-- sub1
aSmallStep (Sub e1 e2,s)  = let (ef,_) = aSmallStep (e1, s) in (Sub ef e2,s)

-- multiplicacao
-- mult3
aSmallStep (Mult (Num x) (Num y), s) = (Num (x*y),s)
 -- mult2
aSmallStep (Mult (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s)in (Mult (Num x) ef,s)
-- mult1
aSmallStep (Mult e1 e2,s)  = let (ef,_) = aSmallStep (e1, s) in (Mult ef e2,s)

-- Interpretador para Expressões aritméticas
interpretA :: (AExp,Estado) -> (AExp,Estado)
interpretA (a,s) = if isFinalA a then (a,s) else interpretA (aSmallStep (a,s))

isFinalA :: AExp -> Bool
isFinalA (Num a) = True
isFinalA x = False

-- Expressões booleanas
bSmallStep :: (BExp,Estado) -> (BExp,Estado)

-- Not3
bSmallStep (Not FALSE,s) = (TRUE,s)
-- Not2
bSmallStep (Not TRUE,s) = (FALSE, s)
-- Not1
bSmallStep (Not b, s) = let (bn,sn) = bSmallStep (b,s) in (Not bn ,sn)

-- And3
bSmallStep (And TRUE b2,s) = (b2,s)
-- And2
bSmallStep (And FALSE b2,s) = (FALSE,s)
-- And1
bSmallStep (And b1 b2,s) = let (bn,sn) = bSmallStep (b1,s) in (And bn b2,sn)

-- Or3
bSmallStep (Or TRUE b2,s) = (TRUE, s)
-- Or2
bSmallStep (Or FALSE b2,s) = (b2, s)
-- Or1
bSmallStep (Or b1 b2,s) = let(bn, sn) = bSmallStep (b1, s) in (Or bn b2, sn)

-- Igual3
bSmallStep (Ig (Num x) (Num y), s)  = if x==y then (TRUE, s)  else (FALSE, s)
-- Igual2
bSmallStep (Ig (Num x) e2,s )  = let (ef,_) = aSmallStep(e2, s) in (Ig (Num x) ef, s)
-- Igual1
bSmallStep (Ig e1 e2,s )  = let (ef, _) = aSmallStep(e1, s) in (Ig ef e2, s)

interpretB :: (BExp,Estado) -> (BExp,Estado)
interpretB (b,s) = if isFinalB b then (b,s) else interpretB (bSmallStep (b,s))

isFinalB :: BExp -> Bool
isFinalB TRUE = True
isFinalB FALSE = True
isFinalB x = False

cSmallStep :: (CExp,Estado) -> (CExp,Estado)

-- IF3
cSmallStep (If FALSE c1 c2,s) = (c2, s)
-- IF2
cSmallStep (If TRUE c1 c2,s) = (c1, s)
-- IF1
cSmallStep (If b c1 c2,s) = let(bf,sL) = bSmallStep(b,s) in (If bf c1 c2,sL)

-- Atrib2
cSmallStep (Atrib (Var x) (Num n), s) = (Skip, mudaVar s x n)
-- Atrib1
cSmallStep (Atrib (Var x) e,s) = let(ef,sL) = aSmallStep(e,s) in (Atrib (Var x) ef,sL)

-- Seq2
cSmallStep (Seq Skip c,s) = cSmallStep(c,s)
-- Seq1
cSmallStep (Seq c1 c2,s) = let(cf,sL) = cSmallStep(c1,s) in (Seq cf c2,sL)

-- While
cSmallStep (While b c,s) = let(bf,sL) = bSmallStep(b,s) in (If bf (Seq c (While b c)) (Skip), sL)

-- Repeat Until
cSmallStep (RepeatUntil c b,s) = let(cf,sL) = cSmallStep(c,s) in (Seq c (If b (Skip) (RepeatUntil c b)), sL)

-- For
cSmallStep (For c1 b c2,s) = let(cf,sL) = cSmallStep(c1,s) in (RepeatUntil c2 b,sL)

-- Dupla atribuicao
-- DA3
cSmallStep (DAtrib (Var x) (Var y) (Num z) (Num w), s) = let(_,sL) = (Skip,mudaVar s x z) in (Skip, (mudaVar sL y w))
-- DA2
cSmallStep (DAtrib (Var x) (Var y) (Num z) e,s) = let(ef,sL) = aSmallStep(e,s) in (DAtrib (Var x) (Var y) (Num z) ef,sL)
-- DA1
cSmallStep (DAtrib (Var x) (Var y) e1 e2,s) = let(ef,sL) = aSmallStep(e1,s) in (DAtrib (Var x) (Var y) ef e2,sL)


interpretC :: (CExp,Estado) -> (CExp,Estado)
interpretC(c,s) = if isFinalC c then (c,s) else interpretC (cSmallStep (c,s))

isFinalC :: CExp -> Bool
isFinalC Skip = True
isFinalC x = False

------------------------------------ EXEMPLOS DE TESTES -------------------------------

 -- Estado representa a "memória"
meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",11)]

-- 3 * (3 - 0) = 9
exemplo :: AExp
exemplo = Mult (Num 3) (Sub (Num 3) (Num 0))

-- y == z ?
exemplo2 :: BExp
exemplo2 = (Ig (Var"y") (Var"z"))

-- Se exemplo2 == true then Skip else exemplo4
exemplo3 :: CExp
exemplo3 = (If exemplo2 Skip exemplo4)

-- y := 71
exemplo4::CExp
exemplo4 = (Atrib (Var "y") (Num 71))

-- interpreta exemplo4 e na sequencia o exemplo6
exemplo5::CExp
exemplo5 = (Seq exemplo4 exemplo6)

-- z := 17
exemplo6::CExp
exemplo6 = (Atrib (Var "z") (Num 17))

-- Enquanto exemplo2 for verdade interpreta exemplo8
exemplo7::CExp
exemplo7 = (While exemplo2 exemplo8)

-- y := y+1
exemplo8::CExp
exemplo8 = (Atrib (Var "y") (Soma (Var "y") (Num 1)))

-- Repeta exemplo8 enquanto exemplo2 for falso
exemplo9::CExp
exemplo9 = (RepeatUntil exemplo8 exemplo2)

-- for y:=0, y != z, y:=y+1
exemplo10::CExp
exemplo10 = (For exemplo11 exemplo12 exemplo8)

-- y:=0
exemplo11::CExp
exemplo11 = (Atrib (Var "y") (Num 0))

-- y == z ???
exemplo12 :: BExp
exemplo12 = Ig (Var"y") (Var"z")

-- x:=23, y:=55
exemplo13::CExp
exemplo13 = DAtrib (Var "x") (Var "y") (Num 23) (Num 55)
