-- https://www.fpcomplete.com/user/bartosz/understanding-algebras
--

data ExprF a = Const Int
        | Add a a
        | Mul a a
        --deriving (Show)

type Algebra f a = f a -> a

newtype Fix f = Fx (f (Fix f))

-- * type Expr = Fix ExprF

-- * type ExprInitAlg = Algebra ExprF (Fix ExprF)

instance Functor ExprF where
    fmap eval (Const i) = Const i
    fmap eval (left `Add` right) = (eval left) `Add` (eval right)
    fmap eval (left `Mul` right) = (eval left) `Mul` (eval right)

type SimpleA = Algebra ExprF Int

-- alg :: ExprF Int -> Int
alg :: SimpleA
alg (Const i) = i
alg (x `Add` y) = x + y
alg (x `Mul` y) = x * y

-- * ex_init_alg :: ExprF (Fix ExprF) -> Fix ExprF
-- * ex_init_alg = Fx

unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x

-- * g = alg . (fmap g) . unFix

-- * cata :: Functor f => (f a -> a) -> (Fix f -> a)
-- * cata alg = alg . fmap (cata alg) . unFix

eval :: Fix ExprF -> Int
eval = alg . fmap eval . unFix
-- alg ((eval x) `Mul` (eval y))
-- alg ((eval x) `Mul` 4)
-- alg ((alg (eval x' `Add` eval y')) `Mul` 4)
-- alg ((alg (2 `Add` 3)) `Mul` 4)

val = Fx (Const 12)

testExpr = Fx $ (Fx $ (Fx $ Const 2) `Add`
                (Fx $ Const 3)) `Mul` (Fx $ Const 4)

main = print $ eval $ testExpr

