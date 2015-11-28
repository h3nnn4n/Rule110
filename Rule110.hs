module Rule110 where

data Cell = Alive | Dead
     deriving (Show, Eq)

step Alive Alive Alive = Dead
step Alive Alive Dead  = Alive
step Alive Dead  Alive = Alive
step Alive Dead  Dead  = Dead
step Dead  Alive Alive = Alive
step Dead  Alive Dead  = Alive
step Dead  Dead  Alive = Alive
step Dead  Dead  Dead  = Dead

sample = [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Alive]

iter_ (x:y:z:[]) = [step x y z]
iter_ (x:y:z:xs) =  step x y z : iter_ (y:z:xs)

--iter xs = a : iter_ xs : z
iter xs = a : iter_ xs ++ [z]
    where
        a = step (last xs) (head xs) (xs !! 2)
        z = step ((reverse xs) !! 2) (head $ reverse xs) (head xs)

rule110 xs 0 = []
rule110 xs n = done : rule110 done (n-1)
    where
        done = iter xs

display []     = putStrLn " "
display (x:xs) = do
                    putStrLn w
                    display xs

    where
        w = conv x

conv []     = ""
conv (x:xs)
    | x == Dead  = " " ++ conv xs
    | x == Alive = "x" ++ conv xs
