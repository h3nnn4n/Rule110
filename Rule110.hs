import Control.Monad
import System.Environment
import System.Exit
import System.Random
import Data.List

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

deads  0 = []
deads  n = Dead  : deads (n-1)

alives 0 = []
alives n = Alive : alives (n-1)

sample = deads 19 ++ [Alive]


iter xs = a : iter_ xs ++ [z]
    where
        a = step (last xs) (head xs) (xs !! 2)
        z = step ((reverse xs) !! 2) (head $ reverse xs) (head xs)

iter_ (x:y:z:[]) = [step x y z]
iter_ (x:y:z:xs) =  step x y z : iter_ (y:z:xs)


rule110 xs  n = xs : rule110' xs n

rule110' xs 0 = []
rule110' xs n = done : rule110' done (n-1)
    where
        done = iter xs


display []     = return ()
display (x:xs) = do
    putStrLn $ conv x
    display xs

conv []     = ""
conv (x:xs)
    | x == Dead  = " " ++ conv xs
    | x == Alive = "x" ++ conv xs

--return $ take n (randomRs (0, 1) g)

--numbersToCell :: (Eq a, Num a) => [a] -> IO [Cell]
numbersToCell [] = []
numbersToCell (x:xs)
    | x == 1 = Alive : numbersToCell xs
    | x == 0 = Dead  : numbersToCell xs

randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . randomR (0,1))

main = do
    args <- getArgs
    g <- newStdGen

    when (null args) $ error "Usage: ./rule110 size"

    let size = read (args !! 0)
        tape = deads size ++ [Alive]
        word = numbersToCell $ randomlist size g

    --putStrLn $ show word

    display $ rule110 word size
    return ()

