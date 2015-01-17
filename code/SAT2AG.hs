import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import System.Environment

-- Types
type CNF = [Clause]
type Clause = [Lit] 
data Lit = Pos String
         | Neg String

-- Example formula
-- (a ∨ ¬b ∨ ¬c) ∧ (¬a ∨ ¬d) ∧ d ∧ (¬b ∨ ¬c ∨ d)
example :: CNF
example = [ [Pos "a", Neg "b", Neg "c"], [Neg "a", Neg "d"], [Pos "d"], [Neg "b", Neg "c", Pos "d"] ]

pidgeon :: Int -> CNF
pidgeon n = [[Neg v1, Neg v2] | row <- matrix, v1 <- row, v2 <- row, v1 < v2] 
  ++ [ map Pos column  | column <- transpose matrix ] where
    matrix = [ [ "p" ++ show i ++ "x" ++ show j | j <- [0..n] ] | i <- [1..n] ]

-- Helpers
vars :: CNF -> Set String 
vars = Set.unions . map (Set.fromList . map f) where
  f (Pos v) = v
  f (Neg v) = v


-- Generators
genVars :: CNF -> IO ()
genVars cnf = do
  let vs = Set.toList (vars cnf)
  forM_ vs $ \v -> putStrLn $ "data Var_" ++ v ++ " | Var"
  putStrLn $ "set Vars = " ++ unwords [ "Var_" ++ v | v <- vs ]
  putStrLn ""
  putStrLn "attr Vars"
  putStrLn "  inh true :: {()}"
  putStrLn "  inh false :: {()}"
  putStrLn "  syn true :: {()}"
  putStrLn "  syn false :: {()}"
  putStrLn ""
  putStrLn "sem Vars | Var"
  putStrLn "  lhs.true  = @lhs.false"
  putStrLn "  lhs.false = @lhs.true"

genSat :: CNF -> IO ()
genSat cnf = do
  putStrLn ""
  putStrLn "data SAT"
  forM_ (zip [1..] cnf) $ \(i,c) -> do
    putStrLn $ "  | Clause_" ++ show i
    forM_ (zip [1..] c) $ \(j,lit) -> do
      let v = case lit of Pos v -> v; Neg v -> v
      putStrLn $ "    c" ++ show i ++ "_" ++ show j ++ " :: Var_" ++ v
  putStrLn ""
  putStrLn "attr SAT"
  putStrLn "  inh x :: {()}"
  putStrLn "  syn x :: {()}"
  putStrLn ""
  putStrLn "sem SAT"
  putStrLn "  -- Default values"
  putStrLn "  | *"
  putStrLn "    loc.true = ()"
  putStrLn "    loc.false = ()"
  forM_ (zip [1..] cnf) $ \(i,c) -> do
    putStrLn $ "  | Clause_" ++ show i
    let cs = zip [1..] c
    let lns = zip (cs ++ [(0,undefined)]) ([(0,undefined)] ++ cs)
    forM_ lns $ \((j1,c1),(j2,c2)) -> do
      let vleft | j1 == 0 = "lhs.x"
                | otherwise = "c" ++ show i ++ "_" ++ show j1 
                              ++ "." ++ case c1 of
                                Pos _ -> "true"
                                Neg _ -> "false"
      let vright | j2 == 0 = "@lhs.x"
                 | otherwise = "@c" ++ show i ++ "_" ++ show j2
                               ++ "." ++ case c2 of
                                 Pos _ -> "true"
                                 Neg _ -> "false"
      putStrLn $ "    " ++ vleft ++ " = " ++ vright

genTop :: CNF -> IO ()
genTop _ = do
  putStrLn ""
  putStrLn "data Top | Top sat :: SAT"
  putStrLn "sem Top | Top sat.x = @sat.x"

genAll :: CNF -> IO ()
genAll cnf = genVars cnf >> genSat cnf >> genTop cnf

main :: IO ()
main = do
  args <- getArgs
  let (n:ns) = map read args
  genAll $ map snd $ filter (\(i,c) -> i `notElem` ns) $ zip [1..] $ pidgeon n
