module Main where

import Data.Char
import Data.List
import Data.Maybe

import Foreign

import System.Environment
import System.IO

import FastString3 (FastString)
import qualified FastString3 as FS

import Debug.Trace

----------------------------------------------------------------

type Base = Char
type DNA = FastString
type RNA = FastString

type Pattern = [PItem]
data PItem = PLiteral !Base
           | PSkip !Int
           | PSearch !DNA
           | POpen
           | PClose
           | PPushRNA !RNA
             deriving (Show, Eq)

type Template = [TItem]
data TItem = TLiteral !Base
           | TProtect !Int !Int
           | TQuote !Int
           | TPushRNA !RNA
           | TEnd
             deriving (Show, Eq)

type Environment = [DNA]

asnat :: Int -> DNA
asnat n = FS.pack $ asnat' n
    where
      asnat' 0 = ['P']
      asnat' n' = if (n' > 0) && (n' `rem` 2 == 0)
                  then 'I' : asnat' (n' `div` 2)
                  else 'C' : asnat' (n' `div` 2)

quote :: DNA -> DNA
quote d =  let l = FS.length d in
           if l == 0
           then FS.empty
           else FS.pack $ concatMap (qt . fromJust . FS.index d) [0..((trace ("quote " ++ show l)) $ l - 1)]
    where
      qt c = case c of
               'I' -> "C"
               'C' -> "F"
               'F' -> "P"
               'P' -> "IC"
               _ -> error "something wrong in quote"

protect :: Int -> DNA -> DNA
protect 0 s = s
protect n s = head . drop n . iterate quote $ s

showSnippet :: FastString -> String
showSnippet s = case FS.length s of
                  len -> (FS.unpack $ FS.take 10 s) ++ " (" ++ (show len) ++ ")"

replace :: Template -> Environment -> DNA
replace t e = expandT t FS.empty
    where
      -- FIXME: use a foldr
      expandT :: Template -> DNA -> DNA
      expandT [] acc = acc
      expandT (ti:ts) acc = case ti of
                              TLiteral n -> case span isLiteral ts of
                                              ([], rest) -> expandT rest $ FS.snoc acc n
                                              (ls, rest) -> expandT rest $ FS.append acc (FS.pack $ n : map unwrap ls)
                              TProtect n l -> if n >= (fromIntegral $ length e)
                                              then expandT ts $ FS.append acc (protect l FS.empty)
                                              else expandT ts $ FS.append acc (protect l (e !! (fromIntegral n)))
                              TQuote n -> if n >= (fromIntegral $ length e)
                                          then expandT ts $ FS.append acc (asnat 0)
                                          else expandT ts $ FS.append acc (asnat (fromIntegral $ FS.length (e !! (fromIntegral n))))
                              TEnd -> expandT ts acc  -- FIXME: can we get rid of TEnd ?
                              _ -> error "unexpected template"

      isLiteral (TLiteral _) = True
      isLiteral _ = False

      unwrap (TLiteral b) = b
      unwrap _ = error "not a literal"

subSeq :: Int -> Int -> FastString -> FastString
subSeq b e = if e >= b
             then FS.take (e - b) . FS.drop b
             else const FS.empty

----------------------------------------------------------------
-- execute
data Context = Ctxt DNA !Int deriving (Show)

execute :: Int -> Context -> IO (Bool, Context)
execute 0 ct = return (False, ct)
execute n ct = do
  mct' <- once ct
  case mct' of
    Just ct' -> execute (n - 1) ct'
    Nothing -> return (True, ct)

once :: Context -> IO (Maybe Context)
once (Ctxt dna ticks) = do
  case once' dna of
    Just (dna', rna') -> do mapM FS.putStr rna'
                            return $! Just $! Ctxt dna' (ticks + 1)
    Nothing -> return Nothing

once' :: DNA -> Maybe (DNA, [RNA])
once' dna = do
  (p, dna') <- pattern dna 0
  (t, dna'') <- template dna'
  case filterPatternRNA p of
    (p', rna1) -> case filterTemplateRNA t of
                    (t', rna2) -> case matchReplace dna'' p' t' of
                                    dna''' -> return $! (dna''', rna1 ++ rna2)
    where
      filterPatternRNA p = case partition isPPush p of
                             (rna, p') -> (p', map (\(PPushRNA gi) -> gi) rna)

      filterTemplateRNA t = case partition isTPush t of
                              (rna, t') -> (t', map (\(TPushRNA gi) -> gi) rna)

      isPPush (PPushRNA _) = True
      isPPush _ = False

      isTPush (TPushRNA _) = True
      isTPush _ = False

-- Here's a comment
matchReplace :: DNA -> Pattern -> Template -> DNA
matchReplace dna p t = case match dna p of
                         Just (e, dna') -> FS.append (replace t e) dna'
                         Nothing -> dna

match :: DNA -> Pattern -> Maybe (Environment, DNA)
match dna pat = do (i, e) <- mr' dna pat 0 [] []
                   return $! (e, FS.drop i dna)

mr' :: DNA -> Pattern -> Int -> Environment -> [Int] -> Maybe (Int, Environment)
mr' _ [] i e _ = Just (i, reverse e)
mr' dna (p:ps) i e c = 
    case p of
      PLiteral b -> if (fromJust $ dna `FS.index` i) == b
                    then mr' dna ps (i + 1) e c
                    else Nothing
      PSkip n -> if ((i + n) > (FS.length dna))
                 then Nothing
                 else mr' dna ps (i + n) e c
      PSearch s -> case FS.search s $ FS.drop i dna of
                     Nothing -> Nothing
                     Just n -> mr' dna ps (i + n + (FS.length s)) e c
      POpen -> mr' dna ps i e (i : c)
      PClose -> mr' dna ps i ((subSeq (c !! 0) i dna) : e) (tail c)
      _ -> error "unexpected pattern"

pattern :: DNA -> Int -> Maybe (Pattern, DNA)
pattern dna lvl = do (p, dna') <- pitem dna
                     case p of
                       POpen -> do (ps, dna'') <- pattern dna' (lvl + 1)
                                   return $! (POpen : ps, dna'')
                       PClose -> if lvl == 0
                                 then return ([], dna')
                                 else do (ps, dna'') <- pattern dna' (lvl - 1)
                                         return (PClose : ps, dna'')
                       _ -> do (ps, dna'') <- pattern dna' lvl
                               return $! (p : ps, dna'')

pitem :: DNA -> Maybe (PItem, DNA)
pitem dna = level0
    where
      level0 = case FS.index dna 0 of
                 Just 'C' -> lit 1 'I'
                 Just 'F' -> lit 1 'C'
                 Just 'P' -> lit 1 'F'
                 Just 'I' -> level1
                 _ -> Nothing

      level1 = case FS.index dna 1 of
                 Just 'C' -> lit 2 'P'
                 Just 'P' -> do (n, dna') <- nat (FS.drop 2 dna)
                                return (PSkip n, dna')
                 Just 'F' -> case consts (FS.drop 3 dna) of
                               (s, dna') -> return $! (PSearch s, dna')
                 Just 'I' -> level2
                 _ -> Nothing

      level2 = case FS.index dna 2 of
                 Just 'P' -> return (POpen, FS.drop 3 dna)
                 Just 'C' -> return (PClose, FS.drop 3 dna)
                 Just 'F' -> return (PClose, FS.drop 3 dna)
                 Just 'I' -> return (PPushRNA (FS.range 3 10 dna), FS.drop 10 dna)
                 _ -> Nothing

      lit n b = Just (PLiteral b, FS.drop n dna)

template ::  DNA -> Maybe (Template, DNA)
template dna = do (item, dna') <- titem dna
                  case item of
                    TEnd -> return ([], dna')
                    _ -> do (rest, dna'') <- template dna'
                            return (item : rest, dna'')

titem :: DNA -> Maybe (TItem, DNA)
titem dna = level0
    where
      level0 = case FS.index dna 0 of
                 Just 'C' -> lit 1 'I'
                 Just 'F' -> lit 1 'C'
                 Just 'P' -> lit 1 'F'
                 Just 'I' -> level1
                 _ -> Nothing

      level1 = case FS.index dna 1 of
                 Just 'C' -> lit 2 'P'
                 Just 'F' -> prot
                 Just 'P' -> prot
                 Just 'I' -> level2
                 _ -> Nothing

      level2 = case FS.index dna 2 of
                 Just 'C' -> Just (TEnd, FS.drop 3 dna)
                 Just 'F' -> Just (TEnd, FS.drop 3 dna)
                 Just 'P' -> do (n, dna') <- nat (FS.drop 3 dna)
                                Just (TQuote n, dna')
                 Just 'I' -> Just (TPushRNA (FS.range 3 10 dna), FS.drop 10 dna)
                 _ -> Nothing

      lit n b = Just (TLiteral b, FS.drop n dna)
      prot = do (l, dna') <- nat (FS.drop 2 dna)
                (n, dna'') <- nat dna'
                return $! (TProtect n l, dna'')

nat :: DNA -> Maybe (Int, DNA)
nat dna = level0
    where
      level0 = case FS.index dna 0 of
                 Just 'P' -> Just (0, FS.tail dna)
                 Just 'I' -> reply 0
                 Just 'F' -> reply 0
                 Just 'C' -> reply 1
                 _ -> Nothing

      reply i = do (n, dna') <- nat (FS.tail dna)
                   return $! (2 * n + i, dna')

consts :: DNA -> (DNA, DNA)
consts dna = level0
    where
      level0 = case FS.index dna 0 of
                 Just 'C' -> reply 1 'I'
                 Just 'F' -> reply 1 'C'
                 Just 'P' -> reply 1 'F'
                 Just 'I' -> level1
                 _ -> bad

      level1 = case FS.index dna 1 of 
                 Just 'C' -> reply 2 'P'
                 _ -> bad

      bad = (FS.empty, dna)
      reply n c = case consts (FS.drop n dna) of
                    (s, dna') -> (FS.cons c s, dna')

----------------------------------------------------------------
-- Top level
main :: IO ()
main = do
  args <- getArgs
  endo <- FS.getContents
  let mkDNA prefix = FS.append (FS.pack prefix) endo

  case args of
    "-n" : countS : dna : _ -> runCount (read countS) (mkDNA dna)
    dna : _ -> runCount 10000000 (mkDNA dna)
    _ -> hPutStrLn stderr $ "usage: dna2rna [-n <iters>] <dna prefix>"

test :: IO ()
test = do
  endo <- FS.readFile "dna/endo.dna"
  runCount 15 endo

runCount :: Int -> DNA -> IO ()
runCount n dna = run' n $ Ctxt dna 0

run' :: Int -> Context -> IO ()
run' 0 _ = return ()
run' n ct = do
  r <- execute l ct
  case r of
    (True, ct') -> run' 0 ct'
    (False, ct') -> do progress ct'
                       run' (n - l) ct'
  where l = min 50000 n

printDNA :: Context -> IO ()
printDNA (Ctxt dna _) = FS.putStrLn dna

progress :: Context -> IO ()
progress (Ctxt _ ticks) = hPutStrLn stderr $ show ticks
