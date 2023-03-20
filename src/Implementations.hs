{-# LANGUAGE LambdaCase,
             BlockArguments,
             ViewPatterns,
             RecursiveDo,
             TemplateHaskell,
             MagicHash,
             UnboxedTuples,
             GHCForeignImportPrim,
             UnliftedFFITypes
             #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-name-shadowing -Wno-unused-top-binds #-}

module Implementations (implementations) where

import Data.List
import GHC.Exts
import Foreign
import GHC.IO
import Unsafe.Coerce
import Language.Haskell.TH
import Control.Monad.Fix
import Control.Monad
import qualified GHC.List
import qualified Data.Vector.Unboxed as V

-- | by \@sand_witch
--
-- The most intresting part of this function is
-- that is can be fused and work with infinite
-- lists if you will not evaluate it's elements
-- >>> length . take 20 $ scanrLazy [1 ..]
-- 20
-- >>> take 20 $ scanrLazy [1 ..]
-- [ -- ... will not finish
scanrLazy :: [Int] -> [Int]
scanrLazy = init . scanr (+) 0 where
  scanr f z = foldr (\h ~(x:xs) -> f h x : x : xs) [z]
{-# INLINE scanrLazy #-}

-- | by \@sand_witch
scanrManual :: [Int] -> [Int]
scanrManual = init . scanr (+) 0 where
  scanr f z = foldr (\h (x:xs) -> f h x `seq` f h x : x : xs) [z]
{-# INLINE scanrManual #-}

-- | by \@sand_witch & \@goosedb
scanrPrelude :: [Int] -> [Int]
scanrPrelude = init . scanr (+) 0
{-# INLINE scanrPrelude #-}

-- | by \@goosedb & \@sand_witch
scanr1Prelude :: [Int] -> [Int]
scanr1Prelude = scanr1 (+)
{-# INLINE scanr1Prelude #-}

-- | by \@Des333
scanlPrelude :: [Int] -> [Int]
scanlPrelude = init . (scanl (-) =<< sum)
{-# INLINE scanlPrelude #-}

-- | by \@daniladanko (& \@Des333)
scanl'Prelude :: [Int] -> [Int]
scanl'Prelude = init . (scanl' (-) =<< sum)
{-# INLINE scanl'Prelude #-}

-- | by \@aadaa_fgtaa
scanl'Manual :: [Int] -> [Int]
scanl'Manual = initscanl' (-) =<< sum
    where
      initscanl' :: (b -> a -> b) -> b -> [a] -> [b]
      initscanl' f = go
        where
          go !_ [] = []
          go !q (x:xs) = q:go (f q x) xs
{-# INLINE scanl'Manual #-}

-- | by \@sand_witch
reverse'scanl''reverse :: [Int] -> [Int]
reverse'scanl''reverse = reverse . tail . scanl' (+) 0 . reverse
{-# INLINE reverse'scanl''reverse #-}

-- | by \@cblp_su
obviousFunction :: [Int] -> [Int]
obviousFunction = init . map sum . tails
{-# INLINE obviousFunction #-}

-- | by \@sand_witch
foldl''reverse :: [Int] -> [Int]
foldl''reverse = foldl' (\case [] -> (\x -> [x]); (x:xs) -> (\y -> y + x `seq` y + x : x : xs)) [] . reverse
{-# INLINE foldl''reverse #-}

-- | by \@sand_witch & \@aadaa_fgtaa
foldrPrelude :: [Int] -> [Int]
foldrPrelude = foldr (\y -> \case [] -> [y]; (x:xs) ->  y + x `seq` y + x : x : xs) []
{-# INLINE foldrPrelude #-}

-- | by \@sand_witch & \@aadaa_fgtaa
--
-- This function fails on infinite loop on the inline lazy
-- bench group if you bang the acc in the [] case.
--
-- Seems like a GHC bug.
-- Introduced in 9.0, fixed in GHC 9.6
--
-- Bug MRE: https://play.haskell.org/saved/BxBMHr0B
--
-- TODO: Find a ticket.
tardisManual :: [Int] -> [Int]
tardisManual list =
  let (res, sumKnot) = go list 0 sumKnot in res
  where
    go [] acc _ = ([], acc)
    go (x:xs) !acc sumKnot =
      let !(res', sumKnot') = go xs (acc + x) sumKnot
      in (sumKnot - acc : res', sumKnot')
{-# INLINE tardisManual #-}

-- | by \@effectfully
-- >>> backsums []
-- []
-- >>> backsums [4]
-- [4]
-- >>> backsums [1,2,3]
-- [6,5,3]
-- >>> length . take 3 $ backsums [1..]
-- 3
backsums :: [Int] -> [Int]
backsums = fst . foldr step ([], 0) where
    step x ~(xs', acc) = (acc' : xs', acc') where
        acc' = acc + x
    {-# INLINE step #-}
{-# INLINE backsums #-}

-- | by \@effectfully
-- >>> backsums []
-- []
-- >>> backsums [4]
-- [4]
-- >>> backsums [1,2,3]
-- [6,5,3]
-- >>> length . take 3 $ backsums [1..]
-- -- will not terminate
backsumsStrict :: [Int] -> [Int]
backsumsStrict = fst . foldr step ([], 0) where
    step x ~(xs', acc) = (acc' : xs', acc') where
        !acc' = acc + x
    {-# INLINE step #-}
{-# INLINE backsumsStrict #-}

------------------------------------------

data Acc = Acc ![Int] {-# UNPACK #-} !Int

unAcc :: Acc -> [Int]
unAcc (Acc xs _) = xs
{-# INLINE unAcc #-}

-- | by \@effectfully
backaccs :: [Int] -> [Int]
backaccs = unAcc . foldr step (Acc [] 0) where
    step x (Acc xs' acc) = Acc (acc' : xs') acc' where
        acc' = acc + x
    {-# INLINE step #-}
{-# INLINE backaccs #-}

--------------------------------------------

-- | by \@aadaa_fgtaa
scanl'ManualU :: [Int] -> [Int]
scanl'ManualU = initscanlU' (-) =<< sumU
  where
    initscanlU' o = go
      where
        go !q (a:b:c:d:e:f:g:h:xs) = q:a':b':c':d':e':f':g':go h' xs
          where
            !a' = o q  a
            !b' = o a' b
            !c' = o b' c
            !d' = o c' d
            !e' = o d' e
            !f' = o e' f
            !g' = o f' g
            !h' = o g' h
        go !w (x:xs) = w:go x' xs
          where
            !x' = o w x
        go !_ [] = []

    sumU = go 0
      where
        go !w (a:b:c:d:e:f:g:h:xs) = go (a + b + c + d + e + f + g + h + w) xs
        go !w               (x:xs) = go (w + x) xs
        go !w                   [] = w
{-# INLINE scanl'ManualU #-}

-- | by \@aadaa_fgtaa
scanr1ManualU :: [Int] -> [Int]
scanr1ManualU = unAcc . go
  where
    go :: [Int] -> Acc
    go []      = Acc [] 0
    go (a_ : b_ : c_ : d_ : e_ : f_ : g_ : h_ : xs) =
      let
        !a = a_
        !b = b_
        !c = c_
        !d = d_
        !e = e_
        !f = f_
        !g = g_
        !h = h_
      in let
        !(Acc xs' r) = go xs
      in let
        !h' = r + h
        !g' = h' + g
        !f' = g' + f
        !e' = f' + e
        !d' = e' + d
        !c' = d' + c
        !b' = c' + b
        !a' = b' + a
      in Acc (a':b':c':d':e':f':g':h':xs') a'
    go (!x:xs) = case go xs of Acc xs' r -> let !r' = x + r in Acc (r':xs') r'
{-# INLINE scanr1ManualU #-}

-- | by \@effectfully
-- >>> backlist []
-- []
-- >>> backlist [4]
-- [4]
-- >>> backlist [1,2,3]
-- [6,5,3]
backlist :: [Int] -> [Int]
backlist = go $ \_ -> id where
    go :: (Int# -> [Int] -> [Int]) -> [Int] -> [Int]
    go acc []          = acc 0# []
    go acc (I# x : xs) = go (\z rs -> let z' = z +# x in acc z' (I# z' : rs)) xs
    {-# INLINE go #-}
{-# INLINE backlist #-}

-------------------------------------------

-- | by \@effectfully
-- >>> revsums []
-- []
-- >>> revsums [4]
-- [4]
-- >>> revsums [1,2,3]
-- [6,5,3]
-- >>> length . take 3 $ revsums [1..]
-- 3
revsums :: Traversable t => t Int -> t Int
revsums xs = evalRevState (traverse step xs) 0 where
    step x = mdo
        let acc' = acc + x
        put acc'
        acc <- get
        return acc'
    {-# INLINE step #-}
{-# INLINE revsums #-}

--------------------

newtype RevState s a = RevState
    { runRevState :: s -> (a, s)
    }

evalRevState :: RevState s a -> s -> a
evalRevState a s = fst $ runRevState a s

get :: RevState s s
get = RevState $ \s -> (s, s)

put :: s -> RevState s ()
put s' = RevState $ \_ -> ((), s')

instance Functor (RevState s) where
    fmap = liftM

instance Applicative (RevState s) where
    pure  x = RevState $ \s -> (x, s)
    (<*>) = ap

instance Monad (RevState s) where
    a >>= f = RevState $ \s'' ->
        let (x, s)  = runRevState a s'
            (y, s') = runRevState (f x) s''
        in (y, s)

instance MonadFix (RevState s) where
    mfix f = RevState $ \s ->
        fix $ \(~(x, _)) -> runRevState (f x) s

------------------------------------------

-- | by \@qnikst
qnikst :: [Int] -> [Int]
qnikst = go where
  go [] = []
  go [x] = [x]
  go (x:xs) =
   let ~(r:rs) = go xs
       !s' = r+x
   in s':r:rs

-- | by \@qnikst
-- >>> qnikst2 []
-- []
-- >>> qnikst2 [4]
-- [4]
-- >>> qnikst2 [1,2,3]
-- [6,5,3]
-- >>> length . take 3 $ qnikst2 [1..]
-- 3
qnikst2 :: [Int] -> [Int]
qnikst2 xs = go s xs where
  s = sum' xs
  sum' [] = 0
  sum' (x:xs) = x + sum' xs
  go _ [] = []
  go _ [x] = [x]
  go s (x:xs) = let z = (s-x) in  s : go z xs

--------------------------------------------------

data WithInt a = WithInt a !Int

val :: WithInt a -> a
val (WithInt a _) = a
{-# INLINE[0] val #-}

-- | by \@aadaa_fgtaa
scanl'ManualUF :: [Int] -> [Int]
scanl'ManualUF = scanl'ManualUF'FB (:) []
{-# INLINE[1] scanl'ManualUF #-}

scanl'ManualUF'FB :: (Int -> l -> l) -> l -> [Int] -> l
scanl'ManualUF'FB k z = \xs ->
  let
    infixr 5 .:
    (.:) = k

    initscanlU' o = go
      where
        go !q (a:b:c:d:e:f:g:h:xs) = q .: a' .: b' .: c' .: d' .: e' .: f' .: g' .: go h' xs
          where
            !a' = o q  a
            !b' = o a' b
            !c' = o b' c
            !d' = o c' d
            !e' = o d' e
            !f' = o e' f
            !g' = o f' g
            !h' = o g' h
        go !w (x:xs) = w .: go x' xs
          where
            !x' = o w x
        go !_ [] = z

    sumU = go 0
      where
        go !w (a:b:c:d:e:f:g:h:xs) = go (a + b + c + d + e + f + g + h + w) xs
        go !w               (x:xs) = go (w + x) xs
        go !w                   [] = w
  in
    initscanlU' (-) =<< sumU $ xs
{-# INLINE[1] scanl'ManualUF'FB #-}

scanl'ManualUF'BackaccsFB :: (Int -> a -> b) -> Int -> WithInt a -> WithInt b
scanl'ManualUF'BackaccsFB k x (WithInt xs ((+ x) -> !s)) = WithInt (k s xs) s
{-# INLINE[0] scanl'ManualUF'BackaccsFB #-}

{-# RULES "scanl'ManualU fuse" [~1] forall . scanl'ManualUF = \xs -> build \k z -> val (GHC.List.foldr (scanl'ManualUF'BackaccsFB k) (WithInt z 0) xs) #-}
{-# RULES "scanl'ManualU unfuse" [1] forall k z xs . val (GHC.List.foldr (scanl'ManualUF'BackaccsFB k) (WithInt z 0) xs) = scanl'ManualUF'FB k z xs #-}

----------------------------------------


-- | by \@aadaa_fgtaa
scanr1ManualUF :: [Int] -> [Int]
scanr1ManualUF = scanr1ManualUF'FB (:) []
{-# INLINE[1] scanr1ManualUF #-}

scanr1ManualUF'FB :: (Int -> l -> l) -> l -> [Int] -> l
scanr1ManualUF'FB k z = \xs ->
  let
    infixr 5 .:
    (.:) = k

    go [] = WithInt z 0
    go (a_ : b_ : c_ : d_ : e_ : f_ : g_ : h_ : xs) =
      let
        !a = a_
        !b = b_
        !c = c_
        !d = d_
        !e = e_
        !f = f_
        !g = g_
        !h = h_
      in let
        !(WithInt xs' r) = go xs
      in let
        !h' = r + h
        !g' = h' + g
        !f' = g' + f
        !e' = f' + e
        !d' = e' + d
        !c' = d' + c
        !b' = c' + b
        !a' = b' + a
      in WithInt (a' .: b' .: c' .: d' .: e' .: f' .: g' .: h' .: xs') a'
    go (!x:xs) = case go xs of WithInt xs' r -> let !r' = x + r in WithInt (r' .: xs') r'
  in
    case go xs of WithInt xs' _ -> xs'
{-# INLINE[1] scanr1ManualUF'FB #-}

scanr1ManualUF'BackaccsFB :: (Int -> a -> b) -> Int -> WithInt a -> WithInt b
scanr1ManualUF'BackaccsFB k x (WithInt xs ((+ x) -> !s)) = WithInt (k s xs) s
{-# INLINE[0] scanr1ManualUF'BackaccsFB #-}

{-# RULES "scanr1ManualU fuse" [~1] forall . scanr1ManualUF = \xs -> build \k z -> val (GHC.List.foldr (scanr1ManualUF'BackaccsFB k) (WithInt z 0) xs) #-}
{-# RULES "scanr1ManualU unfuse" [1] forall k z xs . val (GHC.List.foldr (scanr1ManualUF'BackaccsFB k) (WithInt z 0) xs) = scanr1ManualUF'FB k z xs #-}

----------------------------------------------

-- | by \@qnikst
qnikst3 :: [Int] -> [Int]
qnikst3 = V.toList. V.init . V.scanr' (+) 0 . V.fromList

-- | by \@kiber_soplya
sums_Of_Subsets :: [Int] -> [Int]
sums_Of_Subsets list = sums_list list []
  where sums_list original_list result_list | null original_list = result_list
        sums_list original_list result_list = sums_list (tail original_list) (result_list ++ [(sum original_list)])


-------------------------------------------

-- | by \@aadaa_fgtaa
simdy :: [Int] -> [Int]
simdy
  | sizeOf @Int 0 /= 8 = error "Please send a bug report to /dev/null"
  | otherwise          = unsafeDupablePerformIO . outer
  where

    outer :: [Int] -> IO [Int]
    outer xs = allocaBytesAligned (8 * chunkSize) 32 \ptr -> inner ptr 0 xs

    inner :: Ptr Int -> Int -> [Int] -> IO [Int]
    inner ptr idx xs | idx == chunkSize = postfixSumsRound ptr idx =<< outer xs
    inner ptr idx (e1:e2:e3:e4:e5:e6:e7:e8:xs) = do
      pokeElemOff ptr (idx + 0) e1
      pokeElemOff ptr (idx + 1) e2
      pokeElemOff ptr (idx + 2) e3
      pokeElemOff ptr (idx + 3) e4
      pokeElemOff ptr (idx + 4) e5
      pokeElemOff ptr (idx + 5) e6
      pokeElemOff ptr (idx + 6) e7
      pokeElemOff ptr (idx + 7) e8
      inner ptr (idx + 8) xs
    inner ptr idx xs = postfixSumsRound ptr idx $ scanr1 (+) xs

    chunkSize :: Int
    chunkSize = 16 * 1024

-- invariant: offset % 32 == 0
foreign import prim "postfix_sums_round" postfix_sums_round :: Addr# -> Int# -> Int# -> Any -> State# s -> (# State# s, Any #)

-- invariant: index % 4 == 0
postfixSumsRound :: Ptr Int -> Int -> [Int] -> IO [Int]
postfixSumsRound (Ptr addr) (I# size) list = IO (unsafeCoerce postfix_sums_round addr (size *# 8#) acc list)
  where
    !(I# acc) = case list of
      []  -> 0
      x:_ -> x

-------------------------------------------

-------------------------------------------

-- ** IMPLEMENTATIONS

implementations :: [(Code Q ([Int] -> [Int]), Code Q String)]
implementations =
  [ ( [|| scanrLazy ||]              , [|| "scanrLazy" ||] )
  , ( [|| scanrManual ||]            , [|| "scanrManual" ||] )
  , ( [|| scanrPrelude ||]           , [|| "scanrPrelude" ||] )
  , ( [|| scanr1Prelude ||]          , [|| "scanr1Prelude" ||])
  , ( [|| scanlPrelude ||]           , [|| "scanlPrelude" ||] )
  , ( [|| scanl'Prelude ||]          , [|| "scanl'Prelude" ||] )
  , ( [|| scanl'Manual ||]           , [|| "scanl'Manual" ||] )
  , ( [|| reverse'scanl''reverse ||] , [|| "reverse'scanl''reverse" ||] )
  , ( [|| obviousFunction ||]        , [|| "obviousFunction" ||] )
  -- comment this if it would be too slow
  , ( [|| foldl''reverse ||]         , [|| "foldl''reverse" ||] )
  , ( [|| foldrPrelude ||]           , [|| "foldrPrelude" ||] )
  , ( [|| tardisManual ||]           , [|| "tardisManual" ||] )
  , ( [|| backsums ||]               , [|| "backsums" ||] )
  , ( [|| backsumsStrict ||]         , [|| "backsumsStrict" ||] )
  , ( [|| backaccs ||]               , [|| "backaccs" ||] )
  , ( [|| scanl'ManualU ||]          , [|| "scanl'ManualU" ||] )
  , ( [|| scanr1ManualU ||]          , [|| "scanr1ManualU" ||] )
  , ( [|| backlist ||]               , [|| "backlist" ||] )
  , ( [|| revsums ||]                , [|| "revsums" ||] )
  , ( [|| qnikst ||]                 , [|| "qnikst" ||] )
  , ( [|| qnikst2 ||]                , [|| "qnikst2" ||] )
  , ( [|| scanl'ManualUF ||]         , [|| "scanl'ManualUF" ||] )
  , ( [|| scanr1ManualUF ||]         , [|| "scanr1ManualUF" ||] )
  , ( [|| qnikst3 ||]                , [|| "qnikst3" ||] )
  , ( [|| sums_Of_Subsets ||]        , [|| "sums_Of_Subsets" ||] )
  -- comment this if it would be too slow
  , ( [|| simdy ||]                  , [|| "simdy" ||] )
  ]
{-# INLINE implementations #-}
