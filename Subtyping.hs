module Subtyping where

import AST
import Library
import Data.List
import Control.Monad.Trans (liftIO)
import Control.Monad.IO.Class

--
-- [Subtyping relationship]
-- 1. C <: D  if C=D or C <: C' and C' <:D
-- 2. C[]^i <: D[]^i if C <: D for i>=0    
-- 3. Object[]^i <: Object[]^j if j<=i for i,j>=0
--    
subType info (TypeName c) (TypeName d) = 
  c == "null" || 
  c == d || 
  or [ subType info (TypeName pc) (TypeName d) 
     | (cc,pc) <- inheritance, cc == c ] || 
  or [ subType info (TypeName pc) (TypeName d) 
     | (cc,pc) <- basicInheritance, cc == c ]
  where
    inheritance = getInheritance info
subType info (ArrayTypeName c) (ArrayTypeName d) = subType info c d
subType info (ArrayTypeName c) (TypeName d) =
  d == "Object" && isObjectMultiDimArray (ArrayTypeName c)
subType info _ _ = error "subType: Unsupported"   

subTypes info tys1 tys2 =
  length tys1 == length tys2 &&
  all (True==) [subType info t1 t2 | (t1,t2) <- zip tys1 tys2]

isObjectMultiDimArray (ArrayTypeName d) = isObjectMultiDimArray d
isObjectMultiDimArray (TypeName c) = c == "Object"
    

subMtype info (targs1, tret1) (targs2, tret2) =
  length targs1 == length targs2 &&
  all (True==) [subType info t1 t2 | (t1,t2) <- zip targs2 targs1] &&
  subType info tret1 tret2
  
chooseMostSpecificMtype :: Eq a => Info -> [(ClassName, [TypeName], TypeName, a)] 
                           -> Maybe (ClassName, [TypeName], TypeName, a)
chooseMostSpecificMtype info mtys =
  case mtys1 of
    []    -> Nothing -- Can't determine the most specific mtype
    [mty] -> Just mty
    mtys  -> returnMtypeIfAllAreTheSame info mtys
  where
    mtys1 = [ mty | (mty, mtys') <- 
                 [ (mtys !! (i-1), take (i-1) mtys ++ drop i mtys) 
                 | i <- [1..length mtys]],  morespecificthan mty mtys' ]
    morespecificthan (_, argty, _, _) mtys =
      all (True==) [subTypes info argty argty' | (_, argty',_,_) <- mtys]

-- Multiple is the most specific mtype
returnMtypeIfAllAreTheSame info mtys =  -- mtys is not empty.
  case nub mtys of
    [mty] -> Just mty
    mtys' -> chooseOverridingMtype info mtys'
    
chooseOverridingMtype info mtys = -- mtys is not empty and has more than two elements.
  case 
    [ mty1
    | mty1@(c1,atys1,aty1,extra1) <- mtys
    , and [ subType info (TypeName c1) (TypeName c2) 
          | mty2@(c2,atys2,aty2,extra2) <- mtys, mty1/=mty2 ] ] of
    
    [mty] -> Just mty
    _     -> Nothing 
             -- error ("chooseOverrideingMtype: possibly multiple inheritance problem: "
             --        ++ show [ (c,atys,aty) | (c,atys,aty,_) <- mtys])


-- For debuggin
-- _chooseMostSpecificMtype :: (Control.Monad.IO.Class.MonadIO m, Eq a, Show a) => Info -> [(ClassName, [TypeName], TypeName, a)] 
--                            -> m (Maybe (ClassName, [TypeName], TypeName, a))
-- _chooseMostSpecificMtype info mtys =
--   case mtys1 of
--     []    -> do liftIO $ putStrLn $ "_chooseMostSpecificMtype: Nothing"
--                 liftIO $ putStrLn $ show $ mtys1 
--                 return Nothing -- Can't determine the most specific mtype
--     [mty] -> return (Just mty)
--     mtys'  -> do liftIO $ putStrLn $ "_chooseMostSpecificMtype: multiple"
--                  liftIO $ putStrLn $ show $ mtys
--                  liftIO $ putStrLn $ show $ mtys' 
--                  liftIO $ putStrLn $ show $ mtypairs
--                  return Nothing -- Can't determine the most specific mtype
--                  _returnMtypeIfAllAreTheSame info mtys'
--   where
--     mtypairs = [ (mtys !! i, take (i-1) mtys ++ drop (i+1) mtys) 
--                  | i <- [0..length mtys-1] ]
--     mtys1 = [ mty | (mty, mtys') <- mtypairs,  morespecificthan mty mtys' ]
--     morespecificthan (_, argty, _, _) mtys =
--       and [subTypes info argty argty' | (_, argty',_,_) <- mtys]

-- _returnMtypeIfAllAreTheSame :: (Control.Monad.IO.Class.MonadIO m, Eq a, Show a) => Info -> [(ClassName, [TypeName], TypeName, a)] -> m (Maybe (ClassName, [TypeName], TypeName, a))
-- _returnMtypeIfAllAreTheSame info mtys =  -- mtys is not empty.
--   case nub mtys of
--     [mty] -> return (Just mty)
--     mtys' -> _chooseOverridingMtype info mtys'
    
-- _chooseOverridingMtype :: (Control.Monad.IO.Class.MonadIO m, Eq a, Show a) => Info -> [(ClassName, [TypeName], TypeName, a)] -> m (Maybe (ClassName, [TypeName], TypeName, a))
-- _chooseOverridingMtype info mtys = -- mtys is not empty and has more than two elements.
--   case 
--     [ mty1
--     | mty1@(c1,atys1,aty1,extra1) <- mtys
--     , and [ subType info (TypeName c1) (TypeName c2) 
--           | mty2@(c2,atys2,aty2,extra2) <- mtys, mty1/=mty2 ] ] of
    
--     [mty] -> return (Just mty)
--     mtys1-> do liftIO $ putStrLn $ "_chooseOverridingMtype: Nothing"
--                liftIO $ putStrLn $ show $ mtys1 
--                return Nothing 
--              -- error ("chooseOverrideingMtype: possibly multiple inheritance problem: "
--              --        ++ show [ (c,atys,aty) | (c,atys,aty,_) <- mtys])
