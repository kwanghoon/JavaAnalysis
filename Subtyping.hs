module Subtyping where

import AST
import Library

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
  
