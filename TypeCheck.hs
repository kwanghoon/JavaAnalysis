module TypeCheck where

import AST
import Data.Maybe
import Data.List
import Data.Either

objClass = "Object"
strClass = "String"
static   = "static"

type Info = ([Name], 
             [(Name,Name)], 
             [(Name, [(TypeName, Name, [Attrib])])],
             [(Name, Name, [TypeName], TypeName, [Attrib])],
             [(Name, Name, ArgDecls, Stmt)])
  
typecheck program = 
  do let info = initTypeCheck program :: Info 
     maybetc <- tcProgram info program
     let err = fromJust maybetc
     if isJust maybetc 
     then putStrLn err
     else do prTyInfo info 
             putStrLn "Successfully typechecked..."
     
getUserClasses (userClasses, inheritance, fields, mtype, mbody) = userClasses 
getInheritance (userClasses, inheritance, fields, mtype, mbody) = inheritance
getFields (userClasses, inheritance, fields, mtype, mbody) = fields
getMtype (userClasses, inheritance, fields, mtype, mbody) = mtype
getMbody (userClasses, inheritance, fields, mtype, mbody) = mbody

-- 0. Print type information
prTyInfo (userClasses, inheritance, fields, mtypes, mbodies) =
  do prUserClasses userClasses
     prUserClasses basicClasses
     prInheritance inheritance
     prInheritance basicInheritance
     prFields fields
     prFields basicFields
     prMtype mtypes
     prMtype basicMtypes
     prMbody mbodies

prUserClasses userClasses = 
  do putStrLn "Classes: "
     mapM_ putStrLn 
       $ map (\c -> " - " ++ c) $ userClasses
     putStrLn ""
     
prInheritance inheritance =     
  do putStrLn "Inheritance: "
     mapM_ putStrLn 
       $ map (\(f,s) -> " - " ++ f ++ " <: " ++ s) $ inheritance
     putStrLn ""
     
prFields fields =
  do putStrLn "Fields:"
     mapM_ putStrLn 
       $ map (\(c,fs) -> 
               " - " ++ c ++ " : " ++ 
               (concat $ intersperse ", " 
                $ map (\(t,v,attrs) 
                       -> (concat $ intersperse "/" attrs) ++ " " ++ 
                          show t ++ " " ++ v) fs)) $ fields
     putStrLn ""

prMtype mtype =
  do putStrLn "Method Types:"
     mapM_ putStrLn 
       $ map (\(c,m,targs, tret, attrs) ->
               " - " ++ c ++ "," ++ m ++ " : " ++ 
               (concat $ intersperse "/" attrs ++ [" "] ++
                ["("] ++ 
                (intersperse ", " $ map show $ targs) ++ [")"]) ++
               " --> " ++ show tret) mtype
     putStrLn ""

prMbody mbody =     
  do putStrLn "Method bodies:"
     putStrLn $ " - " ++ " *** skip printing *** "
     putStrLn ""

-- 1. Gather type information
initTypeCheck :: Program -> Info     
initTypeCheck cs = (userClasses, inheritance, fields, mtype, mbody)
  where
    userClasses = [ c | Class _ c _ _ _ <- cs ]
    inheritance = [ (c,d) | Class _ c (Just d) _ _ <- cs ] 
                  ++
                  [ (c,objClass) | Class _ c Nothing _ _ <- cs ]
                  ++
                  basicInheritance
    
    fields = [ (c, mkFields cs userClasses c) | c <- userClasses]
    mtype  = concat [ mkMtype cs userClasses c | c <- userClasses ]
    mbody  = concat [ mkMbody cs userClasses c | c <- userClasses ]
    
basicClasses = -- TODO: extension
  [ strClass,
    objClass,
    "HashSet",
    "Set",
    "Iterator"
  ]

basicInheritance =  -- TODO: extension
  [ (strClass, objClass),
    ("HashSet", "Set")
  ]
  
basicFields = -- TODO: extension
  [
    ("System", [(TypeName "PrintStream", "out", [static])])
  ]
  
basicMtypes = -- TODO: extension  
  [ 
    ("Set", "add", [TypeName "Object"], TypeName "boolean", []),
    ("Set", "remove", [TypeName "Object"], TypeName "boolean", []),
    ("Set", "iterator", [], TypeName "Iterator", []),
    
    ("HashSet", "add", [TypeName "Object"], TypeName "boolean", []),
    ("HashSet", "remove", [TypeName "Object"], TypeName "boolean", []),
    ("HashSet", "iterator", [], TypeName "Iterator", []),
    
    ("Iterator", "hasNext", [], TypeName "boolean", []),
    ("Iterator", "next", [], TypeName "Object", []),
    ("Iterator", "remove", [], TypeName "void", []),
    
    ("StringBuilder", "StringBuilder", [], TypeName "StringBuilder", []),
    ("StringBuilder", "append", [TypeName "String"], TypeName "StringBuilder", []),
    ("StringBuilder", "append", [TypeName "char"], TypeName "StringBuilder", []),
    ("StringBuilder", "setLength", [TypeName "int"], TypeName "void", []),
    ("StringBuilder", "toString", [], TypeName "String", []),
    
    ("String", "equals", [TypeName "String"], TypeName "boolean", []),

    ("PrintStream", "println", [TypeName "String"], TypeName "void", [static])
  ]

getClassDef cs c = 
  case [ def | def@(Class _ d _ _ _) <- cs, c == d ] of
    [def] -> [def]
    []    -> error $ "getClassDef: class definition not found " ++ c
    defs  -> error $ "getClassDef: multiple definitions for " ++ c

mkFields cs ucs c =
  if elem c ucs
     then mkFields' cs ucs c
     else mkFields'' cs ucs c     
          
mkFields' cs ucs c = 
  if c == objClass 
  then []
  else dxs ++ dxs'
  where
    [Class _ _ maybec maybeis mdecls] = getClassDef cs c
    dxs  = [ (d,x,attrs) | FieldDecl attrs d x maybei <- mdecls ]
    dxs' = if isJust maybec 
           then mkFields cs ucs (fromJust maybec)
           else []


mkFields'' cs ucs c = concat mfields 
  where
    mfields = [fs | (c', fs) <- basicFields, c == c']
  
  
mkMtype cs ucs c =
  if elem c ucs
     then mkMtype' cs ucs c 
     else mkMtype'' cs ucs c
     
mkMtype' cs ucs c = dxs ++ dxs'
  where
    [Class _ _ maybec maybeis mdecls] = getClassDef cs c
    dxs   = [(c, m, map fst args, d, attrs) 
            | MethodDecl attrs d m args _ <- mdecls] ++
            [(c, c, map fst args, TypeName dn, []) 
            | ConstrDecl dn args _ <- mdecls]
    dxs'  = [(c, m, tyns, d, attrs) 
            | (_, m, tyns, d, attrs) <- dxs'', notin m dxs]
    dxs'' = if isJust maybec 
            then mkMtype cs ucs (fromJust maybec)
            else []
    notin m dxs = and [ m /= m' | (_, m', _, _, _) <- dxs ]
    
mkMtype'' cs ucs c = mtypes
  where
    mtypes = [ (c, m,argtys, retty, attrs) 
             | (c', m, argtys, retty, attrs) <- basicMtypes, c == c']

mkMbody cs ucs c = dxs ++ dxs'
  where
    [Class _ _ maybec maybeis mdecls] = getClassDef cs c
    dxs   = [(c, m, args, s) | MethodDecl attrs d m args s <- mdecls] ++
            [(c, c, args, s) | ConstrDecl dn args s <- mdecls]
    dxs'  = [(c, m, args, s) | (_,m,args,s) <- dxs'', notin m dxs]
    dxs'' = if isJust maybec 
            then mkMbody cs ucs (fromJust maybec)
            else []
    notin m dxs = and [ m /= m' | (_, m', _, _) <- dxs ]

-- 2. Do typecheck
    
anyJust :: [Maybe String] -> Maybe String
anyJust maybes =
  case anyJust' maybes of
    [] -> Nothing
    l  -> Just (head l)
  where
    anyJust' [] = []
    anyJust' (Nothing : maybes) = anyJust' maybes
    anyJust' (Just x  : maybes) = x : anyJust' maybes
    
-- [Subtyping relationship]
-- 1. C <: D  if C=D or C <: C' and C' <:D
-- 2. C[]^i <: D[]^i if C <: D for i>=0    
-- 3. Object[]^i <: Object[]^j if j<=i for i,j>=0
--    
subType info (TypeName c) (TypeName d) = 
  c == "null" || 
  c == d || 
  or [ subType info (TypeName pc) (TypeName d) 
     | (cc,pc) <- inheritance, cc == c ]
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
  
--    
isLeft (Left _)  = True
isLeft (Right _) = False

isRight (Right _) = True
isRight (Left _)  = False

fromLeft (Left l) = l
fromLeft (Right r)  = error $ "fromLeft: Right" ++ show r

fromRight (Right r) = r
fromRight (Left l)  = error $ "fromLeft: Left" ++ show l

--
tcProgram info program = 
  do rs <- mapM (tcClass info) program
     return $ anyJust $ rs
     
tcClass info (Class attrs n p is mdecls) =
  do rs <- mapM (tcMdecl info (n, p, is)) mdecls
     return $ anyJust $ rs
     
tcMdecl info (c,p,is) (MethodDecl attrs retty m targs stmt) = 
  do let env = ((c, p, is, Just m), 
                ("this", TypeName c) : [(x,c) | (c, x) <- targs])
     either1 <- tcBeginStmt info env retty stmt
     let env1 = fromLeft either1
     if isRight either1
     then return $ Just $ "tcMdecl: exps " ++ fromRight either1
     else if isJust (lookupEnv env1 "return") == False && 
             eqType retty (TypeName "void") == False &&
             c /= m   -- TODO: This is not enough!
          then return $ Just $ "tcMdecl: missing return in the method " ++ 
                               m ++ " of class " ++ c
          else return $ Nothing
       
tcMdecl info (c,p,is) (ConstrDecl rettyn targs stmt) = -- TODO: it's own typing?
  do tcMdecl info (c,p,is) (MethodDecl [] (TypeName rettyn) c targs stmt)
    
tcMdecl info (c,p,is) (FieldDecl attrs t v maybei) = 
  if isNothing maybei
  then return $ Nothing
  else do let exp = fromJust maybei
          let env = ((c, p, is, Nothing), [])
          eithert <- tcExp info env exp
          let ty = fromLeft eithert
          if isRight eithert
             then return $ Just $ fromRight eithert
             else if subType info ty t == False
                     then return $ Just $ 
                          "tcMdecl: type error in the initializer: " ++
                          show (FieldDecl attrs t v maybei)
                     else return $ Nothing

tcExps info env [] = return (Left (TypeName "void"))
tcExps info env [exp] = tcExp info env exp
tcExps info env (exp:exps) = 
  do either <- tcExp info env exp
     if isRight either 
     then return either 
     else tcExps info env exps

--     
lookupEnv (ctx,tyenv) x =
  case [ t | (y,t) <- tyenv, x == y ] of
    []    -> Nothing
    (t:_) -> Just t

lookupFields info (TypeName c) f = lookupFields' info c f 
lookupFields info (ArrayTypeName c) f = 
  if f == "length"  -- The length field for arrays
  then Just $ (TypeName "int", [])
  else Nothing
  
lookupFields' info c f =
  let ucs = getUserClasses info in
  if elem c ucs
     then lookupFields'' info c f
     else lookupFields''' info c f
  
lookupFields'' info c f =
  let fields = getFields info in
  case [(e,attrs) 
       | (d,cfs) <- fields, c==d, (e,g,attrs) <- cfs, f==g] of
    []    -> Nothing
    (p:_) -> Just p
    
lookupFields''' info c f = 
  case [(e,attrs) 
       | (d,cfs) <- basicFields, c==d, (e,g,attrs) <- cfs, f==g] of  
    [] -> Nothing
    (p:_) -> Just p
    
lookupMtype info (TypeName c) m argtys = 
  lookupMtype' info c m argtys
lookupMtype info (ArrayTypeName c) m argtys = 
  Nothing  -- No method is available for arrays
  
lookupMtype' info c m argtys =
  let ucs = getUserClasses info in    
  if elem c ucs
     then lookupMtype'' info c m argtys
     else lookupMtype''' info c m argtys

lookupMtype'' info c m argtys =
  let mtypes = getMtype info in
  case [ (argtys', retty, attrs) 
       | (c', m', argtys', retty, attrs) <- mtypes, 
         c == c', 
         m == m',
         subTypes info argtys argtys'
       ] of
    []   -> Nothing
    [mt] -> Just mt
    mts  -> chooseMostSpecificMtype info mts
    
lookupMtype''' info c m argtys =
  case [ (argtys', retty, attrs) 
       | (c', m', argtys', retty, attrs) <- basicMtypes, 
         c == c', 
         m == m',
         subTypes info argtys argtys'
       ] of
    []   -> Nothing
    [mt] -> Just mt
    mts  -> chooseMostSpecificMtype info mts
    
chooseMostSpecificMtype info mtys =
  case mtys1 of
    []    -> Nothing -- Can't determine the most specific mtype
    [mty] -> Just mty
    mtys  -> Nothing -- Multiple is the most specific mtype
  where
    mtys1 = [ mty | (mty, mtys') <- 
                 [ (mtys !! i, take (i-1) mtys ++ drop (i+1) mtys) 
                 | i <- [0..length mtys-1]],  morespecificthan mty mtys' ]
    morespecificthan (argty, _, _) mtys =
      all (True==) [subTypes info argty argty' | (argty',_,_) <- mtys]
    
lookupKtype info (TypeName "int") argtys = Nothing
lookupKtype info (TypeName "char") argtys = Nothing
lookupKtype info (TypeName "double") argtys = Nothing
lookupKtype info (TypeName "byte") argtys = Nothing
lookupKtype info (TypeName "boolean") argtys = Nothing -- TODO: distinguish reference types
lookupKtype info (ArrayTypeName c) argtys = Just [TypeName "int"]
lookupKtype info (TypeName c) argtys = 
  let maybemtype = lookupMtype info (TypeName c) c argtys
      (argtys', retty, attrs) = fromJust maybemtype
  in  if isNothing maybemtype
      then Just []     -- no arguments are required for this constructor
      else Just argtys'

update (ctx,tyenv) x t = (ctx, (x,t) : [ (y,s) | (y,s) <- tyenv, x /= y ])

getCurrentClass ((c,p,is,m),tyenv) = c

getParentClass ((c,Just d,is,m),tyenv)  = d
getParentClass ((c,Nothing,is,m),tyenv) = "Object"

firstStmt (Expr e)           = Just (Expr e, [])
firstStmt (Ite cond s1 s2)   = Just (Ite cond s1 s2, [])
firstStmt (LocalVarDecl t x maybee) = Just (LocalVarDecl t x maybee, [])
firstStmt (Return maybee)    = Just (Return maybee, [])
firstStmt (Seq s1 s2) = 
  let (stmt1,therest) = fromJust $ firstStmt s1 -- TODO: Make sure that s1 contains no NoStmt
  in  Just (stmt1, [toStmt $ therest ++ [s1]])
firstStmt (NoStmt) = Nothing

isSuperCall (Expr (Prim "super" es)) = True
isSuperCall _                        = False

--
tcExp info env (Var x) =
  do let maybet = lookupEnv env x
     if isNothing maybet
       then do eithert <- tcExp info env (Field (Var "this") x) 
               if isRight eithert 
                 then return $ Right $ "tcExp: variable " ++ x ++ " not found"
                 else return eithert
       else return $ Left  $ fromJust maybet

tcExp info env (Field e f) = 
  do eitherty <- tcExp info env e
     let c = fromLeft eitherty
     let maybed = lookupFields info c f
     let (d,attrs) = fromJust maybed
     if isRight eitherty 
       then return $ eitherty
       else if isNothing maybed 
            then return $ Right $ "tcExp: no such field found: " ++ 
                 show (Field e f)
            else return $ Left $ d
                 
tcExp info env (StaticField c f) = 
  do let maybed = lookupFields info c f
     let (d,attrs) = fromJust maybed
     if isNothing maybed
        then return $ Right $ "tcExp: no such field found: " ++ 
             show (StaticField c f)
        else if elem static attrs == False 
             then return $ Right $ "tcExp: not static field: " ++
                  show (StaticField c f)
             else return $ Left $ d
                 
tcExp info env (New t es) = 
  do eitherts <- mapM (tcExp info env) es
     let (ls,rs) = partitionEithers $ eitherts
     let argts1 = ls
     let maybektype = lookupKtype info t argts1
     let argts2 = fromJust maybektype
     if null rs == False 
       then return $ Right $ head $ rs 
       else if isNothing maybektype 
               then return $ Right $ "tcExp: invalid type constructor: " ++
                    show (New t es)
               else return $ Left $ t
            
tcExp info env (Assign lhs e) =  -- Note the parser ensure that lhs is legal.
  do eitherlhsty <- tcExp info env lhs
     let lhsty = fromLeft eitherlhsty
     eitherty <- tcExp info env e
     let ty = fromLeft eitherty
     if isRight eitherlhsty
       then return $ eitherlhsty
       else if isRight eitherty
               then return eitherty 
               else if subType info ty lhsty == False
                       then return $ Right $ "tcExp: type mismatch in " ++ 
                            show (Assign lhs e)   
                       else return $ Left $ TypeName "void"
            
tcExp info env (Cast tn e) =            
  do eithert <- tcExp info env e
     let t = fromLeft eithert
     if isRight eithert 
       then return eithert 
       else if subType info tn t == False && subType info t tn == False 
            then return $ Right $ 
                 "tcExp: type cast error for " ++ show (Cast tn e)
            else return $ Left $ tn
                 
tcExp info env (Invoke e m es) = 
  do eithert  <- tcExp info env e
     eitherts <- mapM (tcExp info env) es
     let t = fromLeft eithert
     let ts = map fromLeft eitherts
     let maybemtype = lookupMtype info t m ts
     let (argts, rett, attrs) = fromJust maybemtype
         
     if isRight eithert 
       then return $ Right $ fromRight eithert ++ " in " ++ show (Invoke e m es)
       else if (all (True==) $ map isLeft $ eitherts) == False
               then return $ head $
                    [eithert' | eithert' <- eitherts, isRight eithert']
               else if isNothing maybemtype 
                    then return $ Right $ "tcExp: method not found: " ++ m ++
                         show ts ++ " in " ++ show t ++ 
                         ": " ++ show (Invoke e m es)
                    else return $ Left $ rett
                           
tcExp info env (StaticInvoke c m es) = 
  do eitherts <- mapM (tcExp info env) es
     let ts = map fromLeft eitherts
     let maybemtype = lookupMtype info c m ts
     let (argts, rett, attrs) = fromJust maybemtype
         
     if isNothing maybemtype 
       then return $ Right $ "tcExp: method not found: " ++ m 
       else if (all (True==) $ map isLeft $ eitherts) == False
               then return $ head $ 
                    [eithert' | eithert' <- eitherts, isRight eithert']
               else if elem static attrs == False
                       then return $ Right $ "tcExp: not static method: " ++
                            show (StaticInvoke c m es) 
                       else return $ Left $ rett
                           
tcExp info env (ConstTrue)  = return $ Left $ TypeName "boolean"
tcExp info env (ConstFalse) = return $ Left $ TypeName "boolean"
tcExp info env (ConstNull) = return $ Left $ TypeName "null" -- TODO: null type
tcExp info env (ConstNum n) = return $ Left $ TypeName "int"
tcExp info env (ConstLit s) = return $ Left $ TypeName "String"
tcExp info env (ConstChar s) = return $ Left $ TypeName "char"
tcExp info env (Prim "[]" [e1,e2]) = 
  do eithert1 <- tcExp info env e1
     eithert2 <- tcExp info env e2
     let t1 = fromLeft eithert1
     let t2 = fromLeft eithert2
     if isRight eithert1
     then return eithert1
     else if isRight eithert2
          then return eithert2
          else if isArray t1 == False || isInt t2 == False
               then return $ Right $ "tcExp: type mismatch in array indexing: " 
                           ++ show (Prim "[]" [e1,e2])
               else return $ Left  $ elemType $ t1

tcExp info env (Prim "[]=" [e1, e2]) = 
  do eithert1 <- tcExp info env e1
     eithert2 <- tcExp info env e2
     let t1 = fromLeft eithert1
     let t2 = fromLeft eithert2
     if isRight eithert1
        then return eithert1
        else if isRight eithert2
                then return eithert2 
                else if subType info t2 t1 == False
                        then return $ Right $ "tcExp: not assignable type: " ++ 
                             show (Prim "[]=" [e1, e2]) 
                        else return $ Left $ TypeName $ "void"
        
tcExp info env (Prim "super" es) = 
  do return $ Right $ "tcExp: misplaced super call: " ++ show (Prim "super" es)

tcExp info env (Prim n es) = 
  if elem n ["==", "!="] == False
     then tcExp' info env (Prim n es)
     else tcExp'' info env (Prim n es)     
  
tcExp' info env (Prim n es) =  
  do eitherts <- mapM (tcExp info env) es
     let (ls,rs) = partitionEithers $ eitherts
     let mtypes = [(argts,rett) | (p, argts, rett) <- primTypeTable, p == n]
     let retts  = [rett | (argts, rett) <- mtypes, length ls == length argts
                        , all (True==) [eqType t1 t2 | (t1,t2) <- zip ls argts]]
     if null rs == False
     then return $ Right $ head $ rs
     else if null retts
          then return $ Right $ "tcExp: type mismatch in " ++ show (Prim n es)
          else return $ Left  $ head $ retts

tcExp'' info env (Prim n [e1,e2]) = -- ==, !=
  do eitherts <- mapM (tcExp info env) [e1,e2]
     let (ls,rs) = partitionEithers $ eitherts
     let [ty1,ty2] = map fromLeft eitherts
     if null rs == False
     then return $ Right $ head $ rs
     else if (subType info ty1 ty2 || subType info ty2 ty1) == False
          then return $ Right $ "tcExp: type mismatch in " ++ 
                                show (Prim n [e1,e2])
          else return $ Left  $ TypeName "boolean"


-- tcExp info env es = error ("Missing: " ++ show es)

tcBeginStmt info env retty stmt = 
  do let maybe = firstStmt stmt
     let (stmt1, therest) = fromJust maybe
     if isJust maybe == False || isSuperCall stmt1 == False
     then tcStmt info env retty stmt
     else do either1 <- tcSuperCall info env stmt1
             let env1 = fromLeft either1
             if null therest || isRight either1
             then return either1
             else tcStmt info env1 retty (head therest)

tcSuperCall info env (Expr (Prim "super" es)) =
  do eithertys <- mapM (tcExp info env) es
     let (argtys2,rs) = partitionEithers $ eithertys
     let p          = getParentClass env
     let maybektype = lookupKtype info (TypeName p) argtys2
     let argtys1    = fromJust maybektype
     if null rs == False
        then return $ Right $ head $ rs
        else if isNothing maybektype
                then return $ Right $ "tcExp: invalid super call: " ++ 
                     show (Prim "super" es)
                else return $ Left $ env
        
tcSuperCall info env stmt =
  return $ Right $ "tcSuperCall: unexpected statement: " ++ show stmt

tcStmt info env retty (Expr e) =
  do eithert <- tcExp info env e
     let err = fromRight eithert
     if isRight eithert 
     then return $ Right $ err
     else return $ Left  $ env
    
tcStmt info env retty (Ite cond s1 s2) =
  do eithercondt <- tcExp info env cond
     either1     <- tcStmt info env retty s1
     either2     <- tcStmt info env retty s2
     let condt = fromLeft  eithercondt
     let err   = fromRight eithercondt
     let env1  = fromLeft either1
     let env2  = fromLeft either2
     let env'  = if isJust (lookupEnv env1 "return") && 
                    isJust (lookupEnv env2 "return")
                 then update env "return" retty
                 else env
         
     if isRight eithercondt 
     then return (Right err)
     else if isRight either1
          then return either1
          else if isRight either2
               then return either2 
               else if isBoolean condt == False
                    then return $ Right $ "tcExp: not boolean type for " ++
                                show (Ite cond s1 s2)
                    else return $ Left  $ env'

tcStmt info env retty (LocalVarDecl tn x maybee) = 
  do let e = fromJust maybee
     let env' = update env x tn 
     if isNothing maybee 
     then return $ Left $ env'
     else do eithert <- tcExp info env e
             let t = fromLeft eithert
             if isRight eithert 
             then return $ Right $ fromRight eithert 
             else if subType info t tn == False
                   then return $ Right $ "tcExp: type mismatch for " ++ x ++
                               " in " ++ show e
                   else return $ Left  $ env'

tcStmt info env retty (Return Nothing) = 
  do let env1 = update env "return" (TypeName "void")
     if eqType retty (TypeName "void") == False 
     then return $ Right $ "tcStmt: return type is not void: " ++ show retty
     else return $ Left $ env1
     
tcStmt info env retty (Return (Just e)) = 
  do eithert <- tcExp info env e
     let err  = fromRight eithert
     let t    = fromLeft eithert
     let env1 = update env "return" t
     if isRight eithert 
     then return $ Right $ err   
     else if subType info t retty == False
          then return $ Right $ "tcStmt: return type mismatch for " ++ 
                      show (Return (Just e))
          else return $ Left  $ env1

tcStmt info env retty (Seq s1 s2) =
  do either1 <- tcStmt info env retty s1
     let env1 = fromLeft either1
     if isRight either1
     then return $ either1
     else if isJust (lookupEnv env1 "return") 
          then return $ Right $ "tcStmt: dead code after " ++ show s1
          else tcStmt info env1 retty s2

tcStmt info env retty (NoStmt) = return $ Left $ env

tcStmt info env retty (For maybety x init cond upd s) =
  do let ty = fromJust maybety
     let env' = if isJust maybety then update env x ty else env
     eitherinit <- tcExp info env' (Assign (Var x) init)
     eitherbool <- tcExp info env' cond
     eitherupd  <- tcExp info env' upd
     eitherfor  <- tcStmt info env' retty s
     if isRight eitherinit
        then return $ Right $ fromRight eitherinit ++ " in " ++ show (Assign (Var x) init)
        else if isRight eitherbool 
                then return $ Right $ fromRight $ eitherbool
                else if eqType (TypeName "boolean") (fromLeft eitherbool) == False
                        then return $ Right $ "tcstmt: type  mismatch in " ++ show cond
                        else if isRight eitherupd
                                then return $ Right $ fromRight $ eitherupd
                                else if isRight eitherfor
                                        then return eitherfor
                                        else return $ Left $ env -- Restore the environment

tcStmt info env retty (While e s) = 
  do eitherty <- tcExp info env e
     let ty = fromLeft eitherty
     eitherenv <- tcStmt info env retty s
     if isRight eitherty
        then return $ Right $ fromRight eitherty
        else if isRight eitherenv 
             then return $ eitherenv 
             else return $ Left $ env  -- Restore the environment

tcStmt info env retty (Block s) = 
  do eitherenv <- tcStmt info env retty s
     let env' = 
           if isJust (lookupEnv (fromLeft eitherenv) "return")
           then update env "return" retty
           else env
     if isRight eitherenv 
        then return eitherenv 
        else return $ Left $ env'  -- Restore the environment

--
isBoolean (TypeName "boolean") = True
isBoolean _ = False

isInt (TypeName "int") = True
isInt _ = False

isArray (ArrayTypeName _) = True
isArray ( _) = False

elemType (ArrayTypeName c) = c
elemType _ = error "elemType: called with non-array type"

eqType (TypeName t) (TypeName s) = t == s
eqType (ArrayTypeName t) (ArrayTypeName s) = eqType t s
eqType _ _ = False 

primTypeTable = 
  [ ("primStartActivity", [TypeName "Intent"], TypeName "void"),
    ("primAddButton", [TypeName "int"], TypeName "void"),
    ("<", [TypeName "int", TypeName "int"], TypeName "boolean"),
    ("++", [TypeName "int", TypeName "int"], TypeName "int"),
    ("--", [TypeName "int", TypeName "int"], TypeName "int")
  ]
               
     
