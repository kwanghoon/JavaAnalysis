module TypeCheck where

import AST
import Library
import Data.Maybe
import Data.List
import Data.Either
import Control.Monad.Error

--
type TypingEnv = [(Name, TypeName)]
type TypingCtx = (ClassName, Maybe ClassName, [ClassName], Maybe MethodName)
type TCError = String

--
typecheck :: Program -> IO (Maybe (Info, Program))
typecheck program = 
  do let info = initTypeCheck program
     eitherprogram <- runErrorT (tcProgram info program)
     returnResult info eitherprogram

returnResult info (Left err) = do
  putStrLn err
  return Nothing
returnResult info (Right program) = do  
  prTyInfo info
  putStrLn "Successfully typechecked..."
  return $ Just (info, program)

-- 0. Print type information
prTyInfo (userClasses, inheritance, fields, mtypes) =
  do prUserClasses userClasses
     prUserClasses basicClasses
     prInheritance inheritance
     prInheritance basicInheritance
     prFields fields
     prFields basicFields
     prMtypes mtypes
     prMtypes basicMtypes

prUserClasses userClasses = 
  do putStrLn "Classes: "
     mapM_ putStrLn 
       $ map (\(c,attrs) -> 
           " - " ++ (concat $ intersperse ", " $ attrs) ++ " " ++ c) userClasses
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

prMtypes mtypes =
  do putStrLn "Method Types:"
     mapM_ putStrLn 
       $ map (\(c, m, id, targs, tret, attrs, args, maybestmt) ->
               " - " ++ c ++ "," ++ m ++ " : " ++ 
               (concat $ intersperse "/" attrs ++ [" "] ++
                ["("] ++ 
                (intersperse ", " $ map show $ targs) ++ [")"]) ++
               " --> " ++ show tret) mtypes
     putStrLn ""

-- 1. Gather type information
initTypeCheck :: Program -> Info
initTypeCheck cs = (userClasses, inheritance, fields, mtypes)
  where
    userClasses = map fclass cs 
    fclass (Class attrs c _ _ _) = (c, [java_class])
    fclass (Interface c _ _)     = (c, [java_interface])
    
    inheritance = concat $ map finherit cs 
    finherit (Class _ c maybepc pis _) = [ (c, fpcorobj maybepc) ] ++ 
                                         [ (c,pi) | pi <- pis ]
    finherit (Interface c [] _)  = [(c, objClass)]
    finherit (Interface c pis _) = [(c, pi) | pi <- pis]
    
    fpcorobj (Just c)  = c
    fpcorobj (Nothing) = objClass
    
    fields = [ (c, mkFields cs userClasses c) | (c, _) <- userClasses]
    mtypes = concat [ mkMtype cs userClasses c | (c, _) <- userClasses]


getClassDef cs c = 
  let fname (Class _ d _ _ _) = d
      fname (Interface d _ _) = d
  in
  case [ def | def <- cs, c == fname def ] of
    [def] -> def
    []    -> error $ "getClassDef: class definition not found " ++ c
    defs  -> error $ "getClassDef: multiple definitions for " ++ c

mkFields cs ucs c =
  if isUserClass c ucs || isUserInterface c ucs
     then mkFields' cs ucs c
     else mkFields'' cs ucs c     
          
mkFields' cs ucs c = 
  if c == objClass 
  then []
  else dxs
  where
    (maybec, is, mdecls) =
      case getClassDef cs c of
        Class _ _ maybec is mdecls -> (maybec, is, mdecls)
        Interface _ is mdecls -> (Nothing, is, mdecls)
        
    dxs  = [ (d,x,attrs) | FieldDecl attrs d x maybei <- mdecls ]
    dxs1 = if isJust maybec 
           then mkFields cs ucs (fromJust maybec)
           else []
    dxs2 = concat [mkFields cs ucs i | i <- is]


mkFields'' cs ucs c = concat mfields 
  where
    mfields = [fs | (c', fs) <- basicFields, c == c']
  
  
mkMtype cs ucs c =
  if isUserClass c ucs || isUserInterface c ucs
     then mkMtype' cs ucs c
     else mkMtype'' cs ucs c
     
mkMtype' cs ucs c = dxs
  where
    (maybec, is, mdecls) =
      case getClassDef cs c of
        Class _ _ maybec is mdecls -> (maybec, is, mdecls)
        Interface _ is mdecls -> (Nothing, is, mdecls)

    dxs   = concat [ fmdecl mdecl | mdecl <- mdecls]
    
    fst3 (f,s,t) = f
    snd3 (f,s,t) = s
            
    fmdecl (MethodDecl attrs d m id args s) = 
      [(c, m, id, map fst3 args, d, attrs, map snd3 args, Just s)]
    fmdecl (ConstrDecl dn id args s)        = 
      [(c, c, id, map fst3 args, TypeName dn, [], map snd3 args, Just s)]
    fmdecl (AbstractMethodDecl d m id args) = 
      [(c, m, id, map fst3 args, d, [abstract], map snd3 args, Nothing)]
    fmdecl (FieldDecl _ _ _ _)              = []
    
mkMtype'' cs ucs c = mtypes
  where
    mtypes = [ (c, m, id, argtys, retty, attrs, args, maybestmt) 
             | (c', m, id, argtys, retty, attrs, args, maybestmt) <- basicMtypes
             , c == c' ]

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
  
--
tcProgram :: Info -> Program -> ErrorT TCError IO Program
tcProgram info program = 
  do program1 <- mapM (tcClass info) program
     return $ program1
     
-- tcClass :: Info -> Class -> IO (Maybe String)
tcClass :: Info -> Class -> ErrorT TCError IO Class
tcClass info (Class attrs n p is mdecls) =
  do mdecls1 <- mapM (tcMdecl info (n, p, is)) mdecls
     return $ (Class attrs n p is mdecls1)
tcClass info (Interface n is mdecls) =
  -- TODO: all abstract?
  do mdecls1 <- mapM (tcMdecl info (n, Nothing, is)) mdecls 
     return $ (Interface n is mdecls1)

-- tcMdecl :: Info -> ClassInfo -> MemberDecl -> IO (Maybe String)
tcMdecl :: Info -> ClassInfo -> MemberDecl -> ErrorT TCError IO MemberDecl
tcMdecl info (c,p,is) (AbstractMethodDecl retty m id targs) = do
  return (AbstractMethodDecl retty m id targs)
                       
tcMdecl info (c,p,is) (MethodDecl attrs retty m id targs stmt) = do
  let ctx = (c, p, is, Just m)
  let env = ("this", TypeName c) : [(x,ty) | (ty, x, _) <- targs]
  (env1, stmt1) <- tcBeginStmt info ctx env retty stmt
  if isJust (lookupEnv env1 "return") == False &&
     eqType retty (TypeName "void") == False &&
     c /= m   -- TODO: This is not enough!
    then throwError ("tcMdecl: missing return in the method " ++ 
                     m ++ " of class " ++ c)
    else return $ (MethodDecl attrs retty m id targs stmt1)
       
tcMdecl info (c,p,is) (ConstrDecl rettyn id targs stmt) = do 
  let ctx = (c,p,is,Just c)
  let env = ("this", TypeName c) : [(x,ty) | (ty, x, _) <- targs]
  (env1, stmt1) <- tcBeginStmt info ctx env (TypeName rettyn) stmt
  if (rettyn == c) == False
     then throwError ("tcMdecl: a wrong constructor name " ++ rettyn ++ 
                      " in " ++ c)
     else return (ConstrDecl rettyn id targs stmt1)     
    
tcMdecl info (c,p,is) (FieldDecl attrs t v maybei) =
  if isNothing maybei
  then do return $ (FieldDecl attrs t v maybei)
  else do 
    let initexp = fromJust maybei
    let env = []
    (ty, expr) <- tcExp info env initexp
    if subType info ty t == False
       then throwError ("tcMdecl: type error in the initializer: " ++
                        show (FieldDecl attrs t v maybei))
       else return $ (FieldDecl attrs t v (Just expr))
                          
--     
lookupEnv tyenv x =
  case [ t | (y,t) <- tyenv, x == y ] of
    []    -> Nothing
    (t:_) -> Just t

lookupFields info (TypeName c) f = lookupFields' info c f 
lookupFields info (ArrayTypeName c) f = 
  if f == "length"  -- The length field for arrays
  then Just $ (TypeName "int", [])
  else Nothing
  
lookupFields' info c f =
  if isUserClass c (getUserClasses info) || 
     isUserInterface c (getUserClasses info)
     then lookupFields'' info c f (getFields info) (getInheritance info)
     else lookupFields'' info c f basicFields basicInheritance
  
lookupFields'' info c f fields inheritance =
  case [(e,attrs) 
       | (d,cfs) <- fields, c==d, (e,g,attrs) <- cfs, f==g] of
    []    -> lookupFields''' info c f inheritance
    (p:_) -> Just p
    
lookupFields''' info c f inheritance =
  case filter isJust 
       [ lookupFields' info c2 f | (c1,c2) <- inheritance, c==c1 ]
  of 
    []  -> Nothing -- No such field
    [f] -> f
    fs  -> Nothing -- Duplicate field
    
lookupMtype info (TypeName c) m argtys = 
  lookupMtype' info c m argtys
lookupMtype info (ArrayTypeName c) m argtys = 
  Nothing  -- No method is available for arrays
  
lookupMtype' info c m argtys =
  if isUserClass c (getUserClasses info) || 
     isUserInterface c (getUserClasses info)
     then lookupMtype'' info c m argtys (getMtype info) (getInheritance info)
     else lookupMtype'' info c m argtys basicMtypes basicInheritance

lookupMtype'' info c m argtys mtypes inheritance =
  case [ (argtys', retty, attrs) 
       | (c', m', id', argtys', retty, attrs, _, _) <- mtypes, 
         c == c', 
         m == m',
         subTypes info argtys argtys'
       ] of
    []   -> lookupMtype''' info c m argtys inheritance
    [mt] -> Just mt
    mts  -> chooseMostSpecificMtype info mts
    
lookupMtype''' info c m argtys inheritance =
  case filter isJust 
       [ lookupMtype' info c2 m argtys | (c1,c2) <- inheritance, c==c1 ]
  of 
    []   -> Nothing -- No such field
    [mt] -> mt
    mts  -> chooseMostSpecificMtype info (map fromJust mts)
    
    
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
  let maybemtype = lookupKtype' info c c argtys
      (argtys', retty, attrs) = fromJust maybemtype
  in  if isNothing maybemtype
      then Just []     -- no arguments are required for this constructor
      else Just argtys'
           
lookupKtype' info c m argtys =
  if isUserClass c (getUserClasses info) || 
     isUserInterface c (getUserClasses info)
     then lookupKtype'' info c m argtys (getMtype info) (getInheritance info)
     else lookupKtype'' info c m argtys basicMtypes basicInheritance

lookupKtype'' info c m argtys mtypes inheritance =
  case [ (argtys', retty, attrs) 
       | (c', m', id', argtys', retty, attrs, _, _) <- mtypes, 
         c == c', 
         m == m',
         subTypes info argtys argtys'
       ] of
    []   -> Nothing
    [mt] -> Just mt
    mts  -> chooseMostSpecificMtype info mts
           

update tyenv x t = (x,t) : [ (y,s) | (y,s) <- tyenv, x /= y ]

getCurrentClass ((c,p,is,m),tyenv) = c

getParentClass (c,Just d,is,m)  = d
getParentClass (c,Nothing,is,m) = "Object"

firstStmt (Expr e)                    = Just (Expr e, [])
firstStmt (Ite cond s1 s2)            = Just (Ite cond s1 s2, [])
firstStmt (LocalVarDecl t x n maybee) = Just (LocalVarDecl t x n maybee, [])
firstStmt (Return maybee)             = Just (Return maybee, [])
firstStmt (Seq s1 s2) = 
  let (stmt1,therest) = fromJust $ firstStmt s1 -- TODO: Make sure that s1 contains no NoStmt
  in  Just (stmt1, [toStmt $ therest ++ [s1]])
firstStmt (NoStmt) = Nothing

isSuperCall (Expr (Prim "super" es)) = True
isSuperCall _                        = False

--
tcExp :: Info -> TypingEnv -> Expr -> ErrorT TCError IO (TypeName, Expr)
tcExp info env (Var x) = do
  let maybet = lookupEnv env x
  if isJust maybet
     then return $ (fromJust maybet, Var x) 
     else do (ty,expr) <- tcExp info env (Field (Var "this") x Nothing) 
                               `catchError` handler x
             return (ty, expr)
  where
    handler x s = throwError $ "tcExp: variable " ++ x ++ " not found"

tcExp info env (Field e f maybety) = 
  do (ty,expr) <- tcExp info env e
     let maybed = lookupFields info ty f
     let (d,attrs) = fromJust maybed
     if isNothing maybed 
        then throwError ("tcExp: no such field found: " ++ 
                         show (Field e f maybety))
        else return (d, Field expr f (Just d))
                 
tcExp info env (StaticField c f maybety) = 
  do let maybed = lookupFields info c f
     let (d,attrs) = fromJust maybed
     if isNothing maybed
        then throwError ("tcExp: no such field found: " ++ 
                         show (StaticField c f maybety))
        else if elem static attrs == False 
             then throwError ("tcExp: not static field: " ++ 
                              show (StaticField c f maybety))
             else return (d, StaticField c f (Just d))
                 
tcExp info env (New t es label) = 
  do tyexprs <- mapM (tcExp info env) es
     let (tys,exprs) = unzip tyexprs
     let maybektype = lookupKtype info t tys
     if isNothing maybektype 
        then throwError ("tcExp: invalid type constructor: " ++
                         show (New t es label))
        else return $ (t, New t exprs label)
            
tcExp info env (Assign lhs e) =  -- Note the parser ensures that lhs is legal.
  do (lhsty,lhsexpr) <- tcExp info env lhs
     (ty,   expr)    <- tcExp info env e
     if subType info ty lhsty == False
       then throwError ("tcExp: type mismatch in " ++ show (Assign lhs e))
       else return $ (TypeName "void", Assign lhsexpr expr)
            
tcExp info env (Cast tn e) =            
  do (ty, expr) <- tcExp info env e
     if subType info tn ty == False && subType info ty tn == False 
       then throwError ("tcExp: type cast error for " ++ show (Cast tn e))
       else return $ (tn, Cast tn expr)
                 
tcExp info env (Invoke e m es tyann) = 
  do (ty, expr) <- tcExp info env e
     tyexprs    <- mapM (tcExp info env) es
     let (tys, exprs) = unzip tyexprs
     let maybemtype = lookupMtype info ty m tys
     let (argts, retty, attrs) = fromJust maybemtype
         
     if isNothing maybemtype 
       then throwError ("tcExp: method not found: " ++ m ++
                        show tys ++ " in " ++ show ty ++
                        ": " ++ show (Invoke e m es tyann))
       else return $ (retty, Invoke expr m exprs (Just retty))
                           
tcExp info env (StaticInvoke c m es tyann) = 
  do tyexprs <- mapM (tcExp info env) es
     let (tys,exprs) = unzip tyexprs
     let maybemtype = lookupMtype info c m tys
     let (argts, retty, attrs) = fromJust maybemtype
         
     if elem static attrs == False
       then throwError ("tcExp: not static method: " ++
                        show (StaticInvoke c m es tyann))
       else return $ (retty, StaticInvoke c m exprs (Just retty))
                           
tcExp info env (ConstTrue)  = return $ (TypeName "boolean", ConstTrue)
tcExp info env (ConstFalse) = return $ (TypeName "boolean", ConstFalse)
tcExp info env (ConstNull) = return $ (TypeName "null", ConstNull) -- TODO: null type
tcExp info env (ConstNum n) = return $ (TypeName "int", ConstNum n)
tcExp info env (ConstLit s) = return $ (TypeName "String", ConstLit s)
tcExp info env (ConstChar s) = return $ (TypeName "char", ConstChar s)
tcExp info env (Prim "[]" [e1,e2]) = 
  do (ty1, expr1) <- tcExp info env e1
     (ty2, expr2) <- tcExp info env e2
     if isArray ty1 == False || isInt ty2 == False
       then throwError ("tcExp: type mismatch in array indexing: " ++ 
                        show (Prim "[]" [e1,e2]))
       else return $ (elemType ty1, Prim "[]" [expr1,expr2])

tcExp info env (Prim "[]=" [e1, e2]) = 
  do (ty1, expr1) <- tcExp info env e1
     (ty2, expr2) <- tcExp info env e2
     if subType info ty2 ty1 == False
       then throwError ("tcExp: not assignable type: " ++ 
                        show (Prim "[]=" [e1, e2]))
       else return $ (TypeName "void", Prim "[]=" [expr1,expr2])
        
tcExp info env (Prim "super" es) = 
  do throwError ("tcExp: misplaced super call: " ++ show (Prim "super" es))

tcExp info env (Prim n es) = 
  if elem n ["==", "!="] == False
     then tcExp' info env (Prim n es)
     else tcExp'' info env (Prim n es)     
  
tcExp' info env (Prim n es) =  
  do tyexprs <- mapM (tcExp info env) es
     let (tys,exprs) = unzip tyexprs
     let mtypes = [(argtys,retty) | (p, argtys, retty) <- primTypeTable, p == n]
     let rettys = [retty | (argtys, retty) <- mtypes
                         , length tys == length argtys
                         , all (True==) [eqType ty1 ty2 
                                        | (ty1,ty2) <- zip tys argtys]]
     case rettys of
       []  -> throwError ("tcExp: type mismatch in " ++ show (Prim n es))
       [h] -> return $ (h, Prim n exprs)
       _   -> throwError ("tcExp: multiple type matches: " ++ show (Prim n es))

tcExp'' info env (Prim n [e1,e2]) = -- ==, !=
  do tyexprs <- mapM (tcExp info env) [e1,e2]
     let (tys,exprs) = unzip tyexprs
     let [ty1,ty2] = tys
     if (subType info ty1 ty2 || subType info ty2 ty1) == False
       then throwError ("tcExp: type mismatch in " ++ show (Prim n [e1,e2]))
       else return $ (TypeName "boolean", Prim n exprs)


-- tcExp info env es = error ("Missing: " ++ show es)

tcBeginStmt :: Info -> TypingCtx -> TypingEnv -> TypeName -> Stmt 
          -> ErrorT TCError IO (TypingEnv, Stmt)
tcBeginStmt info ctx env retty stmt = 
  do let maybe = firstStmt stmt
     let (stmt1, therest) = fromJust maybe
     if isJust maybe == False || isSuperCall stmt1 == False
     then tcStmt info ctx env retty stmt
     else do (env1,supercall1) <- tcSuperCall info ctx env stmt1
             if null therest 
             then return (env1, supercall1)
             else tcStmt info ctx env1 retty (head therest)

tcSuperCall :: Info -> TypingCtx -> TypingEnv -> Stmt -> ErrorT TCError IO (TypingEnv, Stmt)
tcSuperCall info ctx env (Expr (Prim "super" es)) =
  do argtysexprs <- mapM (tcExp info env) es
     let (argtys2,argexprs) = unzip argtysexprs
     let p          = getParentClass ctx
     let maybektype = lookupKtype info (TypeName p) argtys2
     let argtys1    = fromJust maybektype
     if isNothing maybektype
       then throwError ("tcExp: invalid super call: " ++ show (Prim "super" es))
       else return $ (env, (Expr (Prim "super" argexprs)))
        
tcSuperCall info ctx env stmt =
  throwError ("tcSuperCall: unexpected statement: " ++ show stmt)

tcStmt :: Info -> TypingCtx -> TypingEnv -> TypeName -> Stmt 
          -> ErrorT TCError IO (TypingEnv, Stmt)
tcStmt info ctx env retty (Expr e) =
  do (ty, expr) <- tcExp info env e
     return $ (env, Expr expr)
    
tcStmt info ctx env retty (Ite cond s1 s2) =
  do (condty, condexpr) <- tcExp  info env cond
     (env1,   stmt1)    <- tcStmt info ctx env retty s1
     (env2,   stmt2)    <- tcStmt info ctx env retty s2
     let env'  = if isJust (lookupEnv env1 "return") && 
                    isJust (lookupEnv env2 "return")
                 then update env "return" retty
                 else env
         
     if isBoolean condty == False
       then throwError ("tcExp: not boolean type for " ++ show (Ite cond s1 s2))
       else return $ (env', Ite condexpr stmt1 stmt2)

tcStmt info ctx env retty (LocalVarDecl tn x id maybee) = 
  do let e = fromJust maybee
     let env' = update env x tn 
     if isNothing maybee 
     then return $ (env', LocalVarDecl tn x id maybee)
     else do (ty, expr) <- tcExp info env e
             if subType info ty tn == False
             then throwError 
                    ("tcExp: type mismatch for " ++ x ++ " in " ++ show e)
             else return $ (env', LocalVarDecl tn x id (Just expr))

tcStmt info ctx env retty (Return Nothing) = 
  do let env1 = update env "return" (TypeName "void")
     if eqType retty (TypeName "void") == False 
     then throwError ("tcStmt: return type is not void: " ++ show retty)
     else return $ (env1, Return Nothing)
     
tcStmt info ctx env retty (Return (Just e)) = 
  do (ty, expr) <- tcExp info env e
     let env1 = update env "return" ty
     if subType info ty retty == False
       then throwError ("tcStmt: return type mismatch for " ++ 
                        show (Return (Just e)))
       else return $ (env1, Return (Just e))

tcStmt info ctx env retty (Seq s1 s2) =
  do (env1, stmt1) <- tcStmt info ctx env retty s1
     if isJust (lookupEnv env1 "return")
       then throwError ("tcStmt: dead code after " ++ show s1)
       else tcStmt info ctx env1 retty s2

tcStmt info ctx env retty (NoStmt) = return $ (env, NoStmt)

tcStmt info ctx env retty (For maybetyn x init cond upd s) =
  do let (ty,n) = fromJust maybetyn
     let env' = if isJust maybetyn then update env x ty else env
     (initty, initexpr) <- tcExp  info env' (Assign (Var x) init)
     (condty, condexpr) <- tcExp  info env' cond
     (updty,  updexpr)  <- tcExp  info env' upd
     (env1,   stmt1)    <- tcStmt info ctx env' retty s
     if eqType (TypeName "boolean") condty == False
       then throwError ("tcstmt: not boolean type for " ++ show cond)
       else return $ (env, (For maybetyn x initexpr condexpr updexpr stmt1))
                           -- Restore the environment

tcStmt info ctx env retty (While e s) = 
  do (ty,   expr)  <- tcExp info env e
     (env1, stmt1) <- tcStmt info ctx env retty s
     if eqType (TypeName "boolean") ty == False
       then throwError ("tcstmt: not boolean type for " ++ show expr)
       else return $ (env, While expr stmt1)  -- Restore the environment

tcStmt info ctx env retty (Block s) = 
  do (env1, stmt1) <- tcStmt info ctx env retty s
     let env' = if isJust (lookupEnv env1 "return")
                then update env "return" retty else env
     return $ (env', Block stmt1)  -- Restore the environment
     -- TODO: something wrong here. The next statement after the block
     -- becomes dead if the block contains a return statement.

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
               
     
