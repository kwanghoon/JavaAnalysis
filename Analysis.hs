module Analysis where

import AST
import Library
import Subtyping
import Data.Maybe
import Data.List
import Control.Monad.Trans (liftIO)
import Control.Monad.State 

--
type ObjAllocSite = UniqueId
type Context      = [ObjAllocSite]
data Set          = Set [Context]

instance Show Set where
  showsPrec p (Set s) =
    conc $ [ "{", " " ] ++ comma (map showContext s) ++ [ " ", "}" ]
    
showContext :: Context -> String    
showContext []         = "empty"
showContext [obj]      = obj_alloc_site_prefix ++ show obj
showContext (obj:objs) = obj_alloc_site_prefix ++ show obj ++ "." ++ showContext objs

obj_alloc_site_prefix = "o"

staticContext = [uniqueidforstatic]

addContext :: Int -> Context -> Label -> Context
addContext k context label = take k $ context ++ [label]

length_k = 1

-- Types annotated with a set of contexts
data AnnoType = 
    AnnoType Name UniqueId           -- C{Xi}
  | AnnoArrayType AnnoType UniqueId  -- C[]{Xi}[]{Xj}
    
instance Show AnnoType where    
  showsPrec p (AnnoType n id)        = conc [ n, "{", constraint_var_prefix, show id, "}" ]
  showsPrec p (AnnoArrayType aty id) = conc [ show aty, "[", "]", "{", show id, "}" ]

type AnnoMethodType = ([AnnoType], AnnoType, Effect)

getAnno (AnnoType c id)        = id
getAnno (AnnoArrayType aty id) = id

unionType :: Info -> AnnoType -> AnnoType -> StateT AnalysisState IO AnnoType
unionType info (AnnoType c1 id1) (AnnoType c2 id2) 
  | c1 == "null" = return (AnnoType c2 id2)
  | c1 == c2 && id1 == id2 = return (AnnoType c1 id1)
  | c1 == c2 && id1 /= id2 = do
    id <- newId
    putConstraint (C_var id1 id)
    putConstraint (C_var id2 id)
    return (AnnoType c1 id)
  | subType info (TypeName c1) (TypeName c2) = do
    id <- newId
    putConstraint (C_var id1 id)
    putConstraint (C_var id2 id)
    return (AnnoType c2 id)
  | subType info (TypeName c2) (TypeName c1) = do
    id <- newId
    putConstraint (C_var id1 id)
    putConstraint (C_var id2 id)
    return (AnnoType c1 id)
    
unionType info (AnnoArrayType c1 id1) (AnnoArrayType c2 id2) = do
  c  <- unionType info c1 c2
  id <- newId
  putConstraint (C_var id1 id)
  putConstraint (C_var id2 id)
  return (AnnoArrayType c id)
  
unionType info (AnnoArrayType c1 id1) (AnnoType c2 id2) 
  | isObjectMultiDimArray (AnnoArrayType c1 id1) && (c2 == "Object") = do
    putConstraint (C_var id1 id2)
    return (AnnoType c2 id2)
  | otherwise = do
    liftIO $ putStrLn ("unionType: something wrong with ")
    return (AnnoType c2 id2)
  where
    -- cf. the equally named function in Subtyping.hs
    isObjectMultiDimArray (AnnoArrayType c id) = isObjectMultiDimArray c 
    isObjectMultiDimArray (AnnoType c id)      = c == "Object"

--
type TypingTable = [TableEntry]
data TableEntry = 
    -- F(C,ctx,f)=Xi
    F ClassName Context FieldName AnnoType  
    
    -- M(C,ctx,m,k)= (Xi1, ... , Xin) ==> Xj
  | M ClassName Context MethodName UniqueId [AnnoType] AnnoType Effect
    
    -- V(C,ctx,m,k,x,j) = Xi
  | V ClassName Context MethodName UniqueId VarName UniqueId AnnoType
    
emptyTypingTable = []    
unionTypingTable t1 t2 = t1 ++ t2

-- Effects
data Effect = Eff [ClassName] | EffVar UniqueId | EffUnion Effect Effect

instance Show Effect where
  showsPrec p (Eff cs)    = conc $ [ "{" ] ++ comma cs ++ [ "}" ]
  showsPrec p (EffVar id) = conc [ effect_var_prefix, show id ]
  showsPrec p (EffUnion eff1 eff2) = conc [ show eff1, " ", "U", " ", show eff2 ]

noEffect = Eff []
    
--
type TypingEnv = [(Name, AnnoType)] -- cf. [(Name, TypeName)] in TypeCheck.hs
type TypingCtx = (ClassName, Maybe ClassName, [ClassName], Maybe MethodName)

addTyEnv :: (Name,AnnoType) -> TypingEnv -> TypingEnv
addTyEnv (n,aty) typingenv = (n,aty):typingenv

unionTyEnv :: Info -> TypingEnv -> TypingEnv -> StateT AnalysisState IO TypingEnv
unionTyEnv info []               tyenv2           = return tyenv2
unionTyEnv info ((n,aty):tyenv1) []               = return ((n,aty):tyenv1)
unionTyEnv info ((n,aty):tyenv1) ((m,bty):tyenv2)
  | n==m = do
    ty    <- unionType info aty bty
    tyenv <- unionTyEnv info tyenv1 tyenv2
    return ((n,ty):tyenv)
  | otherwise = do
    tyenv  <- unionTyEnv info ((n,aty):tyenv1) tyenv2
    tyenv' <- unionTyEnv info [(m,bty)] tyenv
    return tyenv'
    
domain :: TypingEnv -> [Name]    
domain tyenv = [ x | (x,ty) <- tyenv ]

restrict :: TypingEnv -> [Name] -> TypingEnv
restrict tyenv dom = [ (x,ty) | (x,ty) <- tyenv, x `elem` dom ]

-- Constraints
type Constraints = [Constraint]
data Constraint  = 
    -- { r1, ... , rn } \subseteq Xi
    C_lower Set UniqueId        
    -- Xi \subseteq { r1, ... , rn }
  | C_upper UniqueId Set 
    
    -- Xj \subseteq Xi
  | C_var UniqueId UniqueId   
    
    -- C_field C{X} f D{Z}              ===> C{X}.f <: D{Z}
  | C_field AnnoType FieldName AnnoType
    
    -- C_assign D Z C X f               ===> D{Z} <: C{X}
  | C_assign AnnoType AnnoType

    -- C_assignfield C{X} D{Z}f         ===> C{X} <: D{Z}.f
  | C_assignfield AnnoType AnnoType FieldName 
    
    -- C_invoke C X m [S1,...,Sn] eff T ===> C{X}.m <: (S1,...,Sn) --eff--> T 
  | C_invoke AnnoType MethodName [AnnoType] Effect AnnoType
    
    
constraint_var_prefix = "x"
effect_var_prefix = "e"

instance Show Constraint where
  showsPrec p (C_lower set id) = 
    conc $ [ show set, " ", "<=", " ", constraint_var_prefix, show id ]
  showsPrec p (C_upper id set) = 
    conc $ [ constraint_var_prefix, show id, " ", "<=", " ", show set ]
  showsPrec p (C_var x y) = 
    conc [ constraint_var_prefix, show x, " ", "<=", " ", constraint_var_prefix, show y ]
  showsPrec p (C_field aty1 f aty2) = 
    conc [ show aty1, ".", f, " ", "<:", " ", show aty2 ]
  showsPrec p (C_assign aty1 aty2) = 
    conc [ show aty1, " ", "<:", " ", show aty2 ]
  showsPrec p (C_assignfield aty1 aty2 f) = 
    conc [ show aty1, " ", "<:", " ", show aty2, ".", f ]
  showsPrec p (C_invoke aty1 m atys2 eff aty2) = 
    conc $ [ show aty1, ".", m, " ", "<:", " ", 
           "(" ] ++ comma (map show atys2) ++ [ ")", "--", show eff, "-->", show aty2 ]
    
-- WorkList
type WorkList = [(ClassName,Context,MethodName,UniqueId)]

isWorklistEmpty [] = True
isWorklistEmpty _  = False

nextFromWorklist l = (head l, tail l)


type AnalysisState = -- Can be extended if necessary
  (WorkList, Constraints, TypingTable, UniqueId)

-- 
type ActionMethod = 
  Info 
  -> Context 
  -> StateT AnalysisState IO ()
              
type ActionStmt = 
  TypingEnv
  -> TypingCtx
  -> Info
  -> Context
  -> StateT AnalysisState IO (TypingEnv, Effect)
  
type ActionExpr =  
  TypingEnv
  -> Info
  -> Context
  -> StateT AnalysisState IO (AnnoType, Effect)
  
type MethodIdentifier = (ClassName, MethodName, UniqueId)
  
type ActionLookupTable = [ActionLookupTableEntry]
type ActionLookupTableEntry = (MethodIdentifier, ActionMethod)

lookupActionTable alist x =  -- TODO: Excerpted from TypeCheck.hs
  case [ t | (y,t) <- alist, x == y ] of
    []    -> Nothing
    (t:_) -> Just t
    
lookupFieldTyping :: TypingTable -> ClassName -> Context -> FieldName -> [AnnoType]
lookupFieldTyping typingtable c ctx f =
  [ atyi | F ci ctxi fi atyi <- typingtable, c==ci && ctx==ctxi && f==fi ]

lookupMethodTyping :: TypingTable -> ClassName -> Context -> MethodName -> UniqueId 
                      -> [AnnoMethodType]
lookupMethodTyping typingtable c ctx m id =
  [ (atysi, atyi, eff) 
  | M ci ctxi mi idi atysi atyi eff <- typingtable
  , c==ci && ctx==ctxi && m==mi && idi==id ]

lookupVarTyping :: TypingTable -> ClassName -> Context -> MethodName -> UniqueId -> VarName 
                   -> UniqueId -> [AnnoType]
lookupVarTyping typingtable c ctx m id v vid = 
  [ atyi | V ci ctxi mi idi vi vidi atyi <- typingtable, 
    c==ci && ctx==ctxi && m==mi && idi==id && vi==v && vidi==vid ]
  
--
doAnalysis :: Program -> Info -> IO ()
doAnalysis program info = do
  let worklist    = [] :: WorkList
  let typingtable = [] :: TypingTable
  let constraints = [] :: Constraints
  let uniqueid    = initialuniqueid  :: UniqueId    
  let state       = (worklist, constraints, typingtable, uniqueid)
  actionlookuptable <- mkActionProgram program
  prActionLookupTable info [] actionlookuptable state
  -- (_, state1) <- runStateT (doWork info actionlookuptable) state
  -- let (worklist1, constraints1, typingtable1, uniqueid1) = state1
  -- (_, constraints2) <- runStateT doSolve constraints1
  
prActionLookupTable :: Info -> Context -> ActionLookupTable -> AnalysisState -> IO ()
prActionLookupTable info context actionlookuptable initstate = do
  (_,state) <- runStateT (foldr f (return ()) actionlookuptable) initstate
  return ()
  where
    f :: ActionLookupTableEntry -> StateT AnalysisState IO () -> StateT AnalysisState IO ()
    f entry m = do pr info context entry
                   m
    
    pr info context ((c,m,id), actionmethod) = do
      constraints0 <- getConstraints
      resetConstraints []
      liftIO $ putStrLn $ concat [c, ",", m, ",", show id, "(", showContext context, ")", ":"]
      _ <- actionmethod info context
      constraints <- getConstraints
      liftIO $ mapM_ pr1 (reverse constraints) -- reverse for better readability
      resetConstraints (constraints ++ constraints0)
    
    pr1 constraint = do
      putStr   $ " - "
      putStrLn $ show constraint
      

--
doWork :: Info -> ActionLookupTable -> StateT AnalysisState IO ()
doWork info actionlookuptable = do
  -- Repeate the following execution until the worklist is empty
  --  1. Get a piece of work from worklist
  --  2. Get an action for the work from actionlookuptable
  --  3. Execute the action to update the typing table and constraints and
  --  4. to also update the worklist if there is any new typing table entry
  
  worklist <- getWorklist 
  if isWorklistEmpty worklist == False
     then do liftIO $ putStrLn "The worklist is now empty."
     else do let (work, worklist1) = nextFromWorklist worklist
             putWorklist worklist1
             doWork' info actionlookuptable work 
          
doWork' info actionlookuptable (c,ctx,m,id) = do
  let maybeaction = lookupActionTable actionlookuptable (c,m,id)
  let action = fromJust maybeaction
  if isNothing maybeaction 
    then liftIO $ putStrLn $ "Error: Can't find an action for: " ++ show (c,ctx,m,id)
    else do action info ctx
            doWork info actionlookuptable
            
doSolve :: StateT Constraints IO ()
doSolve = liftIO $ putStrLn "dosolve..."

--
newId :: StateT AnalysisState IO UniqueId
newId = do
  (worklist,constraints,typingtable,uniqueid) <- get
  put (worklist,constraints,typingtable,uniqueid+1)
  return uniqueid
  
newIds :: [a] -> StateT AnalysisState IO [UniqueId]
newIds ls = mapM f ls
  where
    f _ = newId
    
newEffVar :: StateT AnalysisState IO Effect
newEffVar = do
  (worklist,constraints,typingtable,uniqueid) <- get
  put (worklist,constraints,typingtable,uniqueid+1)
  return (EffVar uniqueid)

getWorklist :: StateT AnalysisState IO WorkList
getWorklist = do
  (worklist,_,_,_) <- get
  return worklist
  
putWorklist :: WorkList -> StateT AnalysisState IO ()
putWorklist worklist = do
  (_,constraints,typingtable,uniqueid) <- get
  put (worklist,constraints,typingtable,uniqueid)
  
getConstraints :: StateT AnalysisState IO Constraints
getConstraints = do
  (worklist,constraints,typingtable,uniqueid) <- get
  return constraints
  
resetConstraints :: Constraints -> StateT AnalysisState IO ()
resetConstraints constraints = do
  (worklist,_,typingtable,uniqueid) <- get
  put (worklist,constraints,typingtable,uniqueid)

putConstraint :: Constraint -> StateT AnalysisState IO ()
putConstraint constraint = do
  (worklist,constraints,typingtable,uniqueid) <- get
  put (worklist,constraint:constraints,typingtable,uniqueid)
  
getFieldtyping :: ClassName -> Context -> FieldName -> StateT AnalysisState IO (Maybe AnnoType)
getFieldtyping c ctx f = do
  (worklist,constraints,typingtable,uniqueid) <- get
  retMaybeAnnotype $ lookupFieldTyping typingtable c ctx f
  
  where
    retMaybeAnnotype []  = return Nothing
    retMaybeAnnotype [h] = return $ Just h
    retMaybeAnnotype _   = do liftIO $ putStrLn $ "getFieldtyping: duplicate field typings for " 
                                ++ show (c,ctx,f)
                              return Nothing
                              
putTyping :: TableEntry -> StateT AnalysisState IO ()
putTyping typing = do
  (worklist,constraints,typingtable,uniqueid) <- get
  put (worklist,constraints,typing:typingtable,uniqueid)
                              
getMethodtyping :: ClassName -> Context -> MethodName -> UniqueId 
                   -> StateT AnalysisState IO (Maybe AnnoMethodType)
getMethodtyping c ctx m id = do
  (worklist,constraints,typingtable,uniqueid) <- get
  retMaybeAnnomtype $ lookupMethodTyping typingtable c ctx m id
  
  where
    retMaybeAnnomtype []  = return Nothing
    retMaybeAnnomtype [h] = return $ Just h
    retMaybeAnnomtype _   = do liftIO $ putStrLn $ "getMethodtyping: duplicate method typings for "
                                ++ show (c,ctx,m,id)
                               return Nothing

getVartyping :: ClassName -> Context -> MethodName -> UniqueId -> VarName -> UniqueId 
                   -> StateT AnalysisState IO (Maybe AnnoType)
getVartyping c ctx m id v vid = do
  (worklist,constraints,typingtable,uniqueid) <- get
  retMaybeAnnovtype $ lookupVarTyping typingtable c ctx m id v vid
  
  where
    retMaybeAnnovtype []  = return Nothing
    retMaybeAnnovtype [h] = return $ Just h
    retMaybeAnnovtype _   = do liftIO $ putStrLn $ "getVartyping: duplicate variable typings for "
                                ++ show (c,ctx,m,id,v,vid)
                               return Nothing
                               
mkAnnoType :: TypeName -> StateT AnalysisState IO AnnoType
mkAnnoType (TypeName n) = do
  id <- newId
  return (AnnoType n id)
mkAnnoType (ArrayTypeName ty) = do  
  id <- newId
  aty <- mkAnnoType ty
  return (AnnoArrayType aty id)

mkAnnoArgType (ty, x, id) = do
  aty <- mkAnnoType ty
  return (x, aty)
      


--  
lookupEnv tyenv x =  -- TODO: Excerpted from TypeCheck.hs
  case [ t | (y,t) <- tyenv, x == y ] of
    []    -> error $ "no " ++ x ++ " in " ++ show tyenv
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
    
--
mkActionProgram :: Program -> IO ActionLookupTable
mkActionProgram program = do
  actionmethodss <- mapM mkActionClass program
  return $ concat $ actionmethodss
    
mkActionClass :: Class -> IO ActionLookupTable
mkActionClass (Class attrs n p is mdecls) = do
  actionmethodss <- mapM (mkActionMDecl (n, p, is)) mdecls
  return $ concat $ actionmethodss
  
mkActionClass (Interface n is mdecls) = do  
  return []
    
mkActionMDecl :: ClassInfo -> MemberDecl -> IO ActionLookupTable
mkActionMDecl (n, p, is) (MethodDecl attrs retty m id argdecls stmt) = do
  actionstmt <- mkActionStmt stmt
  return [( (n,m,id), mkaction actionstmt (n, p, is, Just m) )]
  where
    mkaction actionstmt typingctx info context = do
      thisaty    <- mkAnnoType (TypeName n)
      retaty     <- mkAnnoType retty
      aargdecls  <- mapM mkAnnoArgType argdecls
      let id = getAnno thisaty
      let typingenv = [("this", thisaty), ("return", retaty)] ++ aargdecls
      putConstraint (C_upper id (Set [context]))
      _ <- actionstmt typingenv typingctx info context
      return ()
      
mkActionMDecl (n, p, is) (ConstrDecl m id argdecls stmt) = do
  actionstmt <- mkActionStmt stmt
  return [((n,m,id),mkaction actionstmt (n, p, is, Just m))]
  where
    mkaction actionstmt typingctx info context = do
      thisaty   <- mkAnnoType (TypeName n)
      retaty    <- mkAnnoType (TypeName n)
      aargdecls <- mapM mkAnnoArgType argdecls
      let id = getAnno thisaty
      let typingenv = [("this", thisaty), ("return", retaty)] ++ aargdecls
      _ <- actionstmt typingenv typingctx info context
      return ()
  
mkActionMDecl (n, p, is) (FieldDecl attrs ty f maybee) = do
  return []
  
mkActionMDecl (n, p, is) (AbstractMethodDecl retty m id argdecls) = do
  return []
      
  
mkActionStmt :: Stmt -> IO ActionStmt
mkActionStmt (Expr expr) = do
  actionexpr <- mkActionExpr expr
  return $ mkaction actionexpr
  where
    mkaction actionexpr typingenv typingctx info context = do
      (_, effect) <- actionexpr typingenv info context
      return (typingenv, effect)
  
mkActionStmt (Ite expr stmt1 stmt2) = do
  actionexpr  <- mkActionExpr expr
  actionstmt1 <- mkActionStmt stmt1
  actionstmt2 <- mkActionStmt stmt2
  return $ mkaction actionexpr actionstmt1 actionstmt2
  where
    mkaction :: ActionExpr -> ActionStmt -> ActionStmt -> ActionStmt
    mkaction actionexpr actionstmt1 actionstmt2 typingenv typingctx info context = do
      (_, effe)      <- actionexpr typingenv info context
      (tyenv1,effs1) <- actionstmt1 typingenv typingctx info context
      (tyenv2,effs2) <- actionstmt2 typingenv typingctx info context
      tyenv <- unionTyEnv info tyenv1 tyenv2
      let eff   = EffUnion effe (EffUnion effs1 effs2)
      return (tyenv, eff)
      
mkActionStmt (LocalVarDecl ty x id maybeexpr) = do
  actionmaybeexpr <- mkActionMaybeExpr maybeexpr
  return $ mkaction ty x id actionmaybeexpr
  where
    mkaction :: TypeName -> VarName -> UniqueId -> Maybe ActionExpr -> ActionStmt
    mkaction ty x id maybeactionexpr typingenv typingctx info context = do
      (atye,effe) <- doit maybeactionexpr typingenv info context
      aty <- mkAnnoType ty
      putConstraint (C_assign atye aty)
      let tyenv = addTyEnv (x,aty) typingenv
      return (tyenv, effe)
    
    mkActionMaybeExpr Nothing = do
      return $ Nothing
    mkActionMaybeExpr (Just expr) = do
      actionexpr <- mkActionExpr expr
      return $ Just actionexpr
      
    doit Nothing typingenv info context = do
      aty <- mkAnnoType (TypeName "null")
      return (aty, noEffect)
    doit (Just actionexpr) typingenv info context = do
      actionexpr typingenv info context
      
mkActionStmt (Return maybeexpr) = do
  maybeaction <- mkmaybeaction maybeexpr
  return $ mkaction maybeaction
  
  where
    mkmaybeaction (Nothing) = return Nothing
    mkmaybeaction (Just expr) = do
      actionexpr <- mkActionExpr expr
      return (Just actionexpr)
      
    mkaction :: Maybe ActionExpr -> ActionStmt
    mkaction (Nothing) typingenv typingctx info context = do
      return (typingenv, noEffect)
    mkaction (Just actionexpr) typingenv typingctx info context = do
      (aty,eff) <- actionexpr typingenv info context
      let atyret = fromJust $ lookupEnv typingenv "return"
      putConstraint (C_assign aty atyret)
      return (typingenv, eff)
      
mkActionStmt (Seq stmt1 stmt2) = do
  actionstmt1 <- mkActionStmt stmt1
  actionstmt2 <- mkActionStmt stmt2
  return $ mkaction actionstmt1 actionstmt2
  where
    mkaction actionstmt1 actionstmt2 typingenv typingctx info context = do
      (typingenv1,effect1) <- actionstmt1 typingenv typingctx info context
      (typingenv2,effect2) <- actionstmt2 typingenv1 typingctx info context
      let effect3 = EffUnion effect1 effect2
      return (typingenv2, effect3)
    
mkActionStmt (NoStmt) = do
  return actionext
  where
    actionext typingenv typingctx info context = do
      return (typingenv, noEffect)
      
mkActionStmt (While e stmt) = do      
  actione    <- mkActionExpr e
  actionstmt <- mkActionStmt stmt
  return $ mkaction actione actionstmt
  where
    mkaction actione actionstmt typingenv typingctx info context = do
      (_,eff1) <- actione typingenv info context
      (typingenv',eff2) <- actionstmt typingenv typingctx info context
      -- tyenv' <- unionTyEnv info typingenv typingenv'
      let tyenv = restrict typingenv' (domain typingenv)
      let eff   = EffUnion eff1 eff2
      return (tyenv, eff)
        
mkActionStmt (For maybedecl x e1 e2 e3 stmt) = do      
  actione1 <- mkActionExpr e1
  actione2 <- mkActionExpr e2
  actione3 <- mkActionExpr e3
  actionstmt <- mkActionStmt stmt
  return $ mkaction maybedecl x actione1 actione2 actione3 actionstmt
  where
    mkaction maybedecl x actione1 actione2 actione3 actionstmt 
      typingenv typingctx info context = do
        (_, eff1) <- actione1 typingenv info context
        typingenv'<- mkmaybedeclaction maybedecl x typingenv typingctx info context
        (_, eff2) <- actione2 typingenv' info context
        (_, eff3) <- actione3 typingenv' info context
        (typingenv'', eff4) <- actionstmt typingenv' typingctx info context
        -- tyenv' <- unionTyEnv info typingenv typingenv''
        let tyenv = restrict typingenv'' (domain typingenv)
        let eff = EffUnion eff1 (EffUnion eff2 (EffUnion eff3 eff4))
        return (tyenv, eff)
        
    mkmaybedeclaction (Nothing) x typingenv typingctx info context = 
      return typingenv
    mkmaybedeclaction (Just (ty, id)) x typingenv typingctx info context = do
      aty <- mkAnnoType ty
      let typingenv' = addTyEnv (x,aty) typingenv
      return typingenv'
      
mkActionStmt (Block stmt) = do
  actionstmt <- mkActionStmt stmt
  return $ mkaction actionstmt
  where
    mkaction actionstmt typingenv typingctx info context = do
      (_,eff) <- actionstmt typingenv typingctx info context
      return (typingenv, eff)
      
--
mkActionExpr :: Expr -> IO ActionExpr
mkActionExpr (Var x) = do
  return $ actionVar x
  where
    actionVar :: VarName -> ActionExpr
    actionVar x typingenv info context = do
      let aty = fromJust $ lookupEnv typingenv x -- TODO: Error handling in case maybeaty is Nothing
      return $ (aty, noEffect)

mkActionExpr (Field e f maybety) = do
  actionexp <- mkActionExpr e
  return $ actionField actionexp f (fromJust maybety)
  where
    actionField :: ActionExpr -> FieldName -> TypeName -> ActionExpr
    actionField actionexp f ty typingenv info context = do
      (atyexp, eff) <- actionexp typingenv info context
      aty <- mkAnnoType ty
      putConstraint (C_field atyexp f aty)
      return (aty, eff)

mkActionExpr (StaticField c f maybety) = do
  return $ actionStaticfield c f (fromJust maybety)
  where
    actionStaticfield :: TypeName -> Name -> TypeName -> ActionExpr  
    actionStaticfield c f ty typingenv info context = do
      let maybefty    = lookupFields info c f
      let (fty,attrs) = fromJust maybefty
      cty <- mkAnnoType c
      aty <- mkAnnoType ty
      let cid = getAnno cty
      putConstraint (C_upper cid (Set [staticContext]))
      putConstraint (C_field cty f aty)
      return (aty, noEffect)
  
mkActionExpr (New c es label) = do
  actionexps <- mapM mkActionExpr es
  return $ actionNew c actionexps label
  where
    actionNew :: TypeName -> [ActionExpr] -> Label -> ActionExpr
    actionNew c actionexps label typingenv info context = do
      atyeffs <- mapM (\actionexp -> actionexp typingenv info context) actionexps
      let (atys,effs) = unzip atyeffs
      cty          <- mkAnnoType c
      effVar       <- newEffVar
      let cid = getAnno cty
      let eff = EffUnion (foldr EffUnion noEffect effs) effVar
          
      putConstraint (C_lower (Set [addContext length_k context label]) cid)
      addInvokeConstraint c cid cty atys effVar
      return (cty, eff)
      
    addInvokeConstraint c@(TypeName cn)  cid cty   atys eff = 
      putConstraint (C_invoke cty cn atys eff cty)
    addInvokeConstraint c@(ArrayTypeName _) cid cty atys eff = return ()
      
mkActionExpr (Assign e1@(Var x) e2) = do
  actionexp2 <- mkActionExpr e2
  return $ actionAssignVar x actionexp2
  where
    actionAssignVar :: Name -> ActionExpr -> ActionExpr
    actionAssignVar x actionexp2 typingenv info context = do
      let aty1 = fromJust $ lookupEnv typingenv x
      (aty2,eff2) <- actionexp2 typingenv info context
      putConstraint (C_assign aty2 aty1)
      avoidty <- mkAnnoType (TypeName "void")
      return (avoidty, eff2)
      
mkActionExpr (Assign (Field e1 f maybety) e2) = do
  actionexp1 <- mkActionExpr e1
  actionexp2 <- mkActionExpr e2
  return $ actionAssignField actionexp1 f (fromJust maybety) actionexp2
  where
    actionAssignField :: ActionExpr -> FieldName -> TypeName -> ActionExpr -> ActionExpr
    actionAssignField actionexp1 f ty actionexp2 typingenv info context = do
      (aty1,eff1) <- actionexp1 typingenv info context
      (aty2,eff2) <- actionexp2 typingenv info context
      putConstraint (C_assignfield aty2 aty1 f)
      avoidty <- mkAnnoType (TypeName "void")
      let eff = EffUnion eff1 eff2
      return (avoidty, eff)
      
mkActionExpr (Assign (StaticField c f maybety) e2) = do
  actionexp2 <- mkActionExpr e2
  return $ actionAssignField c f (fromJust maybety) actionexp2
  where
    actionAssignField :: TypeName -> FieldName -> TypeName -> ActionExpr -> ActionExpr
    actionAssignField c f ty actionexp2 typingenv info context = do
      (aty2,eff2) <- actionexp2 typingenv info context
      cty <- mkAnnoType c
      let cid = getAnno cty
      putConstraint (C_assignfield aty2 cty f)
      putConstraint (C_upper cid (Set [staticContext]))
      avoidty <- mkAnnoType (TypeName "void")
      return (avoidty, eff2)

mkActionExpr (Assign (Prim "[]" [earr,eidx]) e2) = do
  actionexparr <- mkActionExpr earr
  actionexpidx <- mkActionExpr eidx
  actionexp2   <- mkActionExpr e2
  return $ actionAssignArray actionexparr actionexpidx actionexp2
  where
    actionAssignArray :: ActionExpr -> ActionExpr -> ActionExpr -> ActionExpr 
    actionAssignArray actionexparr actionexpidx actionexp2 typingenv info context = do
      (atyarr,effarr) <- actionexparr typingenv info context
      (atyidx,effidx) <- actionexpidx typingenv info context
      (aty2,  eff2)   <- actionexp2   typingenv info context
      let AnnoArrayType atyelem id = atyarr
      putConstraint (C_assign aty2 atyelem)
      avoidty <- mkAnnoType (TypeName "void")
      let eff = EffUnion effarr (EffUnion effidx eff2)
      return (avoidty, eff)

-- In mkActionExpr (Assign e1 e2), e1 can't be the others
-- because the typechecker has already filtered such an illegal program.
      
mkActionExpr (Cast c e) = do
  actionexp <- mkActionExpr e
  return $ actionCast c actionexp
  where
    actionCast :: TypeName -> ActionExpr -> ActionExpr
    actionCast c actionexp typingenv info context = do
      (aty,eff) <- actionexp typingenv info context
      let cty = mkCast c aty
      return (cty, eff)
      
mkActionExpr (Invoke e m es maybety) = do
  actionexp  <- mkActionExpr e
  actionexps <- mapM mkActionExpr es
  return $ actionInvoke actionexp m actionexps (fromJust maybety)
  where
    actionInvoke :: ActionExpr -> MethodName -> [ActionExpr] -> TypeName -> ActionExpr
    actionInvoke actionexp m actionexps ty typingenv info context = do
      (atye,effe) <- actionexp typingenv info context
      atyeffes    <- mapM (\actionexp -> actionexp typingenv info context) actionexps
      aty         <- mkAnnoType ty
      effm        <- newEffVar
      let (atyes,effes) = unzip atyeffes
      let id            = getAnno aty
      let eff           = foldr EffUnion effe effes
          
      putConstraint (C_invoke atye m atyes effm aty)
      return (aty, eff)
      
mkActionExpr (StaticInvoke c m es maybety) = do      
  actionexps <- mapM mkActionExpr es
  return $ actionStaticinvoke c m actionexps (fromJust maybety)
  where
    actionStaticinvoke :: TypeName -> MethodName -> [ActionExpr] -> TypeName -> ActionExpr
    actionStaticinvoke c m actionexps ty typingenv info context = do
      atyeffs <- mapM (\actionexp -> actionexp typingenv info context) actionexps
      let (atyes,effes) = unzip atyeffs
      cty  <- mkAnnoType c
      aty  <- mkAnnoType ty
      effm <- newEffVar
      let cid = getAnno cty
      let eff = foldr EffUnion noEffect effes
          
      putConstraint (C_upper cid (Set [staticContext]))
      putConstraint (C_invoke cty m atyes effm aty)
      return (aty, eff)
      
mkActionExpr (ConstTrue) = do
  return $ actionConst (TypeName "boolean")

mkActionExpr (ConstFalse) = do
  return $ actionConst (TypeName "boolean")

mkActionExpr (ConstNull) = do
  return $ actionConst (TypeName "null")
      
mkActionExpr (ConstNum s) = do
  return $ actionConst (TypeName "int")
  
mkActionExpr (ConstLit s label) = do  
  return $ actionConst (TypeName "String")
  where
    actionLit :: Label -> ActionExpr
    actionLit label typingenv info context = do
      aty <- mkAnnoType (TypeName "String")
      let id = getAnno aty
      putConstraint (C_lower (Set [addContext length_k context label]) id)
      return (aty, noEffect)
      

mkActionExpr (ConstChar s) = do  
  return $ actionConst (TypeName "char")

mkActionExpr (Prim n es) = do
  return $ actionConst (TypeName "char")

actionConst ty typingenv info context = do
  aty <- mkAnnoType ty
  return (aty, noEffect)

      

--      
mkCast :: TypeName -> AnnoType -> AnnoType
mkCast (TypeName c)       (AnnoType d id)        = AnnoType c id
mkCast (TypeName c)       (AnnoArrayType aty id) = AnnoType c id
mkCast (ArrayTypeName ty) (AnnoArrayType aty id) = AnnoArrayType (mkCast ty aty) id