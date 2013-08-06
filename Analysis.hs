module Analysis where

import AST
import Library
import Data.Maybe
import Data.List
import Control.Monad.Trans (liftIO)
import Control.Monad.State 

--
type ObjAllocSite = UniqueId
type Context      = [ObjAllocSite]
data Set          = Set [Context]


staticContext = [uniqueidforstatic]

addContext :: Int -> Context -> Label -> Context
addContext k context label = take k $ context ++ [label]

length_k = 1

-- Constraints
type Constraints = [Constraint]
data Constraint  = 
    -- { r1, ... , rn } \subseteq Xi
    C_lower Set UniqueId        
    -- Xi \subseteq { r1, ... , rn }
  | C_upper UniqueId Set 
    
    -- Xj \subseteq Xi
  | C_var UniqueId UniqueId   
    
    -- C_field C{X} f D{Z} = C{X}.f <: D{Z}
  | C_field AnnoType FieldName AnnoType
    
    -- C_assign D Z C X f = D{Z} <: C{X}
  | C_assign AnnoType AnnoType

    -- C_invoke C X m [S1,...,Sn] eff T = C{X}.m <: (S1,...,Sn) --eff--> T 
  | C_invoke AnnoType MethodName [AnnoType] Effect AnnoType
    

-- Types annotated with a set of contexts
data AnnoType = 
    AnnoType Name UniqueId           -- C{Xi}
  | AnnoArrayType AnnoType UniqueId  -- C[]{Xi}[]{Xj}

type AnnoMethodType = ([AnnoType], AnnoType, Effect)

getAnno (AnnoType c id)        = id
getAnno (AnnoArrayType aty id) = id

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

noEffect = Eff []
    
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
  
type ActionExprs =  
  TypingEnv
  -> Info
  -> Context
  -> StateT AnalysisState IO ([AnnoType], [Effect])

type MethodIdentifier = (ClassName, MethodName, UniqueId)
  
type ActionLookupTable = [(MethodIdentifier, ActionMethod)]

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
doAnalysis program info = return ()

doAnalysis' program info = do
  let worklist    = [] :: WorkList
  let typingtable = [] :: TypingTable
  let constraints = [] :: Constraints
  let uniqueid    = initialuniqueid  :: UniqueId    
  let state       = (worklist, constraints, typingtable, uniqueid)
  actionlookuptable <- mkActionProgram program
  (_, state1) <- runStateT (doWork info actionlookuptable) state
  let (worklist1, constraints1, typingtable1, uniqueid1) = state1
  (_, constraints2) <- runStateT doSolve constraints1
  putStrLn "doAnalysis..."

doSolve :: StateT Constraints IO ()
doSolve = liftIO $ putStrLn "dosolve..."

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


--  
type TypingEnv = [(Name, AnnoType)] -- cf. [(Name, TypeName)] in TypeCheck.hs
type TypingCtx = (ClassName, Maybe ClassName, [ClassName], Maybe MethodName)

lookupEnv tyenv x =  -- TODO: Excerpted from TypeCheck.hs
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
  return [((n,m,id), mkaction actionstmt [] (n, p, is, Just m))]
  where
    mkaction actionstmt typingenv typingctx info context = do
      _ <- actionstmt typingenv typingctx info context
      return ()
      
mkActionMDecl (n, p, is) (ConstrDecl m id argdecls stmt) = do
  actionstmt <- mkActionStmt stmt
  return [((n,m,id),mkaction actionstmt [] (n, p, is, Just m))]
  where
    mkaction actionstmt typingenv typingctx info context = do
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
  
mkActionStmt (NoStmt) = do
  -- Some constratins are generated.
  -- No new typing table entries are generated.
  return actionext
  where
    actionext typingenv typingctx info context = do
      -- put emptyTypingTable? No.
      return (typingenv, noEffect)
      
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
    
--
mkActionExpr :: Expr -> IO ActionExpr
mkActionExpr (Var x) = 
  return $ actionVar x
  where
    actionVar :: VarName -> ActionExpr
    actionVar x typingenv info context = do
      let maybeaty = lookupEnv typingenv x
      -- TODO: Error handling in case maybeaty is Nothing
      return $ (fromJust maybeaty, noEffect)

mkActionExpr (Field e f maybety) = do
  -- TODO: Error handling in case maybety is Nothing. maybety is some type by TypeCheck.hs.
  return $ actionField e f (fromJust maybety)
  where
    actionField :: Expr -> FieldName -> TypeName -> ActionExpr
    actionField exp f ty typingenv info context = do
      actionexp  <- liftIO $ mkActionExpr exp
      (atyexp, eff) <- actionexp typingenv info context
      aty <- mkAnnoType ty
      putConstraint (C_field atyexp f aty)
      return (aty, eff)

mkActionExpr (StaticField c f maybety) =   
  -- TODO: Error handling in case maybety is Nothing. maybety is some type by TypeCheck.hs.
  return $ actionStaticfield c f (fromJust maybety)
  where
    actionStaticfield :: TypeName -> Name -> TypeName -> ActionExpr  
    actionStaticfield c f ty typingenv info context = do
      let maybefty    = lookupFields info c f
      let (fty,attrs) = fromJust maybefty
      -- TODO: Error handling in case maybefty is Nothing or attrs does not have static. maybefty is some type by TypeCheck.hs
      cty <- mkAnnoType c
      aty <- mkAnnoType ty
      let cid = getAnno cty
      putConstraint (C_upper cid (Set [staticContext]))
      putConstraint (C_field cty f aty)
      return (aty, noEffect)
  
mkActionExpr (New c es label) = 
  return $ actionNew c es label
  where
    actionNew :: TypeName -> [Expr] -> Label -> ActionExpr
    actionNew c es label typingenv info context = do
      (atys, effs) <- mkActionExprs es typingenv info context
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
      
-- mkActionExpr (Assign (Prim "[]" es1) e2) =
-- mkActionExpr (Assign (StaticField c f maybety) e2) =
-- mkActionExpr (Assign (Field e1 f maybety) e2) =
mkActionExpr (Assign e1@(Var x) e2) =
  return $ actionAssign e1 e2
  where
    actionAssign :: Expr -> Expr -> ActionExpr
    actionAssign e1 e2 typingenv info context = do
      actionexp1  <- liftIO $ mkActionExpr e1 
      actionexp2  <- liftIO $ mkActionExpr e2 
      (aty1,eff1) <- actionexp1 typingenv info context 
      (aty2,eff2) <- actionexp2 typingenv info context
      
      avoidty <- mkAnnoType (TypeName "void")
      let eff =  EffUnion eff1 eff2
      return (avoidty, eff)
      
-- In mkActionExpr (Assign e1 e2), e1 can't be the others
-- because the typechecker has already filtered such an illegal program.
      
mkActionExprs :: [Expr] -> ActionExprs
mkActionExprs es typingenv info context = do
  actionexps <- liftIO $ mapM mkActionExpr es
  atyEffs    <- mapM (\actionexp -> actionexp typingenv info context) actionexps
  let (atys, effs) = unzip atyEffs
  return (atys, effs)
  