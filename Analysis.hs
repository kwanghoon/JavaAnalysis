module Analysis where

import AST
import Library
import Data.Maybe
import Data.List
import Control.Monad.Trans (liftIO)
import Control.Monad.State 

-- Constraints
type Constraints = [Constraint]
data Constraint  = 
    -- { r1, ... , rn } \subseteq Xi
    C1 Set UniqueId        
    
    -- Xj \subseteq Xi
  | C2 UniqueId UniqueId   
    
    -- C_field C{X} f D{Z} = C{X}.f <: D{Z}
  | C_field AnnoType FieldName AnnoType
    
    -- C_fieldassign D Z C X f = D{Z} <: C{X}.f
  | C_fieldassign ClassName UniqueId FieldName ClassName UniqueId

    -- C_invoke C X m [S1,...,Sn] eff T = C{X}.m <: (S1,...,Sn) --eff--> T 
  | C_invoke ClassName UniqueId MethodName UniqueId [AnnoType] Effect AnnoType


-- Types annotated with a set of contexts
data AnnoType = 
    AnnoType Name UniqueId           -- C{Xi}
  | AnnoArrayType AnnoType UniqueId  -- C[]{Xi}[]{Xj}

type AnnoMethodType = ([AnnoType], AnnoType, Effect)
  
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
data Effect = Effect [ClassName]

noEffect = Effect []
unionEffect (Effect eff1) (Effect eff2) = Effect $ nub $ eff1 ++ eff2
    
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
  let uniqueid    = 1  :: UniqueId    
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
getNewid :: StateT AnalysisState IO UniqueId
getNewid = do
  (worklist,constraints,typingtable,uniqueid) <- get
  put (worklist,constraints,typingtable,uniqueid+1)
  return uniqueid
  
getNewids :: [a] -> StateT AnalysisState IO [UniqueId]
getNewids ls = mapM f ls
  where
    f _ = getNewid

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
  id <- getNewid
  return (AnnoType n id)
mkAnnoType (ArrayTypeName ty) = do  
  id <- getNewid
  aty <- mkAnnoType ty
  return (AnnoArrayType aty id)


--  
type TypingEnv = [(Name, AnnoType)] -- cf. [(Name, TypeName)] in TypeCheck.hs
type TypingCtx = (ClassName, Maybe ClassName, [ClassName], Maybe MethodName)

lookupEnv tyenv x =  -- TODO: Excerpted from TypeCheck.hs
  case [ t | (y,t) <- tyenv, x == y ] of
    []    -> Nothing
    (t:_) -> Just t
    
-- lookupFields info c f =     

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
      let effect3 = unionEffect effect1 effect2
      return (typingenv2, effect3)
    
--
mkActionExpr :: Expr -> IO ActionExpr
mkActionExpr (Var x) = return $ actionvar x

mkActionExpr (Field e f maybety) = do
  -- TODO: Error handling in case maybety is Nothing
  return $ actionfield e f (fromJust maybety)

actionvar :: VarName -> ActionExpr
actionvar x typingenv info context = do
  let maybeaty = lookupEnv typingenv x
  -- TODO: Error handling in case maybeaty is Nothing
  return $ (fromJust maybeaty, noEffect)
         
actionfield :: Expr -> FieldName -> TypeName -> ActionExpr
actionfield exp f ty typingenv info context = do
  actionexp  <- liftIO $ mkActionExpr exp
  (atyexp, eff) <- actionexp typingenv info context
  aty <- mkAnnoType ty
  putConstraint (C_field atyexp f aty)
  return (aty, eff)
  
  
            
