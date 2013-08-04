module Analysis where

import AST
-- import TypedAST
import Library
import Data.Maybe
import Control.Monad.Trans (liftIO)
--import Control.Monad.Writer (WriterT, tell, runWriterT)
import Control.Monad.State 

-- Constraints
type Constraints = [Constraint]
data Constraint  = 
    C1 Set UniqueId        -- { r1, ... , rn } \subseteq Xi
  | C2 UniqueId UniqueId   -- Xj \subseteq Xi

-- Types annotated with a set of contexts
data AnnoType = 
    AnnoType Name UniqueId           -- C{Xi}
  | AnnoArrayType AnnoType UniqueId  -- C[]{Xi}[]{Xj}

--
type TypingTable = [TableEntry]
data TableEntry = 
    -- F(C,ctx,f)=Xi
    F ClassName Context FieldName AnnoType  
    
    -- M(C,ctx,m,k)= (Xi1, ... , Xin) ==> Xj
  | M ClassName Context MethodName UniqueId [AnnoType] AnnoType 
    
    -- V(C,ctx,m,k,x,j) = Xi
  | V ClassName Context MethodName UniqueId VarName UniqueId AnnoType
    
emptyTypingTable = []    
unionTypingTable t1 t2 = t1 ++ t2
    
-- WorkList
type WorkList = [(ClassName,Context,MethodName,UniqueId)]

isWorklistEmpty [] = True
isWorklistEmpty _  = False

nextFromWorklist l = (head l, tail l)


type AnalysisState = (WorkList, Constraints, TypingTable) -- Can be extended if necessary

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
  -> StateT AnalysisState IO TypingEnv
  
type ActionExpr =  
  TypingEnv
  -> Info
  -> Context
  -> StateT AnalysisState IO AnnoType
  
type MethodIdentifier = (ClassName, MethodName, UniqueId)
  
type ActionLookupTable = [(MethodIdentifier, ActionMethod)]

lookupActionTable alist x =  -- TODO: Excerpted from TypeCheck.hs
  case [ t | (y,t) <- alist, x == y ] of
    []    -> Nothing
    (t:_) -> Just t

--
doAnalysis :: Program -> Info -> IO ()
doAnalysis program info = return ()

doAnalysis' program info = do
  let worklist    = [] :: WorkList
  let typingtable = [] :: TypingTable
  let constraints = [] :: Constraints
  let state       = (worklist, constraints, typingtable)
  actionlookuptable <- mkActionProgram program
  (_, state1) <- runStateT (doWork info actionlookuptable) state
  let (worklist1, constraints1, typingtable1) = state1
  (_, constraints2) <- runStateT doSolve constraints1
  putStrLn "doAnalysis..."

doSolve :: StateT Constraints IO ()
doSolve = liftIO $ putStrLn "dosolve..."

--
doWork :: Info -> ActionLookupTable -> StateT AnalysisState IO ()
doWork info actionlookuptable = do
  -- Repeate the following execution
  --  1. Get a piece of work from worklist
  --  2. Get an action for the work from actionlookuptable
  --  3. Do the action and obtain a list of typing table and a set of constraints
  --  4. Update the memorized typing table with the list
  --  5. Also, update the worklist if there is any new typing table entry
  --  6. Add the set of constraints to the accumulated one
  
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
            
getWorklist :: StateT AnalysisState IO WorkList
getWorklist = do
  (worklist,_,_) <- get
  return worklist
  
putWorklist :: WorkList -> StateT AnalysisState IO ()
putWorklist worklist = do
  (_,constraints,typingtable) <- get
  put (worklist,constraints,typingtable)

--  
type TypingEnv = [(Name, AnnoType)] -- cf. [(Name, TypeName)] in TypeCheck.hs
type TypingCtx = (ClassName, Maybe ClassName, [ClassName], Maybe MethodName)

lookupEnv tyenv x =  -- TODO: Excerpted from TypeCheck.hs
  case [ t | (y,t) <- tyenv, x == y ] of
    []    -> Nothing
    (t:_) -> Just t

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
      _ <- actionexpr typingenv info context
      return typingenv
  
mkActionStmt (NoStmt) = do
  -- Some constratins are generated.
  -- No new typing table entries are generated.
  return actionext
  where
    actionext typingenv typingctx info context = do
      -- put emptyTypingTable? No.
      return typingenv
      
mkActionStmt (Seq stmt1 stmt2) = do
  actionstmt1 <- mkActionStmt stmt1
  actionstmt2 <- mkActionStmt stmt2
  return $ mkaction actionstmt1 actionstmt2
  where
    mkaction actionstmt1 actionstmt2 typingenv typingctx info context = do
      typingenv1 <- actionstmt1 typingenv typingctx info context
      typingenv2 <- actionstmt2 typingenv1 typingctx info context
      return typingenv2
    
mkActionExpr :: Expr -> IO ActionExpr
mkActionExpr (Var x) = do
  return mkactionexpr
  where
    mkactionexpr typingenv info context = do
      let maybet = lookupEnv typingenv x
      if isNothing maybet
        then do actionexpr <- liftIO $ mkActionExpr (Field (Var "this") x)
                actionexpr typingenv info context
        else return $ fromJust maybet
            
