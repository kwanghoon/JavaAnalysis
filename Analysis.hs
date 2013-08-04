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

type AnalysisState = Either Constraints TypingTable -- Can be extended if necessary

-- ActionMethod (a function type)
--   Input
--   - Info: program inforamtion such as class hierarchies
--   - TypingTable: the table entries generated until now
--   - Context: the context for this action
--
--   Output
--   - Constraints: the generated constraints
--   - TypingTable: the newly generated table entries to be added
type ActionMethod = 
  Info 
  -> TypingTable 
  -> Context 
  -> StateT AnalysisState IO ()
              
type ActionStmt = 
  TypingEnv
  -> TypingCtx
  -> Info
  -> TypingTable
  -> Context
  -> StateT AnalysisState IO TypingEnv
  
type ActionExpr =  
  TypingEnv
  -> Info
  -> TypingTable
  -> Context
  -> StateT AnalysisState IO AnnoType
  
type MethodIdentifier = (ClassName, MethodName, UniqueId)
  
type ActionLookupTable = [(MethodIdentifier, ActionMethod)]

--
doAnalysis :: Program -> Info -> IO ()
doAnalysis program info = return ()

setup :: WorkList -> (TypingTable, Constraints)
setup worklist = ([], [])

solve :: Constraints -> Constraints
solve constraints = constraints

--
doWork :: Info 
          -> ActionLookupTable 
          -> WorkList 
          -> TypingTable 
          -> (WorkList, TypingTable, Constraints)
doWork info actionlookuptable worklist typingtable =
  (worklist, typingtable, [])

doAction :: MethodIdentifier 
            -> ActionLookupTable 
            -> Info
            -> TypingTable 
            -> Context
            -> (WorkList, TypingTable, Constraints)
doAction methodidnetifier actionlookuptable info typingtable context =
  ([], typingtable, [])
  
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
    mkaction actionstmt typingenv typingctx info typingtable context = do
      _ <- actionstmt typingenv typingctx info typingtable context
      return ()
      
mkActionMDecl (n, p, is) (ConstrDecl m id argdecls stmt) = do
  actionstmt <- mkActionStmt stmt
  return [((n,m,id),mkaction actionstmt [] (n, p, is, Just m))]
  where
    mkaction actionstmt typingenv typingctx info typingtable context = do
      _ <- actionstmt typingenv typingctx info typingtable context
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
    mkaction actionexpr typingenv typingctx info typingtable context = do
      _ <- actionexpr typingenv info typingtable context
      return typingenv
  
mkActionStmt (NoStmt) = do
  -- Some constratins are generated.
  -- No new typing table entries are generated.
  return actionext
  where
    actionext typingenv typingctx info typingtable context = do
      -- put emptyTypingTable
      return typingenv
      
mkActionStmt (Seq stmt1 stmt2) = do
  actionstmt1 <- mkActionStmt stmt1
  actionstmt2 <- mkActionStmt stmt2
  return $ mkaction actionstmt1 actionstmt2
  where
    mkaction actionstmt1 actionstmt2 typingenv typingctx info typingtable context = do
      typingenv1 <- actionstmt1 typingenv typingctx info typingtable context
      typingenv2 <- actionstmt2 typingenv1 typingctx info typingtable context
      return typingenv2
    
mkActionExpr :: Expr -> IO ActionExpr
mkActionExpr (Var x) = do
  return mkactionexpr
  where
    mkactionexpr typingenv info typingtable context = do
      let maybet = lookupEnv typingenv x
      if isNothing maybet
        then do actionexpr <- liftIO $ mkActionExpr (Field (Var "this") x)
                actionexpr typingenv info typingtable context
        else return $ fromJust maybet
            
