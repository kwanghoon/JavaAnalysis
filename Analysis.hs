module Analysis where

import AST
import TypedAST
import Library
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell, runWriterT)

type Constraint  = (Ctx, UniqueId)
type Constraints = [Constraint]

doAnalysis :: Program -> Info -> IO ()
doAnalysis program info = do
  typedInfo <- initAnalysis program info
  (_, constraints) <- runWriterT (analysis program typedInfo)
  putStrLn $ show $ constraints

--
initAnalysis :: Program -> Info -> IO TypedInfo
initAnalysis program info = do
  let typedinfo = mkTypedInfo info
  return typedinfo
  
mkTypedInfo info =  
  let ucs  = getUserClasses info
      is   = getInheritance info
      fs   = getFields info
      mtys = getMtype info
      
      typedmtys = [ (c, m, i, tys, ty, attrs, args, maybestmt) 
                  | ((c,m, tys, ty, attrs, args, maybestmt), i) <- zip mtys [1..] ]
  in  (ucs, is, fs, typedmtys)

--
analysis :: Program -> TypedInfo -> WriterT Constraints IO ()
analysis program info = do
  tell [([1,2,3,4,5],6)]
  tell [([1,2],7)]
  liftIO . putStrLn $ "print something..."
  
aClass :: TypedInfo -> WriterT Constraints IO ()  
aClass info = do
  liftIO . return $ ()

aMdecl :: TypedInfo -> (Name, Maybe Name, [Name]) -> MemberDecl -> WriterT Constraints IO ()
aMdecl info triple mdecl = do
  liftIO . return $ ()
  
aExp :: TypedInfo -> (TypingCtx, TypingEnv) -> Expr -> WriterT Constraints IO ()
aExp info env e = do
  liftIO . return $ ()
