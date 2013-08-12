module Analysis where

import AST
import Library
import Subtyping
import Data.Maybe
import Data.List
import Control.Monad.Trans (liftIO)
import Control.Monad.State 

--
doAnalysis :: Program -> Info -> IO ()
doAnalysis program info = do
  let worklist    = [] :: WorkList
  let typingtable = [] :: TypingTable
  let constraints = [] :: Constraints
  let alloctable  = [] :: AllocLabelTable
  let allocobjs   = [] :: AllocObjs
  let uniqueid    = initialuniqueid  :: UniqueId    
  let state       = 
        (worklist, constraints, typingtable, alloctable, allocobjs, uniqueid)
  actionlookuptable <- mkActionProgram info program
  prActionLookupTable info [] actionlookuptable state
  -- (_, state1) <- runStateT (doWork info actionlookuptable) state
  -- let (worklist1, constraints1, typingtable1, uniqueid1) = state1
  -- (_, constraints2) <- runStateT doSolve constraints1

--
type ObjAllocSite = UniqueId
type Context      = [ObjAllocSite]
data Set          = Set [Context] deriving Eq

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
addContext k context label = reverse $ take k $ reverse $ context ++ [label]

isEmptyContext [] = True
isEmptyContext _  = False

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
      
mkVoidType = do
  aty <- mkAnnoType (TypeName "void")
  let id = getAnno aty
--  putConstraint (C_upper id (Set []))
  putConstraint (C_lower (Set []) id)
  return aty
  
showMethodtype atys aty eff = concat $
  ["("] ++ comma (map show atys) ++ [ ")", "--", show eff, "-->", show aty]

-- Effects
data Effect = Eff [ClassName] | EffVar UniqueId | EffUnion Effect Effect

instance Show Effect where
  showsPrec p (Eff cs)    = conc $ [ "{" ] ++ comma cs ++ [ "}" ]
  showsPrec p (EffVar id) = conc [ effect_var_prefix, show id ]
  showsPrec p (EffUnion eff1 eff2) = conc [ show eff1, " ", "U", " ", show eff2 ]

noEffect = Eff []

-- Constraints
type Constraints = [Constraint]
data Constraint  = 
    -- { r1, ... , rn } \subseteq Xi
    C_lower Set UniqueId        
    -- Xi \subseteq { r1, ... , rn }
  -- | C_upper UniqueId Set 
    
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
    
    -- C_effect eff1 eff2               ===> eff1 <= eff2
  | C_effect Effect UniqueId
    
  | C_mtype [AnnoType] Effect AnnoType [AnnoType] Effect AnnoType

constraint_var_prefix = "x"
effect_var_prefix = "e"

instance Show Constraint where
  showsPrec p (C_lower set id) = 
    conc $ [ show set, " ", "<=", " ", constraint_var_prefix, show id ]
  -- showsPrec p (C_upper id set) = 
  --   conc $ [ constraint_var_prefix, show id, " ", "=", " ", show set ]
  showsPrec p (C_var x y) = 
    conc [ constraint_var_prefix, show x, " ", "<=", " ", constraint_var_prefix, show y ]
  showsPrec p (C_field aty1 f aty2) = 
    conc [ show aty1, ".", f, " ", "<:", " ", show aty2 ]
  showsPrec p (C_assign aty1 aty2) = 
    conc [ show aty1, " ", "<:", " ", show aty2 ]
  showsPrec p (C_assignfield aty1 aty2 f) = 
    conc [ show aty1, " ", "<:", " ", show aty2, ".", f ]
  showsPrec p (C_invoke aty1 m atys2 eff aty2) = 
    conc [ show aty1, ".", m, " ", "<:", " ", showMethodtype atys2 aty2 eff ]
  showsPrec p (C_effect eff id) = 
    conc [ show eff, " ", "<=", " ", show (EffVar id) ]
  showsPrec p (C_mtype atys1 eff1 aty1 atys2 eff2 aty2) =
    conc [ showMethodtype atys1 eff1 aty1, " ", "<:", " ", showMethodtype atys2 eff2 aty2 ]
    
-- A very^k inefficient constraint solving method
-- TODO: To replace this with union-find algorithm
solve :: Constraints -> [(UniqueId, Set)]
solve constraints = nochange $ rep (solveAll constraints) []
  where
    rep f x = x : rep f (f x)
    nochange (cenv1:cenv2:cenvs) = 
      if cenv1==cenv2 then cenv1 else nochange (cenv2:cenvs)
  
solveAll constraints cenv = foldr solveOne cenv constraints

solveOne (C_lower set x) cenv = 
  (x, foldr unionSet set [ set' | (x',set') <- cenv, x==x' ]) 
  : [ (x', set') | (x',set') <- cenv, x/=x' ]
solveOne (C_var x1 x2) cenv =
  (x2, foldr unionSet set1 [ set' | (x',set') <- cenv, x2==x' ])
  : [ (x',set') | (x',set') <- cenv, x2/=x' ]
  where
    set1 = lookupCEnv cenv x1
solveOne (C_assign (AnnoType _ x) (AnnoType _ y)) cenv = 
  solveOne (C_var x y) cenv
solveOne (C_assign (AnnoArrayType aty x) (AnnoType _ y)) cenv = 
  solveOne (C_var x y) cenv
solveOne (C_assign (AnnoType "null" x1) (AnnoArrayType aty2 x2)) cenv = 
  solveOne (C_var x1 x2) cenv
solveOne (C_assign (AnnoArrayType aty1 x1) (AnnoArrayType aty2 x2)) cenv = 
  solveOne (C_var x1 x2) cenv
solveOne (C_effect eff1 eff2) cenv =  -- TODO: handling effects
  cenv
solveOne (C_mtype atys1 eff1 aty1 atys2 eff2 aty2) cenv = 
  solveOne (C_assign aty1 aty2)
  -- $ solveOne (C_effect eff1 eff2) -- TODO: handling effects
  $ foldr f cenv (zip atys1 atys2)
  where
    f (aty1,aty2) cenv = solveOne (C_assign aty2 aty1) cenv
solveOne (C_field _ _ _) cenv = cenv    
solveOne (C_assignfield _ _ _) cenv = cenv
solveOne (C_invoke _ _ _ _ _) cenv = cenv
solveOne c cenv = error (show c)
    

unionSet (Set s1) (Set s2) = Set $ nub (s1 ++ s2)

lookupCEnv cenv x = foldr unionSet (Set [])
  [ set' | (x',set') <- cenv, x==x' ]

--
type TypingTable = [TableEntry]
data TableEntry = 
    -- F(C,ctx,f)=Xi
    F ClassName Context FieldName UniqueId AnnoType  
    
    -- M(C,ctx,m,k)= (Xi1, ... , Xin) ==> Xj
  | M ClassName Context MethodName UniqueId [AnnoType] AnnoType Effect
    
    -- V(C,ctx,m,k,x,j) = Xi
  | V ClassName Context (Maybe (MethodName, UniqueId)) VarName UniqueId AnnoType
    
instance Show TableEntry where    
  showsPrec p (F c context f id aty) = 
    conc [ " - ", c, "{", showContext context, "}", ".", f, ",", show id, " = ", show aty ]
  showsPrec p (M c context m id atys aty eff) = 
    conc [ " - ", c, "{", showContext context, "}"
         , ".", m, ",", show id, " = ", showMethodtype atys aty eff ]
  showsPrec p (V c context maybemid v vid aty) =
    conc [ " - ", c, "{", showContext context, "}" ] .
    showMaybemid maybemid . conc [ ",", v, ",", show vid, " = ", show aty ]
    
showMaybemid (Just (m,id)) = conc [ ".", m, ",", show id ]
showMaybemid (Nothing)     = conc [ "" ]
    
emptyTypingTable = []    
unionTypingTable t1 t2 = t1 ++ t2

prTypingTable typingtable = do
  putStrLn "Typing table:"
  mapM_ pr $ reverse $ typingtable
  where
    pr entry = putStrLn $ show $ entry    

--
type TypingEnv = [(Name, AnnoType)] -- cf. [(Name, TypeName)] in TypeCheck.hs
type TypingCtx = (ClassName, Maybe ClassName, [ClassName], Maybe (MethodName, UniqueId))

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

getClassFromTypingCtx (c,_,_,_) = c
getMethodFromTypingCtx (_,_,_,maybemid) = maybemid

-- WorkList
type WorkList = [(ClassName,Context,MethodName,UniqueId)]

isWorklistEmpty [] = True
isWorklistEmpty _  = False

nextFromWorklist l = (head l, tail l)

-- A Table for Unique Allocation Labels
type AllocLabelTable = [(UniqueId, AllocLabelLocation, ClassName)]
-- type AllocLabelLocation = (ClassName, Context, MethodName, UniqueId, Label)
type AllocLabelLocation = (ClassName, MethodName, UniqueId, Label)

type AllocObjs = [Context]

--
type AnalysisState = -- Can be extended if necessary
  (WorkList, Constraints, TypingTable, AllocLabelTable, AllocObjs, UniqueId)

newId :: StateT AnalysisState IO UniqueId
newId = do
  (worklist,constraints,typingtable,alloctable,allocobjs,uniqueid) <- get
  put (worklist,constraints,typingtable,alloctable,allocobjs,uniqueid+1)
  return uniqueid
  
newIds :: [a] -> StateT AnalysisState IO [UniqueId]
newIds ls = mapM f ls
  where
    f _ = newId
    
newEffVar :: StateT AnalysisState IO Effect
newEffVar = do
  (worklist,constraints,typingtable,alloctable,allocobjs,uniqueid) <- get
  put (worklist,constraints,typingtable,alloctable,allocobjs,uniqueid+1)
  return (EffVar uniqueid)

getWorklist :: StateT AnalysisState IO WorkList
getWorklist = do
  (worklist,_,_,_,_,_) <- get
  return worklist
  
putWorklist :: WorkList -> StateT AnalysisState IO ()
putWorklist worklist = do
  (_,constraints,typingtable,alloctable,allocobjs,uniqueid) <- get
  put (worklist,constraints,typingtable,alloctable,allocobjs,uniqueid)
  
getConstraints :: StateT AnalysisState IO Constraints
getConstraints = do
  (_,constraints,_,_,_,_) <- get
  return constraints
  
resetConstraints :: Constraints -> StateT AnalysisState IO ()
resetConstraints constraints = do
  (worklist,_,typingtable,alloctable,allocobjs,uniqueid) <- get
  put (worklist,constraints,typingtable,alloctable,allocobjs,uniqueid)

putConstraint :: Constraint -> StateT AnalysisState IO ()
putConstraint constraint = do
  (worklist,constraints,typingtable,alloctable,allocobjs,uniqueid) <- get
  put (worklist,constraint:constraints,typingtable,alloctable,allocobjs,uniqueid)
  
resolveConstraint ::  Constraint -> StateT AnalysisState IO [Constraint]
resolveConstraint (C_assignfield aty1 aty2 f) = do
  (_,constraints,typingtable,_,_,_) <- get
  let cid  = getAnno aty2
  let cenv = solve constraints
  let Set ctxs = lookupCEnv cenv cid
  let c    = case aty2 of { AnnoType c _ -> c; _ -> "" } -- TODO: something missing
  let atys = [ aty' | F c' context' f' id' aty' <- typingtable
                    , context <- ctxs
                    , c==c' && context==context' && f==f' ]
             
  -- liftIO $ putStrLn $ "resolveConstraint:"
  -- liftIO $ putStrLn $ show (C_assignfield aty1 aty2 f)
  -- liftIO $ putStrLn $ show [ C_assign aty1 aty | aty <- atys ]
  -- liftIO $ putStrLn $ show cenv
  -- liftIO $ putStrLn $ show constraints
  
  return $ [ C_assignfield aty1 aty2 f ] ++ [ C_assign aty1 aty | aty <- atys ]
  
resolveConstraint (C_field aty1 f aty2) = do
  (_,constraints,typingtable,_,_,_) <- get
  let cid  = getAnno aty1
  let cenv = solve constraints
  let Set ctxs = lookupCEnv cenv cid
  let c    = case aty1 of { AnnoType c _ -> c; _ -> "" } -- TODO: something missing
  let atys = [ aty' | F c' context' f' id' aty' <- typingtable
                    , context <- ctxs
                    , c==c' && context==context' && f==f' ]
             
  -- liftIO $ putStrLn $ "resolveConstraint:"
  -- liftIO $ putStrLn $ show (C_field aty1 f aty2)
  -- liftIO $ putStrLn $ show [ C_assign aty aty2 | aty <- atys ]
  -- liftIO $ putStrLn $ show cenv
  -- liftIO $ putStrLn $ show constraints
  
  return $ [ C_field aty1 f aty2 ] ++ [ C_assign aty aty2 | aty <- atys ]
  
resolveConstraint (C_invoke cty m atys eff aty) = do
  (_,constraints,typingtable,_,_,_) <- get
  let cid = getAnno cty
  let cenv = solve constraints
  let Set ctxs = lookupCEnv cenv cid
  let mtypes =
        [ (matys, maty, meff) | M c' context' m' id' matys maty meff <- typingtable
                              , context <- ctxs
                              , context==context' && m==m' ] -- TODO: any condition on c'?

  -- liftIO $ putStrLn $ "resolveConstraint:"
  -- liftIO $ putStrLn $ show (C_invoke cty m atys eff aty)
  -- liftIO $ putStrLn $ show [ C_mtype matys meff maty atys eff aty | (matys, maty, meff) <- mtypes ]
  -- liftIO $ putStrLn $ show cenv
  -- liftIO $ putStrLn $ show constraints

  return $ [ C_invoke cty m atys eff aty ] ++ [ C_mtype matys meff maty atys eff aty | (matys, maty, meff) <- mtypes ]
  
getTyping :: StateT AnalysisState IO TypingTable
getTyping = do
  (_,_,typing,_,_,_) <- get
  return typing
  
putTyping :: TableEntry -> StateT AnalysisState IO ()
putTyping typing = do
  (worklist,constraints,typingtable,alloctable,allocobjs,uniqueid) <- get
  put (worklist,constraints,typing:typingtable,alloctable,allocobjs,uniqueid)
                              
getFieldtyping :: ClassName -> Context -> FieldName -> UniqueId -> StateT AnalysisState IO (Maybe AnnoType)
getFieldtyping c ctx f id = do
  (worklist,constraints,typingtable,alloctable,allocobjs,uniqueid) <- get
  retMaybeAnnotype $ lookupFieldTyping typingtable c ctx f id
  
  where
    retMaybeAnnotype []  = return Nothing
    retMaybeAnnotype [h] = return $ Just h
    retMaybeAnnotype _   = do 
      liftIO $ putStrLn $ 
        "getFieldtyping: duplicate field typings for " 
        ++ show (c,ctx,f)
      return Nothing
                              
getMethodtyping :: ClassName -> Context -> MethodName -> UniqueId 
                   -> StateT AnalysisState IO (Maybe AnnoMethodType)
getMethodtyping c ctx m id = do
  (worklist,constraints,typingtable,alloctable,allocobjs,uniqueid) <- get
  retMaybeAnnomtype $ lookupMethodTyping typingtable c ctx m id
  
  where
    retMaybeAnnomtype []  = return Nothing
    retMaybeAnnomtype [h] = return $ Just h
    retMaybeAnnomtype _   = do
      liftIO $ putStrLn $ "getMethodtyping: duplicate method typings for "
        ++ show (c,ctx,m,id)
      return Nothing

getVartyping :: ClassName -> Context -> Maybe (MethodName,UniqueId) -> VarName 
                -> UniqueId -> StateT AnalysisState IO (Maybe AnnoType)
getVartyping c ctx maybemid v vid = do
  (worklist,constraints,typingtable,alloctable,allocobjs,uniqueid) <- get
  retMaybeAnnovtype $ lookupVarTyping typingtable c ctx maybemid v vid
  
  where
    retMaybeAnnovtype []  = return Nothing
    retMaybeAnnovtype [h] = return $ Just h
    retMaybeAnnovtype _   = do 
      liftIO $ putStrLn $ "getVartyping: duplicate variable typings for "
        ++ show (c,ctx,maybemid,v,vid)
      return Nothing
      
getArgVartyping c ctx m id argdecls = do      
  mapM g argdecls
  where
    g (ty,argv,argid) = getVartyping c ctx (Just (m,id)) argv argid
                               
lookupAllocTableEntry :: AllocLabelLocation -> StateT AnalysisState IO (Maybe ClassName)
lookupAllocTableEntry  entry = do
  (_,_,_,alloctable,_,_) <- get
  case [ newc' | (id',entry',newc') <- alloctable, entry==entry' ] of
    []    -> return $ Nothing
    (h:_) -> return $ Just h

putAllocTableEntry :: Context -> AllocLabelLocation -> ClassName -> StateT AnalysisState IO Context
putAllocTableEntry context entry@(cname,m,mid,label) newc = do
  alloctable <- registerAllocTableEntry entry newc
  let f allocsiteid = 
        head [ entry | (allocsiteid', entry, newc) <- alloctable
                      , allocsiteid==allocsiteid' ]
  let entrys = map f context
  let newentrys = take length_k $ entrys ++ [entry]
  let g entry = 
        head [ allocsiteid | (allocsiteid,entry',newc) <- alloctable
                           , entry==entry' ]
  let newContext = map g newentrys
  putAllocLabelTable alloctable
  putAllocObj newContext
  return newContext
  
registerAllocTableEntry entry newc = do  
  alloctable <- getAllocLabelTable
  case [ (id,entry',newc) 
       | (id,entry',newc) <- alloctable, entry==entry' ] of
    (h:t) -> return alloctable
    []    -> do id <- newId
                return ((id,entry,newc):alloctable)
  
duplicateCheck (h:_) context entry newc alloctable = return (h, alloctable)
  
getAllocLabelTable :: StateT AnalysisState IO AllocLabelTable
getAllocLabelTable = do
  (_,_,_,alloctable,_,_) <- get
  return alloctable
  
putAllocLabelTable :: AllocLabelTable -> StateT AnalysisState IO ()  
putAllocLabelTable alloctable = do
  (worklist,constraints,typingtable,_,allocobjs,uniqueid) <- get
  put (worklist,constraints,typingtable,alloctable,allocobjs,uniqueid)

prAllocLabelTable :: AllocLabelTable -> IO ()  
prAllocLabelTable alloctable = do
  putStrLn "Alloc Labels"
  mapM_ pr $ reverse $ alloctable
  where
    pr (id, (c,m,mid,label), newc) = 
      putStrLn $ " - " ++ obj_alloc_site_prefix ++ show id ++ " : " ++ 
      "(" ++ c ++","++ m ++ "," ++ show mid ++ "," ++ show label ++ ")" ++
      " " ++ "new" ++ " " ++ newc
      
getAllocObjs :: StateT AnalysisState IO AllocObjs
getAllocObjs = do
  (_,_,_,_,allocobjs,_) <- get
  return allocobjs

putAllocObj :: Context -> StateT AnalysisState IO ()
putAllocObj allocobj = do
  (worklist,constraints,typingtable,alloctable,allocobjs,uniqueid) <- get
  let allocobjs' = if allocobj `elem` allocobjs then allocobjs
                   else allocobj:allocobjs
  put (worklist,constraints,typingtable,alloctable,allocobjs',uniqueid)

prAllocObjs :: AllocObjs -> IO ()
prAllocObjs allocobjs = do
  putStrLn "Allocated Objects"
  mapM_ pr $ reverse $ allocobjs
  where
    pr context = putStrLn $ " - " ++ showContext context

-- 
type ActionMember = 
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
  -> TypingCtx
  -> Info
  -> Context
  -> StateT AnalysisState IO (AnnoType, Effect)
  
data ActionIdentifier = 
    MethodActionId ClassName MethodName UniqueId
  | FieldActionId ClassName FieldName UniqueId
  | VarActionId ClassName (Maybe (MethodName, UniqueId)) VarName UniqueId
    deriving Eq
  
type ActionLookupTable = [ActionLookupTableEntry]
type ActionLookupTableEntry = (ActionIdentifier, ActionMember)

lookupActionTable alist x =  -- TODO: Excerpted from TypeCheck.hs
  case [ t | (y,t) <- alist, x == y ] of
    []    -> Nothing
    (t:_) -> Just t
    
lookupFieldTyping :: TypingTable -> ClassName -> Context -> FieldName -> UniqueId -> [AnnoType]
lookupFieldTyping typingtable c ctx f id =
  [ atyi | F ci ctxi fi idi atyi <- typingtable
         , c==ci && ctx==ctxi && f==fi && id==idi ]

lookupMethodTyping :: TypingTable -> ClassName -> Context -> MethodName -> UniqueId 
                      -> [AnnoMethodType]
lookupMethodTyping typingtable c ctx m id =
  [ (atysi, atyi, eff) 
  | M ci ctxi mi idi atysi atyi eff <- typingtable
  , c==ci && ctx==ctxi && m==mi && idi==id ]

lookupVarTyping :: TypingTable -> ClassName -> Context -> Maybe (MethodName,UniqueId) -> VarName -> UniqueId -> [AnnoType]
lookupVarTyping typingtable c ctx maybemid v vid = 
  [ atyi | V ci ctxi maybemidi vi vidi atyi <- typingtable, 
    c==ci && ctx==ctxi && maybemid==maybemidi && v==vi && vid==vidi ]
  
--
prActionLookupTable :: Info -> Context -> ActionLookupTable -> AnalysisState -> IO ()
prActionLookupTable info context actionlookuptable initstate = do
  putStrLn $ "Action lookup table for " ++ showContext context
  (_,state) <- runStateT (repRun info context actionlookuptable) initstate
  -- (_,state) <- runStateT (runAll info context actionlookuptable) initstate
  let (_,_,typingtable,alloctable,allocobjs,_) = state
  prTypingTable typingtable
  prAllocLabelTable alloctable
  prAllocObjs allocobjs

repRun :: Info -> Context -> ActionLookupTable -> StateT AnalysisState IO ()
repRun info context actionlookuptable = do
  runAll info context actionlookuptable
  alloctable <- getAllocLabelTable
  allocobjs   <- getAllocObjs
  -- liftIO $ prAllocLabelTable alloctable
  -- liftIO $ prAllocObjs allocobjs
  repRun' 1 info actionlookuptable

repRun' :: Int -> Info -> ActionLookupTable -> StateT AnalysisState IO ()
repRun' n info actionlookuptable = do
  -- liftIO $ putStrLn $ "repRun: " ++ show n
  allocobjs1 <- getAllocObjs
  runAllContext allocobjs1
  alloctable <- getAllocLabelTable
  allocobjs2 <- getAllocObjs
  -- liftIO $ prAllocLabelTable alloctable
  -- liftIO $ prAllocObjs allocobjs2
  if length allocobjs1 == length allocobjs2 then return ()
    else repRun' (n+1) info actionlookuptable
         
  where
    runAllContext = foldr f (return ())
    f ctx m = do runAll info ctx actionlookuptable
                 m
    
runAll :: Info -> Context -> ActionLookupTable -> StateT AnalysisState IO ()
runAll info context actionlookuptable = 
  foldr (runOneEntry info context) (return()) actionlookuptable
    
runOneEntry :: Info -> Context -> ActionLookupTableEntry 
               -> StateT AnalysisState IO () 
               -> StateT AnalysisState IO ()
runOneEntry info context entry m = do 
  prActionLookupTableEntry info context entry
  m
    
-- prActionLookupTable :: Info -> Context -> ActionLookupTable -> AnalysisState -> IO ()
-- prActionLookupTable info context actionlookuptable initstate = do
--   putStrLn $ "Action lookup table for " ++ showContext context
--   (_,state) <- runStateT (runAll actionlookuptable) initstate
--   let (_,_,typingtable,alloctable,_) = state
--   prTypingTable typingtable
--   prAllocLabelTable alloctable
--   where
--     runAll actionlookuptable = foldr runOneEntry (return()) actionlookuptable
    
--     runOneEntry :: ActionLookupTableEntry 
--          -> StateT AnalysisState IO () 
--          -> StateT AnalysisState IO ()
--     runOneEntry entry m = do 
--       prActionLookupTableEntry info context entry
--       m

prActionLookupTableEntry info context (MethodActionId c m id, actionmethod) = do
  constraints0 <- getConstraints
  resetConstraints []
  liftIO $ putStrLn $ concat [c, ",", m, ",", show id, "(", showContext context, ")", ":"]
  _ <- actionmethod info context
  constraints <- getConstraints
  liftIO $ mapM_ prConstraint (reverse constraints) -- reverse for better readability
  resetConstraints (constraints ++ constraints0)
      
prActionLookupTableEntry info context (FieldActionId c f id, actionfield) = do
  constraints0 <- getConstraints
  resetConstraints []
  liftIO $ putStrLn $ concat [c, ",", f, ",", show id, "(", showContext context, ")", ":"]
  _ <- actionfield info context
  constraints <- getConstraints
  liftIO $ mapM_ prConstraint (reverse constraints) -- reverse for better readability
  resetConstraints (constraints ++ constraints0)
      
prActionLookupTableEntry info context (VarActionId c maybemid x id, actionvar) = do      
  constraints0 <- getConstraints
  resetConstraints []
  liftIO $ putStrLn $ concat $ [c, ","] ++ tostr maybemid ++ [x, ",", show id]
  _ <- actionvar info context
  constraints <- getConstraints
  liftIO $ mapM_ prConstraint (reverse constraints)
  resetConstraints (constraints ++ constraints0)
  where
    tostr Nothing       = []
    tostr (Just (m,id)) = [m, ",", show id, ","]
    

prConstraint constraint = do
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
  let maybeaction = lookupActionTable actionlookuptable (MethodActionId c m id)
  let action = fromJust maybeaction
  if isNothing maybeaction 
    then liftIO $ putStrLn $ "Error: Can't find an action for: " ++ show (c,ctx,m,id)
    else do action info ctx
            doWork info actionlookuptable
            
doSolve :: StateT Constraints IO ()
doSolve = liftIO $ putStrLn "dosolve..."

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
mkActionProgram :: Info -> Program -> IO ActionLookupTable
mkActionProgram info program = do
  actionmethodss <- mapM (mkActionClass info) program
  return $ concat $ actionmethodss
    
mkActionClass :: Info -> Class -> IO ActionLookupTable
mkActionClass info (Class attrs n p is mdecls) = do
  actionmethodss <- mapM (mkActionMDecl (n, p, is)) mdecls
  actionvars     <- mkActionVar (n, p, is) (getVtypes info)
  return $ wrapFieldInit (concat actionmethodss) actionvars
  where
    wrapFieldInit :: ActionLookupTable -> ActionLookupTable -> ActionLookupTable
    wrapFieldInit actionmethods actionvars = 
      let memorizedactionmethods = map memorizedActionEntry actionmethods 
          memorizedactionvars    = map memorizedActionEntry actionvars
          
          memorizedactionfields  = 
            [ a | (FieldActionId _ _ _, a) <- memorizedactionmethods ]
      in
       (map (wrap memorizedactionfields memorizedactionvars) 
        memorizedactionmethods)
    
    wrap :: [ActionMember] -> ActionLookupTable -> ActionLookupTableEntry 
            -> ActionLookupTableEntry
    wrap fieldActions varActionTable (FieldActionId c f id, a) = 
      let varAction = head
            [ a | (VarActionId c' Nothing "this" _, a) <- varActionTable,c==c']
      in  (FieldActionId c f id, thisInit varAction a)
    wrap fieldActions varActionTable (MethodActionId c m id, a) = 
      let varActions = 
            [ a | (VarActionId c' (Just (m',id')) _ _, a) <- varActionTable
                , c==c', m==m', id==id' ] ++
            [ a | (VarActionId c' Nothing "this" _, a) <- varActionTable,c==c' ]
      in
       (MethodActionId c m id, 
        fieldVarInit fieldActions varActions a)
    
    fieldVarInit :: [ActionMember] -> [ActionMember]
                    -> ActionMember -> ActionMember
    fieldVarInit fieldActions varActions a info context = do
      let f a m = do a info context; m
      foldr f (return ()) varActions    -- 'this' should be initialized first
      foldr f (return ()) fieldActions  -- Every field initialization needs it.
      a info context

    thisInit varAction a info context = do
      varAction info context
      a info context
      
mkActionClass info (Interface n is mdecls) = do
  return []
  
memorizedActionEntry :: ActionLookupTableEntry -> ActionLookupTableEntry 
memorizedActionEntry (FieldActionId c f id, a) = 
  let check action info context = do
        maybeaty <- getFieldtyping c context f id
        case maybeaty of
          Nothing -> action info context
          Just _  -> return () 
  in  (FieldActionId c f id, check a)
    
memorizedActionEntry (MethodActionId c m id, a) = 
  let check action info context = do
        maybemty <- getMethodtyping c context m id
        case maybemty of
          Nothing -> action info context
          Just _  -> return ()
  in (MethodActionId c m id, check a)
     
memorizedActionEntry (VarActionId c maybemid v vid, a) = 
  let check action info context = do
        maybemty <- getVartyping c context maybemid v vid
        case maybemty of
          Nothing -> action info context
          Just _  -> return ()
  in (VarActionId c maybemid v vid, check a)
     
    
mkActionMDecl :: ClassInfo -> MemberDecl -> IO ActionLookupTable
mkActionMDecl (n, p, is) (MethodDecl attrs retty m id argdecls stmt) = do
  actionstmt <- mkActionStmt stmt
  return [( MethodActionId n m id, 
            mkaction actionstmt (n, p, is, Just (m,id)) )]
  where
    mkaction actionstmt typingctx info context 
      | isEmptyContext context && elem "static" attrs == False = return ()
      | otherwise = mkaction' actionstmt typingctx info context 

          --           do
          -- maybe <- lookupAllocTableEntry 
          --          (n, context, m, id, head (reverse context))
          -- let cond = isEmptyContext context == False && isJust maybe 
          --            || isEmptyContext context
          -- if cond 
          --   then mkaction' actionstmt typingctx info context 
          --   else return ()
                    
    mkaction' actionstmt typingctx info context = do
      maybethisaty <- getVartyping n context Nothing "this" numThis
      Just retaty  <- getVartyping n context (Just (m,id)) "return" numReturn
      maybeaargtys <- getArgVartyping n context m id argdecls
      eff          <- newEffVar
      let aargtys = map fromJust maybeaargtys
      let aargdecls = [ (x,aty) | ((_, x, id),aty) <- zip argdecls aargtys ] 
          
      _ <-
        if elem "static" attrs == False 
        then do let id = getAnno (fromJust maybethisaty)
--                putConstraint (C_upper id (Set [context]))
                putConstraint (C_lower (Set [context]) id)
        else return ()
          
      typingenv0 <-
        if elem "static" attrs == False
        then -- do putTyping (V n context m id "this" numThis thisaty)
             return [("this", fromJust maybethisaty)]
        else return []
          
      let typingenv = typingenv0 ++ [("return", retaty)] ++ aargdecls
          
      putTyping (M n context m id aargtys retaty eff)
      _ <- actionstmt typingenv typingctx info context
      return ()
      
mkActionMDecl (n, p, is) (ConstrDecl m id argdecls stmt) = do
  actionstmt <- mkActionStmt stmt
  return [(MethodActionId n m id,
           mkaction actionstmt (n, p, is, Just (m,id)))]
  where
    mkaction actionstmt typingctx info context 
      | isEmptyContext context = return ()
      | otherwise = mkaction' actionstmt typingctx info context
                    
    mkaction' actionstmt typingctx info context = do
      Just thisaty <- getVartyping n context Nothing "this" numThis
      Just retaty  <- getVartyping n context (Just (m,id)) "return" numReturn
      maybeaargtys <- getArgVartyping n context m id argdecls
      eff       <- newEffVar
      let aargtys = map fromJust maybeaargtys
      let aargdecls = [ (x,aty) | ((_, x, id),aty) <- zip argdecls aargtys ]
          
      let typingenv = [("this", thisaty), ("return", retaty)] ++ aargdecls
          
      putTyping (M n context m id aargtys retaty eff)
      _ <- actionstmt typingenv typingctx info context
      return ()
  
mkActionMDecl (n, p, is) (FieldDecl attrs ty f id maybee) = do
  actionmaybeexpr <- mkActionMaybeExpr maybee
  return [(FieldActionId n f id, mkaction actionmaybeexpr (n, p, is, Just (f,id)))]
  where
    mkaction maybeactionexpr typingctx info context 
      | isEmptyContext context = return ()
      | otherwise = mkaction' maybeactionexpr typingctx info context
        
    mkaction' Nothing typingctx info context = do 
      nullty <- mkAnnoType (TypeName "null")
      mkaction'' nullty typingctx info context
    mkaction' (Just actionexpr) typingctx info context = do
      (aty,eff) <- actionexpr [] typingctx info context -- TODO: effect?
      mkaction'' aty typingctx info context
      
    mkaction'' aty typingctx info context = do
      fty      <- mkAnnoType ty
      Just cty <- getVartyping n context Nothing "this" numThis
      let cid = getAnno cty
      putTyping (F n context f id fty)
--      putConstraint (C_upper cid (Set [context]))
      putConstraint (C_lower (Set [context]) cid)
      resolvedConstraints <- resolveConstraint (C_assignfield aty cty f)
      mapM_ putConstraint resolvedConstraints
  
mkActionMDecl (n, p, is) (AbstractMethodDecl retty m id argdecls) = do
  return []
      

mkActionVar :: ClassInfo -> Vtypes -> IO ActionLookupTable
mkActionVar (n, p, is) vtypes = 
  mapM mkactionentry vtypes
  where
    mkactionentry (c,maybemid,x,xid,ty) = do
      let mkaction info context = do
            if isEmptyContext context && x=="this" ||
               isEmptyContext context && isInstanceMethod info c maybemid 
              then return () 
              else do
                   aty <- mkAnnoType ty
                   putTyping (V c context maybemid x xid aty)
      return (VarActionId c maybemid x xid, mkaction)
      
mkActionStmt :: Stmt -> IO ActionStmt
mkActionStmt (Expr expr) = do
  actionexpr <- mkActionExpr expr
  return $ mkaction actionexpr
  where
    mkaction actionexpr typingenv typingctx info context = do
      (_, effect) <- actionexpr typingenv typingctx info context
      return (typingenv, effect)
  
mkActionStmt (Ite expr stmt1 stmt2) = do
  actionexpr  <- mkActionExpr expr
  actionstmt1 <- mkActionStmt stmt1
  actionstmt2 <- mkActionStmt stmt2
  return $ mkaction actionexpr actionstmt1 actionstmt2
  where
    mkaction :: ActionExpr -> ActionStmt -> ActionStmt -> ActionStmt
    mkaction actionexpr actionstmt1 actionstmt2 typingenv typingctx info context = do
      (_, effe)      <- actionexpr typingenv typingctx info context
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
      (atye,effe) <- doit maybeactionexpr typingenv typingctx info context
      let c = getClassFromTypingCtx typingctx
      let maybemid = getMethodFromTypingCtx typingctx
      maybeaty <- getVartyping c context maybemid x id
      let aty = fromJust maybeaty
      -- aty <- mkAnnoType ty
      putConstraint (C_assign atye aty)
      let tyenv = addTyEnv (x,aty) typingenv
      return (tyenv, effe)
    
    doit Nothing typingenv typingctx info context = do
      aty <- mkAnnoType (TypeName "null")
      return (aty, noEffect)
    doit (Just actionexpr) typingenv typingctx info context = do
      actionexpr typingenv typingctx info context
      
mkActionStmt (Return maybeexpr) = do
  maybeaction <- mkActionMaybeExpr maybeexpr
  return $ mkaction maybeaction
  
  where      
    mkaction :: Maybe ActionExpr -> ActionStmt
    mkaction (Nothing) typingenv typingctx info context = do
      return (typingenv, noEffect)
    mkaction (Just actionexpr) typingenv typingctx info context = do
      (aty,eff) <- actionexpr typingenv typingctx info context
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
      (_,eff1) <- actione typingenv typingctx info context
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
        (_, eff1) <- actione1 typingenv typingctx info context
        typingenv'<- mkmaybedeclaction maybedecl x typingenv typingctx info context
        (_, eff2) <- actione2 typingenv' typingctx info context
        (_, eff3) <- actione3 typingenv' typingctx info context
        (typingenv'', eff4) <- actionstmt typingenv' typingctx info context
        -- tyenv' <- unionTyEnv info typingenv typingenv''
        let tyenv = restrict typingenv'' (domain typingenv)
        let eff = EffUnion eff1 (EffUnion eff2 (EffUnion eff3 eff4))
        return (tyenv, eff)
        
    mkmaybedeclaction (Nothing) x typingenv typingctx info context = 
      return typingenv
    mkmaybedeclaction (Just (ty, id)) x typingenv typingctx info context = do
      let c = getClassFromTypingCtx typingctx
      let maybemid = getMethodFromTypingCtx typingctx
      maybeaty <- getVartyping c context maybemid x id
      let aty = fromJust maybeaty
      -- aty <- mkAnnoType ty
      let typingenv' = addTyEnv (x,aty) typingenv
      return typingenv'
      
mkActionStmt (Block stmt) = do
  actionstmt <- mkActionStmt stmt
  return $ mkaction actionstmt
  where
    mkaction actionstmt typingenv typingctx info context = do
      (_,eff) <- actionstmt typingenv typingctx info context
      return (typingenv, eff)
    
mkActionMaybeExpr Nothing = do
  return $ Nothing
mkActionMaybeExpr (Just expr) = do
  actionexpr <- mkActionExpr expr
  return $ Just actionexpr

--
mkActionExpr :: Expr -> IO ActionExpr
mkActionExpr (Var x) = do
  return $ actionVar x
  where
    actionVar :: VarName -> ActionExpr
    actionVar x typingenv typingctx info context = do
      let aty = fromJust $ lookupEnv typingenv x -- TODO: Error handling
      return $ (aty, noEffect)

mkActionExpr (Field e f maybety) = do
  actionexp <- mkActionExpr e
  return $ actionField actionexp f (fromJust maybety)
  where
    actionField :: ActionExpr -> FieldName -> TypeName -> ActionExpr
    actionField actionexp f ty typingenv typingctx info context = do
      (atyexp, eff) <- actionexp typingenv typingctx info context
      aty <- mkAnnoType ty
      resolvedConstraints <- resolveConstraint (C_field atyexp f aty) 
      mapM_ putConstraint resolvedConstraints
      return (aty, eff)

mkActionExpr (StaticField c f maybety) = do
  return $ actionStaticfield c f (fromJust maybety)
  where
    actionStaticfield :: TypeName -> Name -> TypeName -> ActionExpr  
    actionStaticfield c f ty typingenv typingctx info context = do
      let maybefty    = lookupFields info c f
      let (fty,attrs) = fromJust maybefty
      cty <- mkAnnoType c
      aty <- mkAnnoType ty
      let cid = getAnno cty
--      putConstraint (C_upper cid (Set [staticContext]))
      putConstraint (C_lower (Set [staticContext]) cid) 
      resolvedConstraints <- resolveConstraint (C_field cty f aty)
      mapM_ putConstraint resolvedConstraints
      return (aty, noEffect)
  
mkActionExpr (New c es label) = do
  actionexps <- mapM mkActionExpr es
  return $ actionNew c actionexps label
  where
    actionNew :: TypeName -> [ActionExpr] -> Label -> ActionExpr
    actionNew c actionexps label typingenv typingctx info context = do
      atyeffs <- mapM (\actionexp -> actionexp typingenv typingctx info context) actionexps
      let (atys,effs) = unzip atyeffs
      cty          <- mkAnnoType c
      effVar       <- newEffVar
      let cid = getAnno cty
      let eff = EffUnion (foldr EffUnion noEffect effs) effVar
          
      let (cname,m,id) = 
            case typingctx of
              (cname,_,_,Just (m,id)) -> (cname, m, id)
              -- (cname,_,_,Nothing)     -> (cname, "*", 100)
      let (TypeName newc) = c    -- TODO: Fix it since c can be an array type! 
      uniqueContext <- putAllocTableEntry context (cname, m, id, label) newc
      putConstraint (C_lower (Set [uniqueContext]) cid)
      addInvokeConstraint c cid cty atys effVar
      return (cty, eff)
      
    addInvokeConstraint c@(TypeName cn)  cid cty   atys eff = do
      resolvedConstraints <- resolveConstraint (C_invoke cty cn atys eff cty)
      mapM_ putConstraint resolvedConstraints
    addInvokeConstraint c@(ArrayTypeName _) cid cty atys eff = return ()
      
mkActionExpr (Assign e1@(Var x) e2) = do
  actionexp2 <- mkActionExpr e2
  return $ actionAssignVar x actionexp2
  where
    actionAssignVar :: Name -> ActionExpr -> ActionExpr
    actionAssignVar x actionexp2 typingenv typingctx info context = do
      let aty1 = fromJust $ lookupEnv typingenv x
      (aty2,eff2) <- actionexp2 typingenv typingctx info context
      putConstraint (C_assign aty2 aty1)
      avoidty <- mkVoidType
      return (avoidty, eff2)
      
mkActionExpr (Assign (Field e1 f maybety) e2) = do
  actionexp1 <- mkActionExpr e1
  actionexp2 <- mkActionExpr e2
  return $ actionAssignField actionexp1 f (fromJust maybety) actionexp2
  where
    actionAssignField :: ActionExpr -> FieldName -> TypeName -> ActionExpr -> ActionExpr
    actionAssignField actionexp1 f ty actionexp2 typingenv typingctx info context = do
      (aty1,eff1) <- actionexp1 typingenv typingctx info context
      (aty2,eff2) <- actionexp2 typingenv typingctx info context
      resolvedConstraints <- resolveConstraint (C_assignfield aty2 aty1 f)
      mapM_ putConstraint resolvedConstraints
      avoidty <- mkVoidType
      let eff = EffUnion eff1 eff2
      return (avoidty, eff)
      
mkActionExpr (Assign (StaticField c f maybety) e2) = do
  actionexp2 <- mkActionExpr e2
  return $ actionAssignField c f (fromJust maybety) actionexp2
  where
    actionAssignField :: TypeName -> FieldName -> TypeName -> ActionExpr -> ActionExpr
    actionAssignField c f ty actionexp2 typingenv typingctx info context = do
      (aty2,eff2) <- actionexp2 typingenv typingctx info context
      cty <- mkAnnoType c
      let cid = getAnno cty
--      putConstraint (C_upper cid (Set [staticContext]))
      putConstraint (C_lower (Set [staticContext]) cid)
      resolvedConstraints <- resolveConstraint (C_assignfield aty2 cty f)
      mapM_ putConstraint resolvedConstraints
      avoidty <- mkVoidType 
      return (avoidty, eff2)

mkActionExpr (Assign (Prim "[]" [earr,eidx]) e2) = do
  actionexparr <- mkActionExpr earr
  actionexpidx <- mkActionExpr eidx
  actionexp2   <- mkActionExpr e2
  return $ actionAssignArray actionexparr actionexpidx actionexp2
  where
    actionAssignArray :: ActionExpr -> ActionExpr -> ActionExpr -> ActionExpr 
    actionAssignArray actionexparr actionexpidx actionexp2 typingenv typingctx info context = do
      (atyarr,effarr) <- actionexparr typingenv typingctx info context
      (atyidx,effidx) <- actionexpidx typingenv typingctx info context
      (aty2,  eff2)   <- actionexp2   typingenv typingctx info context
      let AnnoArrayType atyelem id = atyarr
      let eff = EffUnion effarr (EffUnion effidx eff2)
      putConstraint (C_assign aty2 atyelem)
      avoidty <- mkVoidType
      return (avoidty, eff)

-- In mkActionExpr (Assign e1 e2), e1 can't be the others
-- because the typechecker has already filtered such an illegal program.
      
mkActionExpr (Cast c e) = do
  actionexp <- mkActionExpr e
  return $ actionCast c actionexp
  where
    actionCast :: TypeName -> ActionExpr -> ActionExpr
    actionCast c actionexp typingenv typingctx info context = do
      (aty,eff) <- actionexp typingenv typingctx info context
      let cty = mkCast c aty
      return (cty, eff)
      
mkActionExpr (Invoke e m es maybety) = do
  actionexp  <- mkActionExpr e
  actionexps <- mapM mkActionExpr es
  return $ actionInvoke actionexp m actionexps (fromJust maybety)
  where
    actionInvoke :: ActionExpr -> MethodName -> [ActionExpr] -> TypeName -> ActionExpr
    actionInvoke actionexp m actionexps ty typingenv typingctx info context = do
      (atye,effe) <- actionexp typingenv typingctx info context
      atyeffes    <- mapM (\actionexp -> actionexp typingenv typingctx info context) actionexps
      aty         <- mkAnnoType ty
      effm        <- newEffVar
      let (atyes,effes) = unzip atyeffes
      let id            = getAnno aty
      let eff           = foldr EffUnion effe effes
          
      resolvedConstraints <- resolveConstraint (C_invoke atye m atyes effm aty)
      mapM_ putConstraint resolvedConstraints
      return (aty, eff)
      
mkActionExpr (StaticInvoke c m es maybety) = do      
  actionexps <- mapM mkActionExpr es
  return $ actionStaticinvoke c m actionexps (fromJust maybety)
  where
    actionStaticinvoke :: TypeName -> MethodName -> [ActionExpr] -> TypeName -> ActionExpr
    actionStaticinvoke c m actionexps ty typingenv typingctx info context = do
      atyeffs <- mapM (\actionexp -> actionexp typingenv typingctx info context) actionexps
      let (atyes,effes) = unzip atyeffs
      cty  <- mkAnnoType c
      aty  <- mkAnnoType ty
      effm <- newEffVar
      let cid = getAnno cty
      let eff = foldr EffUnion noEffect effes
          
--      putConstraint (C_upper cid (Set [staticContext]))
      putConstraint (C_lower (Set [staticContext]) cid)
      resolvedConstraints <- resolveConstraint (C_invoke cty m atyes effm aty)
      mapM_ putConstraint resolvedConstraints
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
    actionLit label typingenv typingctx info context = do
      aty <- mkAnnoType (TypeName "String")
      let id = getAnno aty
      putConstraint (C_lower (Set [addContext length_k context label]) id)
      return (aty, noEffect)
      

mkActionExpr (ConstChar s) = do  
  return $ actionConst (TypeName "char")

mkActionExpr (Prim n es) = do
  return $ actionConst (TypeName "char")

actionConst ty typingenv typingctx info context = do
  aty <- mkAnnoType ty
  return (aty, noEffect)

      

--      
mkCast :: TypeName -> AnnoType -> AnnoType
mkCast (TypeName c)       (AnnoType d id)        = AnnoType c id
mkCast (TypeName c)       (AnnoArrayType aty id) = AnnoType c id
mkCast (ArrayTypeName ty) (AnnoArrayType aty id) = AnnoArrayType (mkCast ty aty) id
