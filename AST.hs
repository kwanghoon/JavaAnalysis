module AST where

import Data.List

--
type UniqueId     = Integer
type Label        = Integer

uniqueidforstatic = 1
initialuniqueid   = uniqueidforstatic+1

--
static         = "static"
abstract       = "abstract"
java_class     = "class"
java_interface = "interface"

-- For typechecking
type UserClasses = [(Name, [Attrib])]
type Inheritance = [(Name,Name)]
type Fields      = [(ClassName, [(TypeName, FieldName, [Attrib])])]
type Mtypes      = [(ClassName, MethodName, UniqueId, [TypeName], TypeName, [Attrib], [VarName], Maybe Stmt)]
type Vtypes      = [(ClassName, Maybe (MethodName, UniqueId), VarName, UniqueId, TypeName)]

type Info      = (UserClasses, Inheritance, Fields, Mtypes, Vtypes)
type ClassInfo = (ClassName, Maybe ClassName, [ClassName]) -- itself, parent, interfaces

getUserClasses (userClasses, inheritance, fields, mtypes, vtypes) = userClasses 
getInheritance (userClasses, inheritance, fields, mtypes, vtypes) = inheritance
getFields      (userClasses, inheritance, fields, mtypes, vtypes) = fields
getMtypes      (userClasses, inheritance, fields, mtypes, vtypes) = mtypes
getVtypes      (userClasses, inheritance, fields, mtypes, vtypes) = vtypes

isUserClass c ucs =     
  not $ null $ 
  [ (n,attribs) | (n,attribs) <- ucs, c==n, elem java_class attribs ]

isUserInterface c ucs =     
  not $ null $ 
  [ (n,attribs) | (n,attribs) <- ucs, c==n, elem java_interface attribs ]
  
isInstanceMethod info c Nothing = False
isInstanceMethod info c (Just (m,id)) = 
  let mtypes = getMtypes info 
  in  and
      [ elem "static" attrs == False
      | (c',m',id', _, _, attrs, _, _) <- mtypes
      , c==c' && m==m' && id==id' ]

--
type Name     = String
type Attrib   = String

type ClassName  = Name
type MethodName = Name
type FieldName  = Name
type VarName    = Name
type PrimName   = Name

--
type Program = [Class]

data Class = Class [Attrib] Name (Maybe Name) [Name] MemberDecls 
           | Interface Name [Name] MemberDecls

type MemberDecls = [MemberDecl]

data TypeName = TypeName Name | ArrayTypeName TypeName

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

  
type ArgDecls = [(TypeName, Name, UniqueId)]

getIdOfVar (ty, n, id) = id

data MemberDecl = MethodDecl [Attrib] TypeName Name UniqueId ArgDecls Stmt
                | ConstrDecl Name UniqueId ArgDecls Stmt
                | FieldDecl [Attrib] TypeName Name UniqueId (Maybe Initializer)
                | AbstractMethodDecl TypeName Name UniqueId ArgDecls -- TODO

type Initializer = Expr

data Expr = 
    Var Name
  | Field Expr Name (Maybe TypeName)           -- for field type
  | StaticField TypeName Name (Maybe TypeName) -- for field type
  | New TypeName [Expr] Label
  | Assign Expr Expr
  | Cast TypeName Expr
  | Invoke Expr Name [Expr] (Maybe TypeName)           -- for return type
  | StaticInvoke TypeName Name [Expr] (Maybe TypeName) -- for return type
  | ConstTrue
  | ConstFalse
  | ConstNull
  | ConstNum String
  | ConstLit String Label
  | ConstChar String
  | Prim Name [TypeName] [Expr] -- for argument types
            
data Stmt = 
    Expr Expr
  | Ite Expr Stmt Stmt
  | LocalVarDecl TypeName VarName UniqueId (Maybe Expr)
  | Return (Maybe Expr)
  | Seq Stmt Stmt
  | NoStmt
  | While Expr Stmt
  | For (Maybe (TypeName, UniqueId)) Name Expr Expr Expr Stmt
  | Block Stmt
            
toStmt []           = NoStmt
toStmt [stmt]       = stmt
toStmt (stmt:stmts) = Seq stmt (toStmt stmts)

-- Pretty(?) Printing
tabstop n = combine $ take n $ repeat ((++) "   ")
combine = foldr (.) id
conc ss = combine (map (++) ss)

cond [] s = id
cond _  s = (++) s

comma = intersperse "," 
argsdecl cxns = concat $ comma [ conc [show c, " ", x] "" | (c,x,n) <- cxns ]

opt Nothing  f = (++) ""
opt (Just x) f = f x

list [] f = (++) ""
list xs f = f xs

comment s = conc ["//", " ", s]

varNum    s = "v" ++ show s
methodNum s = "m" ++ show s
fieldNum s = "f" ++ show s

allocLabel p id = conc [" ", "/* a", show id, " */", " "]


-- delimeter (Ite _ _ _) = ""
-- delimeter (LocalVarDecl _ _ _) = ""
-- delimeter _ = ";\n"

prprog cs = concat $ intersperse "\n" $ map show cs

instance Show TypeName where
  showsPrec p (TypeName s) = conc [s]
  showsPrec p (ArrayTypeName a) = conc [show a, "[]"]
    
instance Show Expr where            
  showsPrec p (Var n) = tabstop p . conc [n]
  showsPrec p (Field e f _) = tabstop p . conc [show e, ".", f]
  showsPrec p (StaticField c f _) = tabstop p . conc [show c, ".", f]
  showsPrec p (New (ArrayTypeName c) [e] label) =
    tabstop p . conc ["new", " ",  show c, "[", show e, "]"] . 
    allocLabel p label
  showsPrec p (New c es label) = 
    tabstop p . conc (["new", " ",  show c, "("] ++ comma (map show es) ++ [")"]) .
    allocLabel p label
  showsPrec p (Assign e y) = tabstop p . conc [show e, "=", show y]
  showsPrec p (Cast c e) = tabstop p . conc ["(", show c, ")", show e]
  showsPrec p (Invoke e m ys _) = tabstop p . conc ([show e, ".", m, "("] ++ 
                                                  comma (map show ys) ++ [")"])
  showsPrec p (StaticInvoke c m ys _) = 
    tabstop p . conc ([show c, ".", m, "("] ++ 
                      comma (map show ys) ++ [")"])
  showsPrec p (ConstTrue)  = conc ["true"]
  showsPrec p (ConstFalse) = conc ["false"]
  showsPrec p (ConstNull)  = conc ["null"]
  showsPrec p (ConstNum n) = conc [n]
  showsPrec p (ConstLit s label) = conc ["\"", s, "\""] . allocLabel p label
  showsPrec p (ConstChar s) = conc ["\'", s, "\'"]
  showsPrec p (Prim "==" _ [x,y]) = conc [show x, "==", show y]
  showsPrec p (Prim "!=" _ [x,y]) = tabstop p . conc [show x, "!=", show y]
  showsPrec p (Prim "primAddButton" _ [x]) = 
    tabstop p . conc ["primAddButton", "(", show x, ")"]
  showsPrec p (Prim "primStartActivity" _ [x]) = 
    tabstop p . conc ["primStartActivity", "(", show x, ")"]
  showsPrec p (Prim "[]" _ [x,y]) = tabstop p . conc [show x, "[", show y, "]"]
  -- showsPrec p (Prim "[]=" [x,y]) = tabstop p . conc [show x, "=", show y]
  showsPrec p (Prim "super" _ [x]) = 
    tabstop p . conc ["super", "(", show x, ")"]
  showsPrec p (Prim "<" _ [x,y]) = tabstop p . conc [show x, "<", show y]
  showsPrec p (Prim "++" _ [x,y]) = tabstop p . conc [show x, "++"]
  showsPrec p (Prim "--" _ [x,y]) = tabstop p . conc [show x, "--"]
  
instance Show Stmt where  
  showsPrec p (Expr e) = tabstop p . conc [ show e, ";", "\n"]
  showsPrec p (Ite e s1 NoStmt) = 
    tabstop p . conc ["if", " ", "(", show e, ")", "\n"] 
    . showsPrec (p+1) s1
    . tabstop p . conc ["\n"]
  showsPrec p (Ite e s1 s2) = 
    tabstop p . conc ["if", " ", "(", show e, ")", "\n"] 
    . showsPrec (p+1) s1
    . tabstop p . conc ["\n"]
    . tabstop p . conc ["else", "\n"]
    . showsPrec (p+1) s2
    . tabstop p . conc ["\n"]
  showsPrec p (LocalVarDecl c x n maybee) = 
    tabstop p . conc [show c, " ", x] . 
    opt maybee (\e -> conc ["=", show e]) .
    conc [ ";", " "] . 
    comment (varNum n) .
    conc ["\n"]
  showsPrec p (Return maybee) = 
    tabstop p . conc [ "return "] .
    opt maybee (\e -> conc [show e]) . 
    conc [";", "\n"]
  showsPrec p (Seq s1 s2) = showsPrec p s1 . showsPrec p s2
  showsPrec p (NoStmt) = (++) ""
  showsPrec p (While e s) = 
    tabstop p . conc [ "while", "(", show e, ")", "\n" ] .
    showsPrec (p+1) s .
    tabstop p . conc [ "\n" ]
  showsPrec p (For maybetyn x e1 e2 e3 s) = 
    tabstop p . conc [ "for", "(" ] .
    opt maybetyn (\(ty,_) -> conc [show ty, " "]) .
    conc [  x, "=", show e1, ";", " ", show e2, ";", " ", show e3, ")", " "] . 
    opt maybetyn (\(_,n) -> comment (varNum n)) .
    conc ["\n"] .
    showsPrec (p+1) s .
    tabstop p . conc [ "\n" ]
  showsPrec p (Block s) = 
    tabstop p . conc ["{", "\n"] .
    showsPrec (p+1) s .
    tabstop p . conc ["}", "\n"]
  
  
instance Show MemberDecl where
  showsPrec p (MethodDecl attrs c m id params s) = 
    tabstop p . conc (comma attrs) .
    cond attrs " " .
    conc [show c, " ", m, "(", argsdecl params, ")", " ", "{", " "] .  
    comment (methodNum id) . conc [" "] .
    conc ["(", 
          concat $ intersperse "," $ map (varNum . getIdOfVar) params, ")"] .
    conc ["\n"] .
    showsPrec (p+1) s . 
    tabstop p . conc ["}", "\n"]
  showsPrec p (ConstrDecl k id params s) = 
    tabstop p . conc [k, "(", argsdecl params, ")", " ", "{", " "] .
    comment (methodNum id) . conc [" "] .
    conc ["(", 
          concat $ intersperse "," $ map (varNum . getIdOfVar) params, ")"] .
    conc ["\n"] .
    showsPrec (p+1) s . 
    tabstop p . conc ["}", "\n"]
  showsPrec p (FieldDecl attrs c x id maybei) = 
    tabstop p . conc (comma attrs) .
    conc [show c, " ", x] . 
    opt maybei (\i -> conc ["=", show i]) .
    conc [";", " "] .
    comment (fieldNum id) . conc ["\n"]

  showsPrec p (AbstractMethodDecl c m id params) = 
    tabstop p . 
    conc [show c, " ", m, "(", argsdecl params, ")", " ", ";", " "] . 
    comment (methodNum id) . conc [" "] .
    conc ["(", 
          concat $ intersperse "," $ map (varNum . getIdOfVar) params, ")"] .
    conc ["\n"]
    
instance Show Class where
  showsPrec p (Class attrs c maybepc is mdecl) =
    tabstop p. conc (comma attrs) .
    conc ["class", " ", c, " "] . 
    opt  maybepc (\pc -> conc ["extends", " ", pc]) . 
    list is (\is -> conc ["implements" , " "] .  conc (comma is)) .
    conc [" ", "{", "\n"] .
    combine (map (showsPrec (p+1)) mdecl) .
    conc ["}", "\n"]

  showsPrec p (Interface c is mdecl) =
    tabstop p. conc ["interface", " ", c, " "] . 
    list is (\is -> conc ["extends", " "] . conc (comma is)) .
    conc [" ", "{", "\n"] .
    combine (map (showsPrec (p+1)) mdecl) .
    conc ["}", "\n"]

-- Numbering Methods and Local Variables
numProgram program = [ numClass c | c <- program ]

numClass (Class attrs n pn ins mdecls) = 
  Class attrs n pn ins (numMdecls (defaultConstr ++ mdecls))
  where
    defaultConstr = 
      case [ length argdecls | (ConstrDecl _ _ argdecls _) <- mdecls ] of
        [] -> [ConstrDecl n 0 [] NoStmt]
        _  -> []
numClass (Interface n ins mdecls) =  
  Interface n ins (numMdecls mdecls)
  
numThis   = 0
numReturn = 1

numMdecls mdecls = 
  [ numMdecl mdecl id | (mdecl,id) <- zip mdecls [1..] ]

-- We assume the local variable 'this' gets the number 0.
numMdecl (MethodDecl attrs retty n _ argdecls stmt) id = 
  MethodDecl attrs retty n id argdecls' stmt'
  where
    (argdecls', n') = numArgDecls argdecls (numReturn+1)
    (stmt', _, _)      = numStmt stmt n' 1
numMdecl (ConstrDecl n _ argdecls stmt) id =
  ConstrDecl n id argdecls' stmt'
  where
    (argdecls', n') = numArgDecls argdecls (numReturn+1)
    (stmt', _, _)      = numStmt stmt n' 1
numMdecl (FieldDecl attrs ty n _ maybee) id =
  FieldDecl attrs ty n id maybee'
  where
    (maybee', o') = numMaybeExpr maybee 1
numMdecl (AbstractMethodDecl ty n _ argdecls) id =
  AbstractMethodDecl ty n id argdecls'
  where
    (argdecls', n') = numArgDecls argdecls 1

numArgDecls argdecls n = (argdecls', n')
  where
    argdecls' = [ (ty,x,i) | ((ty, x, _),i) <- zip argdecls [n..] ]
    n'        = n + len argdecls
    
    len []     = 0
    len (x:xs) = 1 + len xs

numStmt :: Stmt -> UniqueId -> UniqueId -> (Stmt, UniqueId, UniqueId)
numStmt (Expr e) n o      = (Expr e', n, o')
  where
    (e',o') = numExpr e o
numStmt (Ite e s1 s2) n o = (Ite e' s1' s2', n'', o''')
  where
    (s1',n',o')   = numStmt s1 n o
    (s2',n'',o'') = numStmt s2 n' o'
    (e', o''') = numExpr e o''
numStmt (LocalVarDecl ty x _ maybee) n o = (LocalVarDecl ty x n maybee', n+1, o') 
  where
    (maybee',o') = numMaybeExpr maybee o
numStmt (Return maybee) n o = (Return maybee', n, o')
  where
    (maybee',o') = numMaybeExpr maybee o
numStmt (Seq s1 s2) n o = (Seq s1' s2', n'', o'')
  where
    (s1',n',o')  = numStmt s1 n o
    (s2',n'',o'') = numStmt s2 n' o'
numStmt (NoStmt) n o = (NoStmt, n, o)
numStmt (While e s) n o = (While e' s', n', o'')
  where
    (s',n', o')  = numStmt s n o
    (e',o'') = numExpr e o'
numStmt (For maybetyn x e1 e2 e3 s) n o = (For maybetyn' x e1' e2' e3' s', n', o3')
  where
    (maybetyn',n1) =
      case maybetyn of
        Nothing -> (Nothing, n)
        Just (ty,_) -> (Just (ty,n), n+1)
        
    (s',n',o') = numStmt s n1 o
    (e1',o1') = numExpr e1 o'
    (e2',o2') = numExpr e2 o1'
    (e3',o3') = numExpr e3 o2'
numStmt (Block s) n o = (Block s', n',o')
  where
    (s',n',o') = numStmt s n o
    
numExpr (Var n) o = (Var n, o)
numExpr (Field e n maybety) o = (Field e' n maybety, o')
  where
    (e',o') = numExpr e o
numExpr (StaticField c n maybety) o = (StaticField c n maybety, o)
numExpr (New c es _) o = (New c es' o, o') 
  where
    (es',o') = numExprs es (o+1)
numExpr (Assign e1 e2) o = (Assign e1' e2', o2)
  where
    (e1',o1) = numExpr e1 o
    (e2',o2) = numExpr e2 o1
numExpr (Cast c e) o = (Cast c e', o')
  where
    (e',o') = numExpr e o
numExpr (Invoke e n es maybety) o = (Invoke e' n es' maybety, o'')
  where
    (e',o') = numExpr e o
    (es',o'') = numExprs es o'
numExpr (StaticInvoke c n es maybety) o = (StaticInvoke c n es' maybety, o')
  where    
    (es',o') = numExprs es o
numExpr (ConstTrue) o = (ConstTrue, o)
numExpr (ConstFalse) o = (ConstFalse, o)
numExpr (ConstNull) o = (ConstNull, o)
numExpr (ConstNum s) o = (ConstNum s, o)
numExpr (ConstLit s _) o = (ConstLit s o, o+1)
numExpr (ConstChar s) o = (ConstChar s, o)
numExpr (Prim n tys es) o = (Prim n tys es', o')
  where
    (es',o') = numExprs es o

numExprs [] o = ([], o)
numExprs (e:es) o = (e':es', o'')
  where
    (e',o') = numExpr e o
    (es',o'') = numExprs es o'

numMaybeExpr (Nothing) o = (Nothing, o)
numMaybeExpr (Just e) o = (Just e', o')
  where
    (e',o') = numExpr e o
    
