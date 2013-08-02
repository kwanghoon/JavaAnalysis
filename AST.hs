module AST where

import Data.List

--
type UniqueId     = Integer
type ObjAllocSite = UniqueId
type Ctx          = [ObjAllocSite]
data Set          = Set [UniqueId]

--
static    = "static"
abstract  = "abstract"
java_class    = "class"
java_interface = "interface"

-- For typechecking
type UserClasses = [(Name, [Attrib])]
type Inheritance = [(Name,Name)]
type Fields      = [(ClassName, [(TypeName, FieldName, [Attrib])])]
type Mtypes      = [(ClassName, MethodName, UniqueId, [TypeName], TypeName, [Attrib], [VarName], Maybe Stmt)]
type Vtypes      = [(ClassName, MethodName, UniqueId, TypeName, [Attrib], VarName)]

type Info = (UserClasses, Inheritance, Fields, Mtypes)

getUserClasses (userClasses, inheritance, fields, mtype) = userClasses 
getInheritance (userClasses, inheritance, fields, mtype) = inheritance
getFields      (userClasses, inheritance, fields, mtype) = fields
getMtype       (userClasses, inheritance, fields, mtype) = mtype

type TypingEnv = [(Name, TypeName)]
type TypingCtx = (ClassName, Maybe ClassName, [ClassName], Maybe MethodName)

--
type Name     = String
type Attrib   = String

type ClassName  = Name
type MethodName = Name
type FieldName  = Name
type VarName    = Name

--
type Program = [Class]

data Class = Class [Attrib] Name (Maybe Name) [Name] MemberDecls 
           | Interface Name [Name] MemberDecls

type MemberDecls = [MemberDecl]

data TypeName = TypeName Name | ArrayTypeName TypeName
  
type ArgDecls = [(TypeName, Name)]

data MemberDecl = MethodDecl [Attrib] TypeName Name UniqueId ArgDecls Stmt
                | ConstrDecl Name UniqueId ArgDecls Stmt
                | FieldDecl [Attrib] TypeName Name (Maybe Initializer)
                | AbstractMethodDecl TypeName Name UniqueId ArgDecls -- TODO

type Initializer = Expr

data Expr = Var Name
          | Field Expr Name
          | StaticField TypeName Name
          | New TypeName [Expr]
          | Assign Expr Expr
          | Cast TypeName Expr
          | Invoke Expr Name [Expr]
          | StaticInvoke TypeName Name [Expr]
          | ConstTrue
          | ConstFalse
          | ConstNull
          | ConstNum String
          | ConstLit String
          | ConstChar String
          | Prim Name [Expr]
            
data Stmt = Expr Expr
          | Ite Expr Stmt Stmt
          | LocalVarDecl TypeName Name (Maybe Expr)
          | Return (Maybe Expr)
          | Seq Stmt Stmt
          | NoStmt
          | While Expr Stmt
          | For (Maybe TypeName) Name Expr Expr Expr Stmt
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
argsdecl cxs = concat $ comma [ conc [show c, " ", x] "" | (c,x) <- cxs ]
--seqEs p es = combine [ showsPrec p e . conc [delimeter e] | e <- es ]

opt Nothing  f = (++) ""
opt (Just x) f = f x

list [] f = (++) ""
list xs f = f xs

comment s = conc ["//", " ", s]

-- delimeter (Ite _ _ _) = ""
-- delimeter (LocalVarDecl _ _ _) = ""
-- delimeter _ = ";\n"

prprog cs = concat $ intersperse "\n" $ map show cs

instance Show TypeName where
  showsPrec p (TypeName s) = conc [s]
  showsPrec p (ArrayTypeName a) = conc [show a, "[]"]
    
instance Show Expr where            
  showsPrec p (Var n) = tabstop p . conc [n]
  showsPrec p (Field e f) = tabstop p . conc [show e, ".", f]
  showsPrec p (StaticField c f) = tabstop p . conc [show c, ".", f]
  showsPrec p (New (ArrayTypeName c) [e]) =
    tabstop p . conc (["new", " ",  show c, "[", show e, "]"])
  showsPrec p (New c es) = 
    tabstop p . conc (["new", " ",  show c, "("] ++ comma (map show es) ++ [")"])
  showsPrec p (Assign e y) = tabstop p . conc [show e, "=", show y]
  showsPrec p (Cast c e) = tabstop p . conc ["(", show c, ")", show e]
  showsPrec p (Invoke e m ys) = tabstop p . conc ([show e, ".", m, "("] ++ 
                                                  comma (map show ys) ++ [")"])
  showsPrec p (StaticInvoke c m ys) = tabstop p . conc ([show c, ".", m, "("] ++ 
                                                  comma (map show ys) ++ [")"])
  showsPrec p (ConstTrue)  = conc ["true"]
  showsPrec p (ConstFalse) = conc ["false"]
  showsPrec p (ConstNull)  = conc ["null"]
  showsPrec p (ConstNum n) = conc [n]
  showsPrec p (ConstLit s) = conc ["\"", s, "\""]
  showsPrec p (ConstChar s) = conc ["\'", s, "\'"]
  showsPrec p (Prim "==" [x,y]) = conc [show x, "==", show y]
  showsPrec p (Prim "!=" [x,y]) = tabstop p . conc [show x, "!=", show y]
  showsPrec p (Prim "primAddButton" [x]) = tabstop p . conc ["primAddButton", "(", show x, ")"]
  showsPrec p (Prim "primStartActivity" [x]) = tabstop p . conc ["primStartActivity", "(", show x, ")"]
  showsPrec p (Prim "[]" [x,y]) = tabstop p . conc [show x, "[", show y, "]"]
  showsPrec p (Prim "[]=" [x,y]) = tabstop p . conc [show x, "=", show y]
  showsPrec p (Prim "super" [x]) = tabstop p . conc ["super", "(", show x, ")"]
  showsPrec p (Prim "<" [x,y]) = tabstop p . conc [show x, "<", show y]
  showsPrec p (Prim "++" [x,y]) = tabstop p . conc [show x, "++"]
  showsPrec p (Prim "--" [x,y]) = tabstop p . conc [show x, "--"]
  
instance Show Stmt where  
  showsPrec p (Expr e) = tabstop p . conc [ show e, ";", "\n"]
  showsPrec p (Ite e s1 NoStmt) = tabstop p . conc ["if", " ", "(", show e, ")", "\n"] 
                               . showsPrec (p+1) s1
                               . tabstop p . conc ["\n"]
  showsPrec p (Ite e s1 s2) = tabstop p . conc ["if", " ", "(", show e, ")", "\n"] 
                               . showsPrec (p+1) s1
                               . tabstop p . conc ["\n"]
                               . tabstop p . conc ["else", "\n"]
                               . showsPrec (p+1) s2
                               . tabstop p . conc ["\n"]
  showsPrec p (LocalVarDecl c x maybee) = 
    tabstop p . conc [show c, " ", x] . 
    opt maybee (\e -> conc ["=", show e]) .
    conc [ ";", "\n"]
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
  showsPrec p (For maybety x e1 e2 e3 s) = 
    tabstop p . conc [ "for", "(" ] .
    opt maybety (\ty -> conc [show ty, " "]) .
    conc [  x, "=", show e1, ";", " ", show e2, ";", " ", show e3, ")", "\n"] .
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
    comment (show id) . conc ["\n"] .
    showsPrec (p+1) s . 
    tabstop p . conc ["}", "\n"]
  showsPrec p (ConstrDecl k id params s) = 
    tabstop p . conc [k, "(", argsdecl params, ")", " ", "{", " "] .
    comment (show id) . conc ["\n"] .
    showsPrec (p+1) s . 
    tabstop p . conc ["}", "\n"]
  showsPrec p (FieldDecl attrs c x maybei) = 
    tabstop p . conc (comma attrs) .
    conc [show c, " ", x] . 
    opt maybei (\i -> conc ["=", show i]) .
    conc [";", "\n"]
  showsPrec p (AbstractMethodDecl c m id params) = 
    tabstop p . 
    conc [show c, " ", m, "(", argsdecl params, ")", " ", ";", " "] . 
    comment (show id) .
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
  Class attrs n pn ins (numMdecls mdecls)
numClass (Interface n ins mdecls) =  
  Interface n ins (numMdecls mdecls)
  
numMdecls mdecls = [ numMdecl mdecl id | (mdecl,id) <- zip mdecls [1..] ]

numMdecl (MethodDecl attrs retty n _ argdecls stmt) id = 
  MethodDecl attrs retty n id argdecls stmt
numMdecl (ConstrDecl n _ argdecls stmt) id =
  ConstrDecl n id argdecls stmt
numMdecl (FieldDecl attrs ty n maybee) id =
  FieldDecl attrs ty n maybee
numMdecl (AbstractMethodDecl ty n _ argdecls) id =
  AbstractMethodDecl ty n id argdecls
