module TypedAST where

import AST 

--  
-- type UniqueId     = Integer
-- type ObjAllocSite = UniqueId
-- type Ctx          = [ObjAllocSite]
-- data Set          = Set [UniqueId]

-- 
type TypedMtypes = 
  [(Name, Name, UniqueId, [TypeName], TypeName, [Attrib], [Name], Maybe Stmt)]

type TypedInfo = (UserClasses, Inheritance, Fields, TypedMtypes)

--
type AnnoTypeName = (TypeName, Ctx)
type AnnoName     = (Name, Ctx)
type NameUnique   = (Name, UniqueId)

type TypedProgram = [TypedClass]

data TypedClass = 
    TypedClass [Attrib] Name (Maybe Name) [Name] TypedMemberDecls
  | TypedInterface Name (Maybe Name) TypedMemberDecls
    
type TypedArgDecls = [(AnnoTypeName, Ctx)]
    
type TypedMemberDecls = [TypedMemberDecl]

data TypedMemberDecl = 
    TypedMethodDecl         [Attrib] AnnoTypeName NameUnique TypedArgDecls TypedStmt
  | TypedConstrDecl         NameUnique TypedArgDecls TypedStmt
  | TypedFieldDecl          [Attrib] AnnoTypeName NameUnique (Maybe TypedInitializer)
  | TypedAbstractMethodDecl AnnoTypeName NameUnique TypedArgDecls -- TODO
    
type TypedInitializer = TypedExpr

data TypedExpr = 
    TypedVar          Name
  | TypedField        TypedExpr Name
  | TypedStaticField  TypeName Name
  | TypedNew          TypeName [TypedExpr] ObjAllocSite
  | TypedAssign       TypedExpr TypedExpr
  | TypedCast         TypeName TypedExpr
  | TypedInvoke       TypedExpr Name [TypedExpr]
  | TypedStaticInvoke TypeName Name [TypedExpr]
  | TypedConstTrue
  | TypedConstFalse
  | TypedConstNull
  | TypedConstNum     String
  | TypedConstLit     String
  | TypedConstChar    String
  | TypedPrim Name   [TypedExpr]
    
data TypedStmt = 
    TypedExpr         TypedExpr
  | TypedIte          TypedExpr TypedStmt TypedStmt
  | TypedLocalVarDecl AnnoTypeName AnnoName (Maybe TypedExpr)
  | TypedReturn       (Maybe TypedExpr)
  | TypedSeq          TypedStmt TypedStmt
  | TypedNoStmt
  | TypedWhile        TypedExpr TypedStmt
  | TypedFor          (Maybe AnnoTypeName) AnnoName TypedExpr TypedExpr TypedExpr TypedStmt
  | TypedBlock        TypedStmt
    

    
    




