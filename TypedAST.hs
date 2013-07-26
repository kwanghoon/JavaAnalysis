module TypedAST where

import AST (Name, Attrib, TypeName)

type UniqueId     = Int
type ObjAllocSite = UniqueId
type Ctx          = [ObjAllocSite]
  
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
    TypedVar          AnnoName
  | TypedField        TypedExpr NameUnique
  | TypedStaticField  TypeName NameUnique
  | TypedNew          AnnoTypeName [TypedExpr]
  | TypedAssign       TypedExpr TypedExpr
  | TypedCast         AnnoTypeName TypedExpr
  | TypedInvoke       TypedExpr NameUnique [TypedExpr]
  | TypedStaticInvoke AnnoTypeName NameUnique [TypedExpr]
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
    

    
    




