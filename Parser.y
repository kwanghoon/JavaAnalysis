
{

--------------------------------------------------------------------------------
-- (c) 2013 Kwanghoon Choi
-- kwanghoon.choi@yonsei.ac.kr

-- A simple parser for a subset of Java:
--------------------------------------------------------------------------------

module Parser where

import Data.Char
import Data.List
import AST

dummy_var_name = "$d"

}

%name parseprog Program

%tokentype { Token }
%error { parseError }

%token
	';'	{ TokenSemiColon }
	'.'	{ TokenDot }
	','	{ TokenComma }
	'('	{ TokenOP }
	')'	{ TokenCP }
	'{'	{ TokenOB }
	'}'	{ TokenCB }
	'['	{ TokenOBK }
	']'	{ TokenCBK }
	'='	{ TokenEQ }
	'+'	{ TokenPlus }
	'-'	{ TokenMinus }
	'*'	{ TokenStar }
	'/'	{ TokenDiv }
	class	{ TokenClass }
	extends	{ TokenExtends }
	new	{ TokenNew }
	if	{ TokenIf }
	else	{ TokenElse }
	true	{ TokenTrue }
	false	{ TokenFalse }
	return	{ TokenReturn }

	package	{ TokenPackage }
	import	{ TokenImport }
	public	{ TokenPublic }
	protected	{ TokenProtected }
	private	{ TokenPrivate }
	static	{ TokenStatic }
	abstract	{ TokenAbstract }
	interface	{ TokenInterface }
	implements	{ TokenImplements }
	final	{ TokenFinal }
	for	{ TokenFor }
	while	{ TokenWhile }

	null	{ TokenNull }
	num	{ TokenNum $$ }
	lit	{ TokenLit $$ }
	char	{ TokenChar $$ }
	var	{ TokenVar $$ }
	classname{ TokenClassType $$ }
	primname { TokenPrimitiveType $$ }

        equal   { TokenEqual }
	lt	{ TokenLT }
	noteq   { TokenNotEq }
	inc	{ TokenInc }
	dec	{ TokenDec }

%%

Program	: optpkgdecl optimportdecl decl	 { $3 }

optpkgdecl
        :             { }
        | pkgdecl     { }

pkgdecl : package pkgname ';'  { }

pkgname : var              { }
        | classname        { }
        | var '.' pkgname  { }

optimportdecl
        :              { }
        | importdecls  { }

importdecl
        : import pkgname ';' { }

importdecls
        : importdecl              { }
        | importdecl importdecls  { }


N	: optAccess optAbstract optStatic class classname optExtend optImplements '{' memberDecl '}' 
          { let attr = if $3 then ["static"] else []
            in  Class attr $5 $6 $7 $9 
          }
        | optAccess interface classname optExtends '{' abstractMemberDecl '}'
	  { Interface $3 $4 $6 }

optExtend
        :                   { Nothing }
        | extends classname { Just $2 }

optExtends
        :                    { [] }
        | extends classnames { $2 }

optImplements
        :                       { [] }
        | implements classnames { $2 }

classnames
        : classname                 { [$1] }
        | classname ',' classnames  { $1 : $3 }

F       : type var ';'            { \attr -> FieldDecl attr $1 $2 0 Nothing }
        | type var '=' expr  ';'  { \attr -> FieldDecl attr $1 $2 0 (Just $4) }

M       : type var '(' varDecl ')' '{' stmts '}' 
           { \attr -> MethodDecl attr $1 $2 0 $4 (toStmt $7) }

abstractM
        : type var '(' varDecl ')' ';'  { AbstractMethodDecl $1 $2 0 $4 }

K       : classname '(' varDecl ')' '{' stmts '}' { ConstrDecl $1 0 $3 (toStmt $6) }

expr    : var '(' exprs ')' { Prim $1 $3 }
        | arg equal arg    { Prim "==" [$1, $3] }
        | arg '+' arg      { Prim "+" [$1, $3] }
        | arg '-' arg      { Prim "-" [$1, $3] }
        | arg '*' arg      { Prim "*" [$1, $3] }
        | arg '/' arg      { Prim "/" [$1, $3] }
        | primary          { $1 }

primary : vars                         { $1 }
        | true                         { ConstTrue }
        | false                        { ConstFalse }
        | null                         { ConstNull }
        | num                          { ConstNum $1 }
        | lit                          { ConstLit $1 0 }
        | char                         { ConstChar $1 }
        | new typename '(' exprs ')'   { New (TypeName $2) $4 0 }
        | new typename '[' exprs ']'   { New (ArrayTypeName (TypeName $2)) $4 0 }
        | '(' expr ')'                 { $2 }
        | '(' type ')' expr            { Cast $2 $4 }
        | vars '=' expr                { Assign $1 $3 }
        | arg lt arg                   { Prim "<" [$1, $3] }
        | arg noteq arg                { Prim "!=" [$1, $3] }
        | arg inc                      { Prim "++" [$1, $1] }
        | arg dec                      { Prim "--" [$1, $1] }

vars    : var                          { Var $1 }
        | vars '.' var                 { Field $1 $3 Nothing }
        | vars '[' expr ']'            { Prim "[]" [$1, $3] }
        | vars '.' var '(' exprs ')'    { Invoke $1 $3 $5 Nothing }
        | typename  '.' var            { StaticField (TypeName $1) $3 Nothing }
        | typename '.' var '(' exprs ')'{ StaticInvoke (TypeName $1) $3 $5 Nothing }

stmts   :            { [] }
        | stmt stmts { $1 : $2 }

blockstmts
        : '{' stmts '}'  { Block $ toStmt $2 }

stmt    : expr ';'  { Expr $1 }
        | if '(' expr ')' blockstmts                 { Ite $3 $5 NoStmt }
        | if '(' expr ')' blockstmts else stmt { Ite $3 $5 $7 }
        | final type var '=' expr ';' { LocalVarDecl $2 $3 0 (Just $5) }
        | type var '=' expr ';' { LocalVarDecl $1 $2 0 (Just $4) }
        | type var ';'          { LocalVarDecl $1 $2 0 Nothing }
        | return ';'      { Return Nothing }
        | return expr ';' { Return (Just $2) }
        | while '(' expr ')' blockstmts   { While $3 $5 }
        | for '(' var '=' expr ';' expr ';' expr ')' blockstmts
           { For Nothing $3 $5 $7 $9 $11 }
        | for '(' type var '='  expr ';' expr ';' expr ')' blockstmts
	   { For (Just ($3,0) ) $4 $6 $8 $10 $12 }
        | ';'  { NoStmt }
        | blockstmts { $1 }

decl    : N      { [$1] }
        | N decl { $1 : $2 }

memberDecl
        :               { [] }
        | optAccess optFinal optStatic F memberDecl	
          { let attr = if $3 then ["static"] else [] in
            ($4 attr) : $5 }
        | optAccess optFinal optStatic M memberDecl	
          { let attr = if $3 then ["static"] else [] in
            ($4 attr) : $5 }
        | optAccess optFinal optStatic K memberDecl	
          { $4 : $5 }

abstractMemberDecl
        :                               { [] }
        | optAccess optFinal abstractM  abstractMemberDecl	{ $3 : $4 }

varDecl	: 		                { [] }
        | varDeclTheRest		{ $1 }

varDeclTheRest
        : optFinal type var                     { [($2, $3, 0)] }
        | optFinal type var ',' varDeclTheRest	{ ($2, $3, 0) : $5 }

exprs   :                               { [] }
        | therestexprs                  { $1 }

therestexprs
        : expr                          { [$1] }
        | expr ',' therestexprs         { $1 : $3 }

arg     : vars  { $1 }
        | lit   { ConstLit $1 0 }
        | char  { ConstChar $1 }
        | num   { ConstNum $1 }
        | true  { ConstTrue }
        | false { ConstFalse }
        | null  { ConstNull }

type    : typename     { TypeName $1 }
        | type '[' ']' { ArrayTypeName $1 }

typename: classname { $1 }
        | primname  { $1 }

optAccess
        :        { False }
        | access { True }

access
        : public    { 1 }
        | protected { 2 }
        | private   { 3 }

optAbstract
        :          { False }
        | abstract { True }

optStatic
        :        { False }
        | static { True }

optFinal
        :       { False }
        | final { True }

{


parseError :: [Token] -> a
parseError toks = error ("Parse error at " ++
			 (concat 
			  $ intersperse " " 
			  $ map toStr 
			  $ take 50
			  $ toks))

toStr :: Token -> String
toStr TokenSemiColon = ";"
toStr TokenDot       = "."
toStr TokenComma     = ","
toStr TokenOP        = "("
toStr TokenCP        = ")"
toStr TokenOB        = "{"
toStr TokenCB        = "}"
toStr TokenOBK       = "["
toStr TokenCBK       = "]"
toStr TokenEQ        = "="
toStr TokenPlus      = "+"
toStr TokenMinus     = "-"
toStr TokenStar      = "*"
toStr TokenDiv       = "/"
toStr TokenClass     = "class"
toStr TokenExtends   = "extends"
toStr TokenNew       = "new"
toStr TokenIf        = "if"
toStr TokenElse      = "else"
toStr TokenTrue      = "true"
toStr TokenFalse     = "false"
toStr TokenNull      = "null"
toStr TokenReturn    = "return"

toStr TokenPackage   = "package"
toStr TokenImport    = "import"
toStr TokenPublic    = "public"
toStr TokenProtected = "protected"
toStr TokenPrivate   = "private"
toStr TokenStatic    = "static"
toStr TokenAbstract  = "abstract"
toStr TokenInterface = "interface"
toStr TokenImplements = "implements"
toStr TokenFinal = "final"
toStr TokenFor = "for"
toStr TokenWhile = "while"

toStr TokenEqual     = "=="
toStr TokenInc       = "++"
toStr TokenDec       = "--"
toStr TokenLT        = "<"
toStr TokenNotEq     = "!="
toStr (TokenNum n)   = show n
toStr (TokenLit s)   = show s
toStr (TokenChar c)  = show c
toStr (TokenVar v)   = v
toStr (TokenClassType t)  = t
toStr (TokenPrimitiveType t)  = t

data Token = TokenSemiColon
     	   | TokenDot
     	   | TokenComma
	   | TokenOP
	   | TokenCP
	   | TokenOB
	   | TokenCB
	   | TokenOBK
	   | TokenCBK
	   | TokenEQ
	   | TokenPlus
	   | TokenMinus
	   | TokenStar
	   | TokenDiv
	   | TokenClass
	   | TokenExtends
	   | TokenNew
	   | TokenIf
	   | TokenElse
	   | TokenTrue
	   | TokenFalse
	   | TokenReturn
	   | TokenNull

	   | TokenPackage
	   | TokenImport
	   | TokenPublic
	   | TokenProtected
	   | TokenPrivate
	   | TokenStatic
	   | TokenAbstract
	   | TokenInterface
	   | TokenImplements
	   | TokenFinal
	   | TokenFor
	   | TokenWhile

	   | TokenNum String
	   | TokenLit String
	   | TokenChar String
	   | TokenVar String
	   | TokenClassType String
	   | TokenPrimitiveType String
	   | TokenEqual
           | TokenLT
           | TokenNotEq
           | TokenInc
           | TokenDec
	   deriving Show


lexer :: String -> [Token]
lexer [] = []
lexer ('/':'/':cs) = lexer (dropWhile (/= '\n') cs)
lexer ('/':'*':cs) = lexer (dropComment cs)
lexer (';':cs) = TokenSemiColon : lexer cs
lexer ('.':cs) = TokenDot : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('{':cs) = TokenOB : lexer cs
lexer ('}':cs) = TokenCB : lexer cs
lexer ('[':cs) = TokenOBK : lexer cs
lexer (']':cs) = TokenCBK : lexer cs
lexer ('=':'=':cs) = TokenEqual : lexer cs
lexer ('!':'=':cs) = TokenNotEq : lexer cs
lexer ('<':cs) = TokenLT : lexer cs
lexer ('+':'+':cs) = TokenInc : lexer cs
lexer ('-':'-':cs) = TokenDec : lexer cs
lexer ('=':cs) = TokenEQ : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenStar : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer (c:cs) | isSpace c = lexer cs
      	     | isAlpha c = lexVar (c:cs)
	     | isDigit c = lexNum (c:cs)
	     | c == '\"' = lexLit (c:cs)
	     | c == '\'' = lexChar (c:cs)
lexer (c:cs) = error [c]

dropComment [] = []
dropComment [c] = []
dropComment ('*':'/':cs) = cs
dropComment (c:cs) = dropComment cs

isVarChar c = isAlpha c || isDigit c || c == '_' || c == '\''

lexVar cs = case span isVarChar cs of
       	       ("class", rest)   -> TokenClass : lexer rest
       	       ("extends", rest) -> TokenExtends : lexer rest
       	       ("new", rest)     -> TokenNew : lexer rest
       	       ("if", rest)      -> TokenIf : lexer rest
       	       ("else", rest)    -> TokenElse : lexer rest
       	       ("true", rest)    -> TokenTrue : lexer rest
       	       ("false", rest)   -> TokenFalse : lexer rest
       	       ("return", rest)  -> TokenReturn : lexer rest
       	       ("null", rest)    -> TokenNull : lexer rest
       	       ("package", rest) -> TokenPackage : lexer rest
       	       ("import", rest)  -> TokenImport : lexer rest
       	       ("public", rest)  -> TokenPublic : lexer rest
       	       ("protected", rest)  -> TokenProtected : lexer rest
       	       ("private", rest)  -> TokenPrivate : lexer rest
       	       ("static", rest)  -> TokenStatic : lexer rest
       	       ("abstract", rest) -> TokenAbstract : lexer rest
       	       ("interface", rest) -> TokenInterface : lexer rest
       	       ("implements", rest) -> TokenImplements : lexer rest
       	       ("final", rest)   -> TokenFinal : lexer rest
       	       ("for", rest)     -> TokenFor : lexer rest
       	       ("while", rest)   -> TokenWhile : lexer rest
	       (ident, rest)     -> varOrType ident : lexer rest

varOrType s = if cond1 then TokenClassType s 
              else if cond2 then TokenPrimitiveType s 
              else TokenVar s
  where
     cond1 = elem (head s) ['A'..'Z']
     cond2 = elem s ["int", "boolean", "char", "float", "byte", "void"]

lexNum cs = TokenNum num : lexer rest
   where (num,rest) = span isDigit cs

lexLit (_:cs) = TokenLit lit : lexer cs'
   where
      (lit,cs') = tw "" cs
      tw a [] = error ("Missing \" in " ++ reverse a)
      tw a (c:cs) | c /= '\"' = tw (c:a) cs
                  | otherwise = (reverse a, cs)

lexChar (_:cs) = TokenChar charstr : lexer cs'
   where
      (charstr,cs') = tw "" cs
      tw a [] = error ("Missing \' in " ++ reverse a)
      tw a (c:cs) | c /= '\'' = tw (c:a) cs
                  | otherwise = (reverse a, cs)


}

