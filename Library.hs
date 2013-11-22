module Library where

import AST
import Data.Maybe

objClass = "Object"
strClass = "String"

basicClasses :: UserClasses
basicClasses = -- TODO: extension
  [ (strClass,   [java_class]),
    (objClass,   [java_class])
  -- ,
  --   ("HashSet",  [java_class]),
  --   ("Set",      [java_class]), -- interface?
  --   ("Iterator", [java_class])  -- interface?
  ]

basicInheritance :: Inheritance
basicInheritance =  -- TODO: extension
  [ (strClass, objClass)
  -- ,
  --   ("HashSet", "Set"),
  --   ("Set", objClass),
  --   ("Iterator", objClass)
  ]

basicFields :: Fields
basicFields = -- TODO: extension
  [
    ("System", [(TypeName "PrintStream", "out", [static])])
  ]
  

basicMtypes :: Mtypes
basicMtypes = -- TODO: extension  
  (\x -> concat 
         [ [ (c, m, id, argtys, retty, attrs, args, maybestmt) 
           | ((c, m, argtys, retty, attrs, args, maybestmt),id) 
             <- zip mtypesPerClass [1..] ] 
         | mtypesPerClass <- x ] )
  [ 
    [ 
      ("Object", "getClass", [], TypeName "Class<?>", [], [], Nothing)
    ],
    
    [
      ("Class<?>", "getName", [], TypeName "String", [], [], Nothing)
    ],
    
    -- [
    --   ("Set", "add", [TypeName "Object"], TypeName "boolean", [], [], Nothing),
    --   ("Set", "remove", [TypeName "Object"], TypeName "boolean", [], [], Nothing),
    --   ("Set", "iterator", [], TypeName "Iterator", [], [], Nothing)
    -- ],
    
    -- [
    --   ("HashSet", "add", [TypeName "Object"], TypeName "boolean", [], [], Nothing),
    --   ("HashSet", "remove", [TypeName "Object"], TypeName "boolean", [], [], Nothing),
    --   ("HashSet", "iterator", [], TypeName "Iterator", [], [], Nothing)
    -- ],
    
    -- [
    --   ("Iterator", "hasNext", [], TypeName "boolean", [], [], Nothing),
    --   ("Iterator", "next", [], TypeName "Object", [], [], Nothing),
    --   ("Iterator", "remove", [], TypeName "void", [], [], Nothing)
    -- ],
    
    [
      ("StringBuilder", "StringBuilder", [], TypeName "StringBuilder", [], [], Nothing),
      ("StringBuilder", "append", [TypeName "String"], TypeName "StringBuilder", [], [], Nothing),
      ("StringBuilder", "append", [TypeName "char"], TypeName "StringBuilder", [], [], Nothing),
      ("StringBuilder", "setLength", [TypeName "int"], TypeName "void", [], [], Nothing),
      ("StringBuilder", "toString", [], TypeName "String", [], [], Nothing)
    ],
    
    [
      ("String", "equals", [TypeName "String"], TypeName "boolean", [], [], Nothing)
    ],

    [
      ("PrintStream", "println", [TypeName "String"], TypeName "void", [static], [], Nothing),
      ("PrintStream", "println", [TypeName "StringBuilder"], TypeName "void", [static], [], Nothing)
    ]
  ]

--            
primTypeTable = 
  [ 
    ("<", [TypeName "int", TypeName "int"], TypeName "boolean"),
    ("++", [TypeName "int", TypeName "int"], TypeName "int"),
    ("--", [TypeName "int", TypeName "int"], TypeName "int"),
    ("primStartActivity", [TypeName "Intent"], TypeName "void"),
    ("primAddButton", [TypeName "int"], TypeName "void")
  ]

lookupPrim :: PrimName -> [TypeName] -> [TypeName]
lookupPrim n tys = rettys
  where
    mtypes = [(argtys,retty) | (p, argtys, retty) <- primTypeTable, p == n]
    rettys = [retty | (argtys, retty) <- mtypes
                    , length tys == length argtys
                    , all (True==) [eqType ty1 ty2 | (ty1,ty2) <- zip tys argtys] ]

lookupOverridenMethod :: Info -> ClassName -> MemberDecl 
                         -> Maybe (ClassName, MethodName, UniqueId)
lookupOverridenMethod info c mdecl =
  case matchedmethods of 
    []     -> lookupOverridenMethod' c
    [cmid] -> Just cmid
    _      -> error "lookupOverridenMethod: Can't have multiple overriding methods"
      
  where
    (MethodDecl attrs retty m id argdecls stmt) = mdecl
    argtys = [argty | (argty,_,_) <- argdecls]
    mtypes = getMtypes info ++ basicMtypes
    matchedmethods = 
      [ (c',m',id') | (c',m',id',argtys',retty',attrs', vars', stmt') <- mtypes,
        let maybep = parentClass info c,
        isJust maybep,
        let p = fromJust maybep,
        p==c' && m==m' && attrs==attrs' && retty==retty' && argtys==argtys' ]
      
    lookupOverridenMethod' c = 
      case parentClass info c of 
        Nothing -> Nothing
        Just p  -> lookupOverridenMethod info p mdecl
      
parentClass :: Info -> ClassName -> Maybe ClassName
parentClass info c =
  if c == objClass then Nothing
  else case pcs of
    [p] -> Just p
    []  -> error $ "parentClass: no parent class for " ++ c
    ps  -> error $ "parentClass: too many parent classes " ++ show ps ++ " for " ++ c
  where
    inheritance = getInheritance info ++ basicInheritance
    pcs = [ p | (c',p) <- inheritance, c==c' && isClass p ]
    
    classes = getUserClasses info ++ basicClasses
    isClass c = 
      not $ null [ c' | (c',attrs') <- classes, c==c' && elem java_class attrs' ]
    
