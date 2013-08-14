module Library where

import AST

objClass = "Object"
strClass = "String"

basicClasses = -- TODO: extension
  [ (strClass,   [java_class]),
    (objClass,   [java_class]),
    ("HashSet",  [java_class]),
    ("Set",      [java_class]), -- interface?
    ("Iterator", [java_class])  -- interface?
  ]

basicInheritance =  -- TODO: extension
  [ (strClass, objClass),
    ("HashSet", "Set")
  ]

basicFields = -- TODO: extension
  [
    ("System", [(TypeName "PrintStream", "out", [static])])
  ]
  

basicMtypes = -- TODO: extension  
  (\x -> concat 
         [ [ (c, m, id, argtys, retty, attrs, args, maybestmt) 
           | ((c, m, argtys, retty, attrs, args, maybestmt),id) 
             <- zip mtypesPerClass [1..] ] 
         | mtypesPerClass <- x ] )
  [ 
    [
      ("Set", "add", [TypeName "Object"], TypeName "boolean", [], [], Nothing),
      ("Set", "remove", [TypeName "Object"], TypeName "boolean", [], [], Nothing),
      ("Set", "iterator", [], TypeName "Iterator", [], [], Nothing)
    ],
    
    [
      ("HashSet", "add", [TypeName "Object"], TypeName "boolean", [], [], Nothing),
      ("HashSet", "remove", [TypeName "Object"], TypeName "boolean", [], [], Nothing),
      ("HashSet", "iterator", [], TypeName "Iterator", [], [], Nothing)
    ],
    
    [
      ("Iterator", "hasNext", [], TypeName "boolean", [], [], Nothing),
      ("Iterator", "next", [], TypeName "Object", [], [], Nothing),
      ("Iterator", "remove", [], TypeName "void", [], [], Nothing)
    ],
    
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
      ("PrintStream", "println", [TypeName "String"], TypeName "void", [static], [], Nothing)
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
