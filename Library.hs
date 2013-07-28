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
  [ 
    ("Set", "add", [TypeName "Object"], TypeName "boolean", []),
    ("Set", "remove", [TypeName "Object"], TypeName "boolean", []),
    ("Set", "iterator", [], TypeName "Iterator", []),
    
    ("HashSet", "add", [TypeName "Object"], TypeName "boolean", []),
    ("HashSet", "remove", [TypeName "Object"], TypeName "boolean", []),
    ("HashSet", "iterator", [], TypeName "Iterator", []),
    
    ("Iterator", "hasNext", [], TypeName "boolean", []),
    ("Iterator", "next", [], TypeName "Object", []),
    ("Iterator", "remove", [], TypeName "void", []),
    
    ("StringBuilder", "StringBuilder", [], TypeName "StringBuilder", []),
    ("StringBuilder", "append", [TypeName "String"], TypeName "StringBuilder", []),
    ("StringBuilder", "append", [TypeName "char"], TypeName "StringBuilder", []),
    ("StringBuilder", "setLength", [TypeName "int"], TypeName "void", []),
    ("StringBuilder", "toString", [], TypeName "String", []),
    
    ("String", "equals", [TypeName "String"], TypeName "boolean", []),

    ("PrintStream", "println", [TypeName "String"], TypeName "void", [static])
  ]
