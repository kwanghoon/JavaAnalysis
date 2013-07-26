module Main where

import AST
import Parser
import TypeCheck
-- import TypedAST
import System.Environment
import Data.List

main =
  do args <- getArgs
     run args
     
run args =
  do css <- mapM parse args
     let cs = concat css
     putStrLn $ prprog $ cs
     typecheck cs
          
parse arg =           
  do s <- readFile arg
     return (parseprog . lexer $ s)
          
mainToks = 
  do args <- getArgs
     tss <- mapM toks args
     putStrLn $ concat $ intersperse " " $ concat $ map (map toStr) $ tss
          
toks arg = 
  do s <- readFile arg
     return (lexer $ s)
  

dotest filelist = 
  do run filelist
     putStrLn "done."
     
tc_android = map ("./sample/androidgame/" ++)
             [ 
               "Android.java",
               "Game.java"
             ]
             
tc_array =  map ("./sample/array/" ++)
             [ 
               "Array.java"
             ]            
            
tc_visitor = map ("./sample/visitorpattern/" ++)
            [
              "AbstractNode.java",
              "Class.java",
              "CompilationUnit.java",
              "Compiler.java",
              "Field.java",
              "Method.java",
              "StatementCompound.java",
              "StatementIf.java",
              "Statement.java",
              "StatementSimple.java",
              "StatementWhile.java",
              "client/Client.java",
              "client/PrettyPrinter.java",
              "visitor/IVisitable.java",
              "visitor/IVisitor.java",
              "visitor/VisitorAdapter.java"
            ]
             
tc_tosem1 = map ("./sample/tosem/" ++)             
           [
             "Container.java"
           ]
            
tc_tosem2 = map ("./sample/tosem/" ++)             
           [
             "FieldEncapsulation.java"
           ]
             
tc_tosem3 = map ("./sample/tosem/" ++)             
           [
             "FieldAssignmentThroughSuperclass.java"
           ]

tc_tosem4 = map ("./sample/tosem/" ++)             
           [
             "ListCopy.java"
           ]

tc_refl = map ("./sample/reflection/" ++)
          [
            "Location.java",
            "ReflectTest.java"
          ]
          
          