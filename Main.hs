module Main where

import AST
import Parser
import TypeCheck
import Analysis
import System.Environment
import Data.List
import Data.Maybe

main =
  do args <- getArgs
     run args
     
run args =
  do css <- mapM parse args
     let cs = numProgram $ concat $ css
     putStrLn $ prprog $ cs
     maybeinfoprog <- typecheck cs
     let (info,program) = fromJust maybeinfoprog
     if isNothing maybeinfoprog
        then return ()
        else doAnalysis program info
          
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
             
tc_tosem0 = map ("./sample/tosem/" ++)             
           [
             "Pointsto.java"
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
          
tc_java = map ("./sample/java/" ++)
          [
            "LocVar.java",
            "Lhs.java",
            "Static.java",
            "MemberInit.java",
            "Str.java"
          ]         