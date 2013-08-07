{-# LANGUAGE TupleSections #-}
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Code.Cpp

testTypes :: [PrimitiveTypes ClassName]
testTypes = [ CPTChar
            , CPTInt
            , CPTLong
            , CPTUChar
            , CPTUInt
            , CPTULong
            , CPTLongLong
            , CPTULongLong
            , CPTDouble
            , CPTLongDouble
            , CPTBool
            , CPTVoid
            , CPTClass (ClassName "MyClass") ] 

testCPPTypes :: [ CPPType ClassName ]
testCPPTypes = [ Ptr (PrimType CPTChar) 
               , (QConst (Ptr (PrimType CPTChar)))
               , (QConst (Ptr (Ptr (PrimType CPTVoid))))
               , (Ptr (QVolatile (PrimType CPTChar)))
               , (QVolatile (Ptr (PrimType CPTChar)))
               , (QRestrict (Ptr (PrimType CPTChar)))
               , (MPtr (ClassName "ClassB") CPTLongDouble)
               , (Arr 10 (Arr 100 (PrimType CPTUChar)))
               , (Ptr (Fun [(Ptr (PrimType CPTChar)), (Ptr (Ptr (QConst (PrimType CPTDouble))))] (Ptr (PrimType CPTBool))))
               , (PrimType (CPTClass (ClassName "SomeClass")))
               , (Fun [(Ref (PrimType (CPTClass (ClassName "ClassA"))))] (PrimType (CPTClass (ClassName "ClassA"))))
               , (Ptr (Arr 10 (PrimType CPTChar)))
               , (Ptr (QConst (PrimType CPTULongLong)))
               , (Ref (PrimType (CPTClass (ClassName "ClassRef"))))
               ]


testTopLevelFunctions :: [ TopLevelFunction ClassName ]
testTopLevelFunctions = [ TopLevelFunction (PrimType CPTChar) "fun1" [(PrimType CPTChar, "arg1")] Nothing Nothing ]

main :: IO ()
main = do 
  putStrLn "cppname:" 
  mapM_ (putStrLn . cppname) testCPPTypes -- testTypes 
  putStrLn ""
  putStrLn "rettypeToString:"
  mapM_ (putStrLn . rettypeToString) testCPPTypes -- test return type
  putStrLn ""
  putStrLn "argToString:"
  mapM_ (putStrLn . argToString . (,"argName")) testCPPTypes
  putStrLn ""
  putStrLn "argToCallString:"
  mapM_ (putStrLn . argToCallString . (,"argName")) testCPPTypes
  putStrLn ""
  putStrLn "genTopLevelFuncCppHeader:"
  mapM_ (putStrLn . genTopLevelFuncCppHeader) testTopLevelFunctions
  putStrLn ""
  putStrLn "genTopLevelFuncCppDefinition:"
  mapM_ (putStrLn . genTopLevelFuncCppDefinition) testTopLevelFunctions
