import FFICXX.Generate.Type.Class

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
               , (QVolatile (Ptr (PrimType CPTChar)))
               , (QRestrict (Ptr (PrimType CPTChar)))
               , (MPtr (ClassName "ClassB") CPTLongDouble)
               , (Arr 10 (Arr 100 (PrimType CPTUChar)))
               , (Ptr (Fun [(Ptr (PrimType CPTChar)), (Ptr (Ptr (QConst (PrimType CPTDouble))))] (Ptr (PrimType CPTBool))))
               , (PrimType (CPTClass (ClassName "SomeClass")))
               , (Fun [(Ref (PrimType (CPTClass (ClassName "ClassA"))))] (PrimType (CPTClass (ClassName "ClassA"))))
               , (Ptr (Arr 10 (PrimType CPTChar)))
               , (Ptr (QConst (PrimType CPTULongLong)))
               ]


main :: IO ()
main = do 
  putStrLn "testing" 
  mapM_ (putStrLn . cppname) testCPPTypes -- testTypes 
  