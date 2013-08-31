import Control.Monad.Identity
import Control.Monad.State
import Data.Monoid
import Data.List (intercalate)
-- 
import FFIC


test1 = do 
  let typ1 = CSimple (SOpaq "A") :: Composite String 
      (proj1,conv1) = project typ1
      var = Var typ1 "x"  
      var_p = Var proj1 "x" 
  print typ1
  print proj1
  print conv1 
  print (mkVarDecl var)
  print (mkVarDecl var_p)
 
test2 = do 
  let typ2 = CPtr (CSimple (SOpaq "A")) :: Composite String 
      (proj2, conv2) = project typ2
      var = Var typ2 "x"  
      var_p = Var proj2 "x" 

  print typ2 
  print proj2
  print conv2
  print (mkVarDecl var)
  print (mkVarDecl var_p)



test3 = do 
  let typ3 = CPtr (CPtr (CSimple (SOpaq "A"))) :: Composite String 
      (proj3, conv3) = project typ3
      var = Var typ3 "x"  
      var_p = Var proj3 "x" 

  print typ3 
  print proj3
  print conv3 
  print (mkVarDecl var)
  print (mkVarDecl var_p)


test4 = do 
  let typ4 = (CRef (CSimple (SOpaq "A"))) :: Composite String 
      (proj4, conv4) = project typ4
      var = Var typ4 "x"  
      var_p = Var proj4 "x" 

  print typ4 
  print proj4
  print conv4
  print (mkVarDecl var)
  print (mkVarDecl var_p)

test5 = do 
  let typ5 = (CPtr (CRef (CSimple (SOpaq "A")))) :: Composite String 
      (proj5, conv5) = project typ5

  print typ5 
  print proj5
  print conv5 

test6 = do 
  let typ6 = (CRef (CPtr (CSimple (SOpaq "A")))) :: Composite String 
      (proj6, conv6) = project typ6

  print typ6 
  print proj6
  print conv6 


test7 = do 
  let typ1 = CSimple (SOpaq "A") :: Composite String 
      typ2 = (CSimple (SPrim PrimInt)) :: Composite String
      typ3 = CSimple (SPrim PrimVoid)

      proj1 = (cp_after.mkCPPair) typ1
      proj2 = (cp_after.mkCPPair) typ2
      proj3 = (cp_after.mkCPPair) typ3

      f_before = Function "test" [Var typ1 "x", Var typ2 "y"] typ3
      f_after = Function "test" [Var proj1 "x", Var proj2 "y"] proj3
  putStrLn $ " before: " ++ (mkFuncDecl f_before)
  putStrLn $ " after: " ++ (mkFuncDecl f_after)
  putStrLn $ (mkConvStr "x" . mkCPPair) typ1


test8 = let strs = mkOpaqueTypedef "A"
            result = intercalate "; \n" strs
        in putStrLn result  


test9 = do 
  let typ1 = CSimple (SOpaq "A") :: Composite String
      typ2 = CPtr (CSimple (SOpaq "A")) :: Composite String 
      typ3 = CSimple (SPrim PrimChar) :: Composite String
      typ4 = CPtr (CPtr (CSimple (SOpaq "A"))) :: Composite String 
      typ5 = CRef (CSimple (SOpaq "A")) :: Composite String 
      typ6 = CRef (CPtr (CSimple (SOpaq "A"))) :: Composite String
  putStrLn $ (mkConvStr "x" . mkCPPair) typ1
  putStrLn $ "1: " ++ show typ1 ++ ":" ++ (mkConvStr "x" . mkCPPair) typ1
  putStrLn $ "2: " ++ show typ2 ++ ":" ++ (mkConvStr "x" . mkCPPair) typ2
  putStrLn $ "3: " ++ show typ3 ++ ":" ++ (mkConvStr "x" . mkCPPair) typ3
  putStrLn $ "4: " ++ show typ4 ++ ":" ++ (mkConvStr "x" . mkCPPair) typ4
  putStrLn $ "5: " ++ show typ5 ++ ":" ++ (mkConvStr "x" . mkCPPair) typ5
  putStrLn $ "6: " ++ show typ6 ++ ":" ++ (mkConvStr "x" . mkCPPair) typ6

  putStrLn "-------------------"

  putStrLn $ "1: " ++ show typ1 ++ ":" ++ (mkRevConvStr "x" . mkCPPair) typ1
  putStrLn $ "2: " ++ show typ2 ++ ":" ++ (mkRevConvStr "x" . mkCPPair) typ2
  putStrLn $ "3: " ++ show typ3 ++ ":" ++ (mkRevConvStr "x" . mkCPPair) typ3
  putStrLn $ "4: " ++ show typ4 ++ ":" ++ (mkRevConvStr "x" . mkCPPair) typ4
  putStrLn $ "5: " ++ show typ5 ++ ":" ++ (mkRevConvStr "x" . mkCPPair) typ5
  putStrLn $ "6: " ++ show typ6 ++ ":" ++ (mkRevConvStr "x" . mkCPPair) typ6

  print ((cp_conv . mkCPPair) typ6)



test10 = do 
  let typ1 = CSimple (SOpaq "A") :: Composite String 
      typ2 = (CSimple (SPrim PrimInt)) :: Composite String
      typ3 = (CSimple (SOpaq "B")) :: Composite String -- CSimple (SPrim PrimVoid)

      proj1 = (cp_after.mkCPPair) typ1
      proj2 = (cp_after.mkCPPair) typ2
      proj3 = (cp_after.mkCPPair) typ3


      pair1 = mkCPPair typ1 
      pair2 = mkCPPair typ2 
      pair3 = mkCPPair typ3


      f_before = Function "test" [Var typ1 "x", Var typ2 "y"] typ3
      f_after = Function "test" [Var proj1 "x", Var proj2 "y"] proj3
      f = Function "test" [Var pair1 "x", Var pair2 "y"] pair3  
  putStrLn $ " before: " ++ (mkFuncDecl f_before)
  putStrLn $ " after: " ++ (mkFuncDecl f_after)
  putStrLn $ (mkFuncCallStr f)
  putStrLn "==============="
  putStrLn $ mkFuncDef f
  -- putStrLn $ (mkConvStr "x" . mkCPPair) typ1




main = test10


main'  = do 
  putStrLn "Testing"

  let typ1 = CPtr (CPtr (CSimple (SOpaq "A"))) :: Composite String 
      typ2 = CPtr (CSimple (SPrim PrimChar)) :: Composite String
      typ3 = (CSimple (SPrim PrimInt)) :: Composite String 

      -- v1 = Var typ1 "a" 
      -- v2 = Var typ2 "b" 

      (a,s) = project typ1
      (a',s') = project typ2
      (a'',s'') = project typ3

      v1 = Var a "a" 
      v2 = Var a' "b"

      f = Function "test" [v1,v2] a''
  
      t1 = mkCPPair typ1
      t2 = mkCPPair typ2 
      vt1 = Var t1 "x"
      vt2 = Var t2 "y"

  print a 
  print s
  putStrLn (mkCTypeFromProjected a)


  print a' 
  print s'  
  putStrLn (mkCTypeFromProjected a')

  putStrLn (mkArgs [v1,v2])
  putStrLn (mkFuncDecl f)
  
  mapM_ putStrLn (mkOpaqueTypedef "testtest")

  putStrLn (mkCallArg vt1) 
