import Foreign.C

import Foreign.Marshal.Array  
import MySample3

testdat :: [CChar] 
testdat = map fromIntegral [65,66,67,68,0] 

main :: IO ()
main = do 
  c_testdat <- newArray testdat 
  

  a <- newA 
  b <- newB 
  
  foo a 
  foo b 

  foo (upcastA b) 

  bar b c_testdat 


