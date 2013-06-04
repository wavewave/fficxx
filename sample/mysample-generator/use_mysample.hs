import MySample

main :: IO ()
main = do 
  a <- newA 
  b <- newB 
  
  foo a 
  foo b 

  foo (upcastA b) 

  bar b 


