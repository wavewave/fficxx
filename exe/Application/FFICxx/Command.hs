module Application.FFICxx.Command where

import Application.FFICxx.ProgType
import Application.FFICxx.Job

commandLineProcess :: FFICxx -> IO ()
commandLineProcess (Generate conf) = do 
  putStrLn "generate called"
  startGenerateJob conf