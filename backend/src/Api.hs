{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api
    ( someFunc
    ) where



import           GHC.Generics (Generic)
import           Servant.API  ((:>), Capture, Get, JSON)


someFunc :: IO ()
someFunc = putStrLn "someFunc"
