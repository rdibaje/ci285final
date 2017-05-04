{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

import           Yesod
--import Import
import Data.Text
import Control.Applicative ((<$>),(<*>))
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, withSmallInput)




data HelloWorld = HelloWorld
--
data Operands = Operands
    { leftOperand :: Integer
    , rightOperand :: Integer
    }

    
mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
/add/#Integer/#Integer AddR GET
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

getAddR :: Integer -> Integer -> Handler Html
getAddR a b = let x = a + b in defaultLayout [whamlet|#{x}|]

main :: IO ()
main = warp 8080 HelloWorld


--sampleForm :: AForm (Text, Text)
--sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
--   <$> areq textField (withSmallInput "ABC") Nothing
--   <*> areq textField (withSmallInput "DEF") Nothing


data Car = Car
    { carModel :: Text
    , carYear  :: Int
    }
  deriving Show

carAForm :: AForm Handler Car
carAForm = Car
    <$> areq textField "Model" Nothing
    <*> areq intField "Year" Nothing

carForm :: Html -> MForm Handler (FormResult Car, Widget)
carForm = renderTable carAForm