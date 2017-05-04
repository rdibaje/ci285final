{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

import           Control.Applicative ((<$>), (<*>))
import           Data.Text           (Text, pack)
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/cal CalculationR GET
/add/#Integer/#Integer AddR GET
/sub/#Integer/#Integer SubR GET
/mul/#Integer/#Integer MulR GET
|]

instance Yesod App

-- Tells our application to use the standard English messages.
-- If you want i18n, then you can supply a translating function instead.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- The datatype we wish to receive from the form

data Operand = Addition | Subtraction | Multiplication deriving (Show, Eq, Enum, Bounded)
data BinaryOperation = BinaryOperation { left :: Integer, right :: Integer, operand :: Operand } deriving Show


calculationAForm ::  AForm Handler BinaryOperation
calculationAForm = BinaryOperation
       <$> areq intField "First number" (Just 0)
       <*> areq intField "Second number" (Just 0)
       <*> areq (selectFieldList operands) "Operation" (Just Addition)
           where
             operands :: [(Text, Operand)]
             operands = [("Addition", Addition), ("Subtraction", Subtraction), ("Multiplication", Multiplication)]
             
calculationForm :: Html -> MForm Handler (FormResult BinaryOperation, Widget)
calculationForm = renderTable calculationAForm

-- The GET handler displays the form
getHomeR :: Handler Html
getHomeR = do
    -- Generate the form to be displayed
    (widget, enctype) <- generateFormGet' calculationForm
    defaultLayout
        [whamlet|
            <p>
            <form action=@{CalculationR} enctype=#{enctype}>
                ^{widget}
                <button>Submit
        |]

calculationResult :: BinaryOperation -> Integer
calculationResult (BinaryOperation l r Addition) = l + r
calculationResult (BinaryOperation l r Subtraction) = l - r
calculationResult (BinaryOperation l r Multiplication) = l * r


getAddR :: Integer -> Integer -> Handler TypedContent
getAddR x y = selectRep $ do
    provideRep $ defaultLayout $ do
        setTitle "Addition"
        [whamlet|#{x} + #{y} = #{z}|]
    provideJson $ object ["result" .= z]
  where
    z = x + y

getSubR :: Integer -> Integer -> Handler TypedContent
getSubR x y = selectRep $ do
    provideRep $ defaultLayout $ do
        setTitle "Subtraction"
        [whamlet|#{x} - #{y} = #{z}|]
    provideJson $ object ["result" .= z]
  where
    z = x - y

getMulR :: Integer -> Integer -> Handler TypedContent
getMulR x y = selectRep $ do
    provideRep $ defaultLayout $ do
        setTitle "Multiplication"
        [whamlet|#{x} * #{y} = #{z}|]
    provideJson $ object ["result" .= z]
  where
    z = x * y
    
    
getCalculationR :: Handler Html
getCalculationR = do
    ((result, widget), enctype) <- runFormGet calculationForm
    case result of
        FormSuccess b@BinaryOperation {left = l, right = r, operand = Addition} -> defaultLayout
           [whamlet|
            <p>#{show $ calculationResult b}
            <a href=@{AddR l r}>Addition (click to make RESTFul GET request)
           |]
           
        FormSuccess b@BinaryOperation {left = l, right = r, operand = Subtraction} -> defaultLayout
           [whamlet|
            <p>#{show $ calculationResult b}
            <a href=@{SubR l r}>Subtraction (click to make RESTFul GET request)
           |]
        FormSuccess b@BinaryOperation {left = l, right = r, operand = Multiplication} -> defaultLayout
           [whamlet|
            <p>#{show $ calculationResult b}
            <a href=@{MulR l r}>Multiplication (click to make RESTFul GET request)
           |]   
        _ -> defaultLayout
             [whamlet|"failed"|]


main :: IO ()
main = warp 8080 App
