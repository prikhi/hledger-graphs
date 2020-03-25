module Main where

import           Data.Default                   ( def )
import           Graphics.Rendering.Chart.Backend.Diagrams
                                                ( toFile )

import           Lib

main :: IO ()
main = do
    report <- getExpenseReport
    toFile def "expense-pie.svg" $ expensePieGraph report
