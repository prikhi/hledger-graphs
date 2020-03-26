module Main where

import           Diagrams.Prelude
import           Diagrams.Backend.SVG
import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Backend.Types
import           Graphics.Rendering.Chart.Renderable
import           Graphics.Rendering.Chart.State

import           Lib

main :: IO ()
main = do
    journal <- getJournal
    dEnv    <- defaultEnv vectorAlignmentFns 500 500
    let makeDiagram = fst . runBackendR dEnv . toRenderable . execEC
    let expensesPie = makeDiagram $ expensePieGraph $ getExpenseReport journal
        incomePie   = makeDiagram $ incomePieGraph $ getIncomeReport journal
    renderSVG "income-expense-pie.svg" (mkSizeSpec $ V2 (Just 1010) Nothing)
        $ hcat [expensesPie, strutX 10, incomePie]
