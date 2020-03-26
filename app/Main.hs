module Main where

import           Diagrams.Prelude
import           Diagrams.Backend.SVG
import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Backend.Types
import           Graphics.Rendering.Chart.Renderable
import           Graphics.Rendering.Chart.State
import           Hledger.Data.Journal           ( journalExpenseAccountQuery
                                                , journalRevenueAccountQuery
                                                )

import           Lib

main :: IO ()
main = do
    journal <- getJournal
    dEnv    <- defaultEnv vectorAlignmentFns 500 500
    dEnv_   <- defaultEnv vectorAlignmentFns 1000 500
    let makeDiagram  = fst . runBackendR dEnv . toRenderable . execEC
        makeDiagram_ = fst . runBackendR dEnv_ . toRenderable . execEC
        expensesPie  = makeDiagram $ expensePieGraph $ getExpenseReport journal
        incomePie    = makeDiagram $ incomePieGraph $ getIncomeReport journal
        bars         = makeDiagram_ $ incomeExpensesBars
            (getMonthlyBalances journalRevenueAccountQuery journal)
            (getMonthlyBalances journalExpenseAccountQuery journal)
    renderSVG "income-expense-pie.svg" (mkSizeSpec $ V2 (Just 1010) Nothing)
        $ vcat
              [ centerX $ hcat [expensesPie, strutX 10, incomePie]
              , strutY 10
              , centerX $ bars
              ]
