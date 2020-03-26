{-# LANGUAGE FlexibleContexts #-}
module Lib where

import           Control.Monad.State            ( MonadState )
import           Data.Default                   ( def )
import           Graphics.Rendering.Chart.Easy  ( (.~)
                                                , (.=)
                                                , (&)
                                                , PieLayout
                                                , PieItem
                                                , pie_title
                                                , pie_plot
                                                , pie_data
                                                , pitem_value
                                                , pitem_label
                                                , Layout
                                                , EC
                                                , PlotIndex
                                                , layout_title
                                                , layout_x_axis
                                                , laxis_generate
                                                , autoIndexAxis
                                                , plot
                                                , plotBars
                                                , bars
                                                , addIndexes
                                                )
import           Hledger.Data.Amount            ( amounts
                                                , showMixedAmount
                                                )
import           Hledger.Data.Dates             ( showDateSpan )
import           Hledger.Data.Journal           ( journalExpenseAccountQuery
                                                , journalRevenueAccountQuery
                                                )
import           Hledger.Data.Types             ( Journal
                                                , Amount(aquantity)
                                                , Interval(Months)
                                                , DateSpan
                                                , MixedAmount
                                                )
import           Hledger.Query                  ( Query )
import           Hledger.Read                   ( defaultJournal )
import           Hledger.Reports.ReportOptions  ( ReportOpts(depth_, interval_)
                                                )
import           Hledger.Reports.BalanceReport  ( BalanceReportItem
                                                , balanceReport
                                                )
import           Hledger.Reports.MultiBalanceReport
                                                ( MultiBalanceReport(..)
                                                , multiBalanceReport
                                                )

import qualified Data.Text                     as T


-- JOURNAL

getJournal :: IO Journal
getJournal = defaultJournal

getExpenseReport :: Journal -> [BalanceReportItem]
getExpenseReport = getBalanceReport journalExpenseAccountQuery

getIncomeReport :: Journal -> [BalanceReportItem]
getIncomeReport = getBalanceReport journalRevenueAccountQuery


getBalanceReport :: (Journal -> Query) -> Journal -> [BalanceReportItem]
getBalanceReport query j =
    fst $ balanceReport (def { depth_ = Just 1 }) (query j) j

getMonthlyBalanceReport :: (Journal -> Query) -> Journal -> MultiBalanceReport
getMonthlyBalanceReport query j = multiBalanceReport
    (def { interval_ = Months 1, depth_ = Just 0 })
    (query j)
    j

getMonthlyBalances :: (Journal -> Query) -> Journal -> [(DateSpan, MixedAmount)]
getMonthlyBalances query j =
    let (MultiBalanceReport (dates, _, totals)) =
                getMonthlyBalanceReport query j
    in  zip dates $ (\(amts, _, _) -> amts) totals



-- GRAPHS

expensePieGraph :: MonadState PieLayout m => [BalanceReportItem] -> m ()
expensePieGraph = balanceReportPieGraph "Expenses" 1


incomePieGraph :: MonadState PieLayout m => [BalanceReportItem] -> m ()
incomePieGraph = balanceReportPieGraph "Income" 1


balanceReportPieGraph
    :: MonadState PieLayout m => String -> Int -> [BalanceReportItem] -> m ()
balanceReportPieGraph title depth items = do
    pie_title .= title
    pie_plot . pie_data .= map toPieItem
                               (filter (\(_, _, d, _) -> d == depth) items)
  where
    toPieItem :: BalanceReportItem -> PieItem
    toPieItem (_, name, _, amts) =
        def
            & (pitem_value .~ mixedToFrac amts)
            & (pitem_label .~ (T.unpack name ++ " " ++ showMixedAmount amts))


incomeExpensesBars
    :: [(DateSpan, MixedAmount)]
    -> [(DateSpan, MixedAmount)]
    -> EC (Layout PlotIndex Double) ()
incomeExpensesBars income expenses = do
    layout_title .= "Income & Expenses"
    layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
    plot $ plotBars <$> bars titles (addIndexes $ map snd values)
  where
    titles = ["Income", "Expenses"]
    values = zipWith
        (\(d, i) (_, e) -> (showDateSpan d, [mixedToFrac i, mixedToFrac e]))
        income
        expenses

mixedToFrac :: Fractional a => MixedAmount -> a
mixedToFrac = abs . realToFrac . aquantity . head . amounts
