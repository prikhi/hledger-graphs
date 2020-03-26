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
                                                )
import           Hledger.Data.Amount            ( amounts
                                                , showMixedAmount
                                                )
import           Hledger.Data.Journal           ( journalExpenseAccountQuery
                                                , journalRevenueAccountQuery
                                                )
import           Hledger.Data.Types             ( Journal
                                                , Amount(aquantity)
                                                )
import           Hledger.Query                  ( Query )
import           Hledger.Read                   ( defaultJournal )
import           Hledger.Reports.ReportOptions  ( ReportOpts(depth_) )
import           Hledger.Reports.BalanceReport  ( BalanceReportItem
                                                , balanceReport
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
            & (pitem_value .~ realToFrac (aquantity $ head $ amounts amts))
            & (pitem_label .~ (T.unpack name ++ " " ++ showMixedAmount amts))
