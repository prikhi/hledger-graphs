{-# LANGUAGE FlexibleContexts #-}
module Lib where

import           Control.Monad.State            ( MonadState )
import           Data.Default                   ( def )
import           Graphics.Rendering.Chart.Easy  ( (.~)
                                                , (.=)
                                                , PieLayout
                                                , pie_title
                                                , pie_plot
                                                , pie_data
                                                , pitem_value
                                                , pitem_label
                                                , pitem_offset
                                                )
import           Hledger.Data.Amount            ( amounts )
import           Hledger.Data.Journal           ( journalExpenseAccountQuery )
import           Hledger.Data.Types             ( Amount(aquantity) )
import           Hledger.Read                   ( defaultJournal )
import           Hledger.Reports.ReportOptions  ( ReportOpts(depth_) )
import           Hledger.Reports.BalanceReport  ( BalanceReportItem
                                                , balanceReport
                                                )

import qualified Data.Text                     as T


getExpenseReport :: IO [BalanceReportItem]
getExpenseReport = do
    journal <- defaultJournal
    return . fst $ balanceReport (def { depth_ = Just 2 })
                                 (journalExpenseAccountQuery journal)
                                 journal


expensePieGraph :: MonadState PieLayout m => [BalanceReportItem] -> m ()
expensePieGraph items = do
    pie_title .= "Expenses"
    pie_plot . pie_data .= map pieItem (filter (\(_, _, d, _) -> d == 1) items)
  where
    pieItem (_, name, _, amts) =
        (pitem_value .~ realToFrac (aquantity $ head $ amounts amts))
            $ (pitem_label .~ (T.unpack name))
            $ (pitem_offset .~ 0)
            $ def
