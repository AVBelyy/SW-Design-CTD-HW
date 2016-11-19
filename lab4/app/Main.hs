{-# LANGUAGE OverloadedStrings #-}

module Main where

import Yesod
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

import Model
import Controller

main :: IO ()
main = runStderrLoggingT $ withSqlitePool ":memory:" 10 $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
        -- insert dummy data here...
        return ()
    warp 3000 (TodoApp pool)
