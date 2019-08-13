--import Prelude     (IO)
--import Application (appMain)
--main :: IO ()
--main = appMain
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Control.Monad (forM)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics
import Yesod

-- Foundation Type
data Despesas =
  Despesas ConnectionPool

instance Yesod Despesas where
  shouldLogIO (Despesas _pool) _src _level = return True
  makeSessionBackend _ = return Nothing

instance YesodPersist Despesas where
  type YesodPersistBackend Despesas = SqlBackend
  runDB action = do
    Despesas pool <- getYesod
    runSqlPool action pool

-- Models
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
DBUser sql=user
  name Text
  deriving Show

DBExpense sql=expense
  amount Int
  payed_by DBUserId
  deriving Show

DBUserExpense sql=user_expense
  userId DBUserId
  expenseId DBExpenseId
  UniqueUserExpense userId expenseId
  deriving Show
|]

data User = User
  { id :: Int
  , name :: Text
  } deriving (Show, Generic)

instance ToJSON User

data Expense = Expense
  { id :: Int
  , amount :: Int
  , payed_by :: User
  } deriving (Show, Generic)

instance ToJSON Expense

-- Routes
mkYesod
  "Despesas"
  [parseRoutes|
/user UserListR GET
/user/#DBUserId UserR GET
/expense ExpenseListR GET
/expense/#DBExpenseId ExpenseR GET
|]

intFromUserKey :: Key DBUser -> Int
intFromUserKey = fromIntegral . fromSqlKey

intFromExpenseKey :: Key DBExpense -> Int
intFromExpenseKey = fromIntegral . fromSqlKey

-- Handlers
getUserListR :: Handler Value
getUserListR = do
  raw_users <- runDB $ selectList [] [Asc DBUserName]
  users <-
    forM raw_users $ \(Entity userId user) -> do
      return User {id = intFromUserKey userId, name = dBUserName user}
  returnJson users

getUserR :: DBUserId -> Handler Value
getUserR userId = do
  user <- runDB $ get userId
  case user of
    Nothing -> returnJson $ object ["error" .= ("User not found" :: [Char])]
    Just u -> returnJson User {id = intFromUserKey userId, name = dBUserName u}

getExpenseR :: DBExpenseId -> Handler Value
getExpenseR expenseId = do
  raw_expense <-
    runDB $
    rawSql
      "SELECT ??, ?? FROM expense INNER JOIN user \
      \ON expense.payed_by=user.id WHERE expense.id=?"
      [PersistInt64 $ fromSqlKey expenseId]
  expenses <-
    forM raw_expense $ \((Entity _ expense), (Entity userId user)) -> do
      return
        Expense
          { id = intFromExpenseKey expenseId
          , amount = dBExpenseAmount expense
          , payed_by =
              User
                {id = fromIntegral $ fromSqlKey userId, name = dBUserName user}
          }
  case expenses of
    (expense:_) -> returnJson expense
    _ -> returnJson $ object ["error" .= ("Expense not found" :: [Char])]

getExpenseListR :: Handler Value
getExpenseListR = do
  raw_expenses <-
    runDB $
    rawSql
      "SELECT ??, ?? FROM expense INNER JOIN user \
      \ON expense.payed_by=user.id"
      []
  expenses <-
    forM raw_expenses $ \((Entity expenseId expense), (Entity userId user)) -> do
      return
        Expense
          { id = intFromExpenseKey expenseId
          , amount = dBExpenseAmount expense
          , payed_by =
              User
                {id = fromIntegral $ fromSqlKey userId, name = dBUserName user}
          }
  returnJson expenses

-- Server start
openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main =
  runStderrLoggingT $
  withSqlitePool "test.db3" openConnectionCount $ \pool ->
    liftIO $ do
      _ <-
        runResourceT $
        flip runSqlPool pool $ do
          runMigration migrateAll
          pedro <- insert $ DBUser "Pedro"
          maia <- insert $ DBUser "Maia"
          silane <- insert $ DBUser "Silane"
          expense <- insert $ DBExpense 200 pedro
          _ <- insert $ DBUserExpense maia expense
          _ <- insert $ DBUserExpense silane expense
          return ()
      warp 3000 $ Despesas pool
