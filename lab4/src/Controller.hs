{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Controller where

import Yesod
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

import Model

-- Shared Controller state
data TodoApp = TodoApp ConnectionPool
instance Yesod TodoApp

mkYesod "TodoApp" [parseRoutes|
/                                   HomeR           GET
/add                                AddTodoR        GET
/del/#TodoListId                    DelTodoR        GET
/todo/#TodoListId                   TodoR           GET
/todo/#TodoListId/add               AddActivityR    GET
/todo/#TodoListId/check/#ActivityId CheckActivityR  GET
|]

instance YesodPersist TodoApp where
    type YesodPersistBackend TodoApp = SqlBackend

    runDB action = do
        TodoApp pool <- getYesod
        runSqlPool action pool

getHomeR :: Handler Html
getHomeR = do
    todos <- runDB $ selectList [] []
    defaultLayout
        [whamlet|
            <ol>
                $forall Entity todoId todo <- todos
                    <li>
                        <a href=@{TodoR todoId}>#{todoListTitle todo}
                        <a href=@{DelTodoR todoId}>[x]
            <form action=@{AddTodoR} method="get">
                <input type="text" name="title">
                <input type="submit" value="Add">
        |]

getAddTodoR :: Handler Html
getAddTodoR = do
    title <- head <$> lookupGetParams "title"
    runDB $ insert $ TodoList title
    redirect HomeR

getDelTodoR :: TodoListId -> Handler Html
getDelTodoR tdlId = do
    runDB $ do
        deleteWhere [ActivityParent ==. tdlId]
        delete tdlId
    redirect HomeR

getTodoR :: TodoListId -> Handler Html
getTodoR tdlId = do
    activities <- runDB $ selectList [ActivityParent ==. tdlId] []
    defaultLayout
        [whamlet|
            <a href=@{HomeR}>←
            <br>
            <ul>
                $forall Entity activityId activity <- activities
                    <li>
                        $if activityChecked activity
                            <s>#{activityName activity}</s>
                        $else
                            #{activityName activity}
                            <a href=@{CheckActivityR tdlId activityId}>[v]
            <form action=@{AddActivityR tdlId} method="get">
                <input type="text" name="name">
                <input type="submit" value="+">
        |]

getAddActivityR :: TodoListId -> Handler Html
getAddActivityR tdlId = do
    name <- head <$> lookupGetParams "name"
    runDB $ insert $ Activity name tdlId False
    redirect (TodoR tdlId)

getCheckActivityR :: TodoListId -> ActivityId -> Handler Html
getCheckActivityR tdlId activityId = do
    runDB $ update activityId [ActivityChecked =. True]
    redirect (TodoR tdlId)
