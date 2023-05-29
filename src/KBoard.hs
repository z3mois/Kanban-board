{-# LANGUAGE RecordWildCards #-}

-- Модель бекэнд

module KBoard (
    emptyTask,
    addTask,
    deleteTask,
    moveTask,
    modifyTask,
    getMaxLenght,
    getNamesColumn,
    getIndexColTask,
    getNamePerson
) where

import Data.List (delete, elemIndex)
import Types (
    Date(..),
    TypeTask(..),
    Tags(..),
    Person(..),
    Task(..),
    ColumnName,
    Column(..),
    KanbanBoard,
    Persons)


getNamePerson :: Int -> Persons -> String
getNamePerson _ [] = ""
getNamePerson curUserId (Person{..} : xs) | curUserId == userId =
                                                namePerson ++ " (" ++ rolePerson ++ ")"
                                          | otherwise = getNamePerson curUserId xs

isTaskIdInColumn :: [Task] -> Int -> Bool
isTaskIdInColumn [] _ = False
isTaskIdInColumn (Task{..} : xs) curTaskId
    | taskId == curTaskId = True
    | otherwise = isTaskIdInColumn xs curTaskId

isTaskIdInBoard :: KanbanBoard -> Int -> Bool
isTaskIdInBoard [] _ = False
isTaskIdInBoard (Column{..} : xs) curTaskId
    | isTaskIdInColumn listTasks curTaskId = True
    | otherwise = isTaskIdInBoard xs curTaskId

findFreeTaskId :: KanbanBoard -> Int -> Int
findFreeTaskId board curTaskId
    | isTaskIdInBoard board curTaskId = findFreeTaskId board (curTaskId + 1)
    | otherwise = curTaskId

emptyTask :: KanbanBoard -> Task
emptyTask board = Task{taskId = findFreeTaskId board 0,
                       title = "",
                       description = "",
                       date = Date (1, 1, 2023),
                       typeTask = Bag,
                       tags = Tags [],
                       taskUserId = 0,
                       taskUserName = ""}

addTask :: Task -> ColumnName -> KanbanBoard -> KanbanBoard
addTask task colName [] = [Column{columnName = colName, listTasks = [task]}]
addTask task colName (col@Column{..} : xs)
    | colName == columnName = (Column{columnName = columnName,
                                      listTasks = listTasks ++ [task]} : xs)
    | otherwise = (col : addTask task colName xs)

deleteTask :: Task -> KanbanBoard -> KanbanBoard
deleteTask _ [] = []
deleteTask task (Column{..} : xs) =
    (Column{columnName = columnName, listTasks = delete task listTasks} : deleteTask task xs)

moveTask :: Task -> ColumnName -> KanbanBoard -> KanbanBoard
moveTask task colName board = addTask task colName $ deleteTask task board

modifyTask :: Task -> Task -> KanbanBoard -> KanbanBoard
modifyTask _ _ [] = []
modifyTask newTask oldTask (col@Column{..} : xs) = case elemIndex oldTask listTasks of
    Nothing -> (col : modifyTask newTask oldTask xs)
    Just index -> (Column{columnName = columnName,
                          listTasks = (take index listTasks) ++ [newTask]
                                ++ (drop (index + 1) listTasks)} : xs)

getMaxLenght :: KanbanBoard -> Int
getMaxLenght [] = 0
getMaxLenght (Column{..} : xs) = max (length listTasks) (getMaxLenght xs)

getNamesColumn :: KanbanBoard -> [String]
getNamesColumn [] = []
getNamesColumn (Column{..} : xs) = (columnName : getNamesColumn xs)

getIndexColTaskAcc :: Task -> KanbanBoard -> Int -> Int
getIndexColTaskAcc _ [] _ = 0
getIndexColTaskAcc task (Column{..} : xs) acc = case elem task listTasks of
    True -> acc
    False -> getIndexColTaskAcc task xs (acc + 1)

getIndexColTask :: Task -> KanbanBoard -> Int
getIndexColTask _ [] = 0
getIndexColTask task x = getIndexColTaskAcc task x 0
