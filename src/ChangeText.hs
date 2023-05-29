{-# LANGUAGE RecordWildCards #-}

-- Модуль с изменением текстов в соотствии с тем что нажали и когда

module ChangeText (addText) where

import Data.Char
import Types (
    Date(..),
    Tags(..),
    Task(..),
    BlockTask(..),
    ButtonDown(..),
    BlockDate(..),
    DisplayTask(..),
    KanbanWorld(..))


---------------------------- Добавление текстов

addTextInTask :: Maybe Char -> BlockTask -> Task -> Task
addTextInTask Nothing TaskTitle task@Task{title = []} = task
addTextInTask Nothing TaskDescription task@Task{description = []} = task
addTextInTask Nothing TaskTitle task@Task{..} = task{title = take (length title - 1) title}
addTextInTask Nothing TaskDescription task@Task{..} =
    task{description = take (length description - 1) description}
addTextInTask (Just ch) TaskTitle task@Task{..} = task{title = title ++ [ch]}
addTextInTask (Just ch) TaskDescription task@Task{..} = task{description = description ++ [ch]}
addTextInTask _ _ task = task

addNumber :: Int -> Char -> Int -> Int
addNumber num ch base =
    if ((length $ show num) < base) && (ch >= '0') && (ch <= '9')
    then num * 10 + (ord ch) - (ord '0')
    else num 

addDateInTask :: Maybe Char -> BlockTask -> BlockDate -> Task -> Task
addDateInTask Nothing TaskDate DateDay task@Task{date = Date (d, m, y)} =
    task{date = Date (div d 10, m, y)}
addDateInTask Nothing TaskDate DateMonth task@Task{date = Date (d, m, y)} =
    task{date = Date (d, div m 10, y)}
addDateInTask Nothing TaskDate DateYear task@Task{date = Date (d, m, y)} =
    task{date = Date (d, m, div y 10)}
addDateInTask (Just ch) TaskDate DateDay task@Task{date = Date (d, m, y)} =
    task{date = Date (addNumber d ch 2, m, y)}
addDateInTask (Just ch) TaskDate DateMonth task@Task{date = Date (d, m, y)} =
    task{date = Date (d, addNumber m ch 2, y)}
addDateInTask (Just ch) TaskDate DateYear task@Task{date = Date (d, m, y)} =
    task{date = Date (d, m, addNumber y ch 4)}
addDateInTask _ _ _ task = task

numInTags :: Int -> [String] -> Maybe String
numInTags num lst | num < 0 = Nothing
                  | num >= length lst = Nothing
                  | otherwise = Just (lst !! num)

addTagInTask :: Maybe Char -> BlockTask -> Int -> Task -> Task
addTagInTask Nothing TaskTags num task@Task{tags = Tags lst} = case numInTags num lst of
    Nothing -> task
    Just str -> if length str == 0
                then task
                else if length str == 1
                     then task{tags = Tags ((take num lst) ++ (drop (num + 1) lst))}
                     else task{tags = Tags ((take num lst) ++ [init str] ++ (drop (num + 1) lst))}
addTagInTask (Just ch) TaskTags num task@Task{tags = Tags lst} = 
    if num == length lst
    then task{tags = Tags $ lst ++ [[ch]]}
    else case numInTags num lst of
        Nothing -> task
        Just str -> task{tags = Tags ((take num lst) ++ [str ++ [ch]] ++ (drop (num + 1) lst))}
addTagInTask _ _ _ task = task

addTextColumn :: Maybe Char -> String -> String
addTextColumn Nothing "" = ""
addTextColumn Nothing name = take (length name - 1) name
addTextColumn (Just ch) name = name ++ [ch]



addText :: Maybe Char -> KanbanWorld -> KanbanWorld
addText key world@KanbanWorld{curDisplayTask =
    curDisplayTask@DisplayTask{curDownButton = ButtonTask, ..}} =
        world{curDisplayTask =
              curDisplayTask{curTask = addTextInTask key curDownBlockTask
                                    $ addDateInTask key curDownBlockTask curDownDate
                                    $ addTagInTask key curDownBlockTask (curDownTag - 1) curTask}}
addText key world@KanbanWorld{curDisplayTask =
    curDisplayTask@DisplayTask{curDownButton = ButtonColumn, ..}, ..} =
        if curDownColumn == length board
        then world{curDisplayTask = curDisplayTask{newColumn = addTextColumn key (newColumn)}}
        else world
addText _ world = world