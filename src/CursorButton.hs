{-# LANGUAGE RecordWildCards #-}

-- Модуль с выяснением куда тыкнули мышкой и отклик на этот тык

module CursorButton (buttonInBoard, buttonInCurTask) where

import Graphics.Gloss.Interface.Pure.Game
import Types (
    TypeTask(..),
    Tags(..),
    Person(..),
    Task(..),
    Column(..),
    KanbanBoard,
    Persons,
    BlockTask(..),
    ButtonDown(..),
    BlockDate(..),
    DisplayTask(..),
    KanbanWorld(..))
import Sizes (
    taskWidth,
    titleHeight,
    descriptionHeight,
    subInfoHeight,
    taskHeight,
    textPadding,
    distanceText,
    boardPadding,
    taskSpacing,
    distanceTasks,
    distanceColumn,
    columnTitleHeight,
    funcTitleHeight,
    windowWidth,
    windowHeight,
    addTaskScale,
    buttonSize,
    buttonPadding,
    crossSize,
    dateWidth,
    dateHeight,
    taskCoord,
    addCoord,
    scaleAddCoord)
import KBoard (
    emptyTask,
    addTask,
    deleteTask,
    moveTask,
    modifyTask,
    getMaxLenght,
    getNamesColumn,
    getIndexColTask,
    getNamePerson)


------------- Проверка куда тыкнули на доске 

funcCoord :: Point -> KanbanWorld -> KanbanWorld
funcCoord (x, y) world@KanbanWorld{..} =
    let halfxSize = (fst buttonSize) / 2
        halfySize = (snd buttonSize) / 2
        beginX = windowWidth / 2 - halfxSize - buttonPadding
        beginY = -windowHeight / 2 + buttonPadding + halfySize
        distance = 2 * halfxSize + buttonPadding
    in if (y >= beginY - halfySize) && (y <= beginY + halfySize)
       then if (x >= beginX - halfxSize) && (x <= beginX + halfxSize)
            then world{displayBoard = False,
                       displayAddTask = True,
                       activeDeleteTask = False,
                       activeModifyTask = False,
                       curDisplayTask = DisplayTask {curTask = emptyTask board,
                                                     curDownBlockTask = TaskNothing,
                                                     curDownColumn = 0,
                                                     curDownButton = ButtonNothing,
                                                     newColumn = "",
                                                     curDownDate = DateDay,
                                                     curDownTag = 1}}
            else if (x >= beginX - halfxSize - distance)
                    && (x <= beginX + halfxSize - distance)
                 then world{activeDeleteTask = not activeDeleteTask,
                            activeModifyTask = False,
                            activeMoveTask = False}
                 else if (x >= beginX - halfxSize - 2 * distance)
                         && (x <= beginX + halfxSize - 2 * distance)
                      then world{activeModifyTask = not activeModifyTask,
                                 activeDeleteTask = False,
                                 activeMoveTask = False}
                      else if (x >= beginX - halfxSize - 3 * distance)
                              && (x <= beginX + halfxSize - 3 * distance)
                           then world{activeMoveTask = not activeMoveTask,
                                      activeDeleteTask = False,
                                      activeModifyTask = False}
                           else world
       else world

isCrossPencilCursor :: Point -> Point -> Bool
isCrossPencilCursor (xButton, yButton) (x, y) =
    (xButton >= x - crossSize)&& (xButton <= x + crossSize)
    && (yButton >= y - crossSize) && (yButton <= y + crossSize)

whereColumnDMTask :: Point -> Float -> Point -> [Task] -> Maybe Task
whereColumnDMTask _ _ _ [] = Nothing
whereColumnDMTask (xButton, yButton) yDist (x, y) (task : xs) =
    case isCrossPencilCursor (xButton, yButton) (x, y) of
    False -> whereColumnDMTask (xButton, yButton) yDist (x, y - yDist) xs
    True -> Just task

whereBoardDMTask :: Point -> Float -> Float -> Point -> KanbanBoard -> Maybe Task
whereBoardDMTask _ _ _ _ [] = Nothing
whereBoardDMTask (xButton, yButton) xDist yDist (x, y) (Column{..} : xs) =
    case whereColumnDMTask (xButton, yButton) yDist (x, y) listTasks of
    Nothing -> whereBoardDMTask (xButton, yButton) xDist yDist (x + xDist, y) xs
    Just task -> Just task

isDMTask :: Point -> KanbanWorld -> KanbanWorld
isDMTask (xButton, yButton) world@KanbanWorld{..} = 
    let heightSize = distanceTasks * (fromIntegral $ getMaxLenght board) + taskSpacing
        numsColumns = (fromIntegral $ length board)
        widthSize = numsColumns * distanceColumn
        scaleSize = min ((windowHeight - columnTitleHeight - funcTitleHeight) / heightSize)
                        (windowWidth / widthSize)
        linesDistance = windowWidth / numsColumns
        firstColumnX = -windowWidth / 2 + linesDistance / 2
        maybeTask = whereBoardDMTask (xButton, yButton) linesDistance ((taskHeight +taskSpacing)
                        * scaleSize) (firstColumnX + taskWidth / 2 * scaleSize, windowHeight / 2
                        - columnTitleHeight - taskSpacing * scaleSize) board
    in if activeDeleteTask 
       then case maybeTask of
                Nothing -> world
                Just task -> world{board = deleteTask task board}
       else if activeModifyTask
            then case maybeTask of
                Nothing -> world
                Just task -> world{displayBoard = False,
                                   displayModifyTask = True,
                                   oldModifyTask = task,
                                   curDisplayTask = DisplayTask {curTask = task,
                                                                 curDownBlockTask = TaskNothing,
                                                                 curDownColumn =
                                                                    getIndexColTask task board,
                                                                 curDownButton = ButtonNothing,
                                                                 newColumn = "",
                                                                 curDownDate = DateDay,
                                                                 curDownTag = 1}}
            else if activeMoveTask
                 then case maybeTask of
                    Nothing -> world
                    Just task -> world{curMoveTask = Just task}
                 else world

moveColumnTask :: Point -> KanbanWorld -> KanbanWorld
moveColumnTask (x, y) world@KanbanWorld{..} =
    let numsColumns = fromIntegral $ length board
        linesDistance = windowWidth / numsColumns
        colName = columnName $ board !! (truncate ((x + windowWidth / 2) / linesDistance))
    in if (y >= windowHeight / 2 - columnTitleHeight) && activeMoveTask
       then case curMoveTask of
                Nothing -> world
                Just task -> world{board = moveTask task colName board}
       else world

buttonInBoard :: Point -> KanbanWorld -> KanbanWorld
buttonInBoard (x, y) world = funcCoord (x, y) $ isDMTask (x, y) $ moveColumnTask (x, y) world


---------------------------------------------------------------














--------------------------- проверка куда тыкнули на задаче

whatBlockTask :: Point -> BlockTask -> BlockTask
whatBlockTask (x, y) curBlock =
    if (x >= -taskWidth / 2 * addTaskScale + fst taskCoord)
       && (x <= taskWidth / 2 * addTaskScale + fst taskCoord)
       && (y >= (taskHeight / 2 - titleHeight) * addTaskScale + snd taskCoord)
       && (y <= taskHeight / 2 * addTaskScale + snd taskCoord)
    then TaskTitle
    else
    if (x >= -taskWidth / 2 * addTaskScale + fst taskCoord)
       && (x <= taskWidth / 2 * addTaskScale + fst taskCoord)
       && (y >= (taskHeight / 2 - titleHeight - descriptionHeight) * addTaskScale + snd taskCoord)
       && (y <= (taskHeight / 2 - titleHeight) * addTaskScale + snd taskCoord)
    then TaskDescription
    else
    if (x >= -taskWidth / 2 * addTaskScale + fst taskCoord)
       && (x <= taskWidth / 2 * addTaskScale + fst taskCoord)
       && (y >= (taskHeight / 2 - titleHeight - descriptionHeight - subInfoHeight) * addTaskScale
                + snd taskCoord)
       && (y <= (taskHeight / 2 - titleHeight - descriptionHeight) * addTaskScale + snd taskCoord)
    then TaskDate
    else
    if (x >= -taskWidth / 2 * addTaskScale + fst taskCoord)
       && (x <= taskWidth / 2 * addTaskScale + fst taskCoord)
       && (y >= (taskHeight / 2 - titleHeight - descriptionHeight - 2*subInfoHeight) * addTaskScale
                + snd taskCoord)
       && (y <= (taskHeight / 2 - titleHeight - descriptionHeight - subInfoHeight) * addTaskScale
                + snd taskCoord)
    then TaskType
    else
    if (x >= -taskWidth / 2 * addTaskScale + fst taskCoord)
       && (x <= taskWidth / 2 * addTaskScale + fst taskCoord)
       && (y >= (taskHeight / 2 - titleHeight - descriptionHeight - 3*subInfoHeight) * addTaskScale
                + snd taskCoord)
       && (y <= (taskHeight / 2 - titleHeight - descriptionHeight - 2*subInfoHeight) * addTaskScale
                + snd taskCoord)
    then TaskTags
    else
    if (x >= -taskWidth / 2 * addTaskScale + fst taskCoord)
       && (x <= taskWidth / 2 * addTaskScale + fst taskCoord)
       && (y >= (taskHeight / 2 - titleHeight - descriptionHeight - 4*subInfoHeight) * addTaskScale
                + snd taskCoord)
       && (y <= (taskHeight / 2 - titleHeight - descriptionHeight - 3*subInfoHeight) * addTaskScale
                + snd taskCoord)
    then TaskUser
    else curBlock

whatColumn :: Point -> KanbanBoard -> Int -> Int
whatColumn (x, y) board number = 
    let numsColumns = (fromIntegral $ length board) + 1
        linesDistance = windowWidth / numsColumns
    in if (y >= (windowHeight / 2 - columnTitleHeight))
        then truncate ((x + windowWidth / 2) / linesDistance)
       else number

whereButtonDown :: Point -> ButtonDown -> ButtonDown
whereButtonDown (x, y) curButton =
    if (y >= windowHeight / 2 - columnTitleHeight)
    then ButtonColumn
    else if (x >= -taskWidth / 2 * addTaskScale + fst taskCoord)
            && (x <= taskWidth / 2 * addTaskScale + fst taskCoord)
            && (y >= -taskHeight / 2 * addTaskScale + snd taskCoord)
            && (y <= taskHeight / 2 * addTaskScale + snd taskCoord)
         then ButtonTask
         else curButton

whatDateDown :: Point -> BlockTask -> BlockDate -> BlockDate
whatDateDown (x, y) TaskDate curBlockDate =
    if (y >= snd addCoord - scaleAddCoord * dateHeight / 2 - scaleAddCoord * distanceText)
        && (y <= snd addCoord + scaleAddCoord * dateHeight / 2  - scaleAddCoord * distanceText)
    then
        if (x >= fst addCoord - 2 * scaleAddCoord * dateWidth + dateWidth)
            && (x <= fst addCoord - scaleAddCoord * dateWidth + dateWidth)
        then DateDay
        else
        if (x >= fst addCoord - scaleAddCoord * dateWidth + dateWidth)
            && (x <= fst addCoord + dateWidth)
        then DateMonth
        else
        if (x >= fst addCoord + dateWidth)
            && (x <= fst addCoord + 0.5 * dateWidth + dateWidth)
        then DateYear
        else curBlockDate
    else curBlockDate
whatDateDown _ _ _ = DateDay

isWhatLine :: Float -> Float -> Bool
isWhatLine y n = y >= (-textPadding / 2 - (n + 1) * distanceText) * scaleAddCoord + snd addCoord
                 && y <= (-textPadding / 2 - n * distanceText) * scaleAddCoord + snd addCoord

whatTypeDown :: Point -> BlockTask -> Task -> Task
whatTypeDown (x, y) TaskType task =
    if (x >= fst addCoord) && (x <= fst addCoord + 200)
    then if isWhatLine y 0
         then task{typeTask = Bag}
         else if isWhatLine y 1
         then task{typeTask = Ficha}
         else if isWhatLine y 2
         then task{typeTask = Question}
         else if isWhatLine y 3
         then task{typeTask = Testing}
         else if isWhatLine y 4
         then task{typeTask = Development}
         else task
    else task
whatTypeDown _ _ task = task

whatTagDown :: Point -> BlockTask -> Task -> Int -> Int
whatTagDown (x, y) TaskTags Task{tags = Tags lst} curTag =
    let coordY = ((y - (snd addCoord)) / scaleAddCoord + textPadding / 2) / distanceText
        numTag = -1 * (floor coordY :: Int)
    in if (x >= fst addCoord) && (x <= fst addCoord + 200)
            && (numTag <= length lst + 1) && (numTag > 0)
        then numTag
        else curTag
whatTagDown _ _ _ curTag = curTag

whatUserDown :: Point -> BlockTask -> Persons -> Task -> Task
whatUserDown (x, y) TaskUser users task =
    let coordY = ((y - (snd addCoord)) / scaleAddCoord + textPadding / 2) / distanceText
        numUser = -1 * (floor coordY :: Int) - 1
    in if (x >= fst addCoord) && (x <= fst addCoord + 200)
            && (numUser < length users) && (numUser >= 0)
        then task{taskUserId = userId $ users !! numUser,
                  taskUserName = getNamePerson (userId $ users !! numUser) users}
        else task
whatUserDown _ _ _ task = task

isCancelButton :: Point -> Bool
isCancelButton (x, y) = 
    let halfxSize = fst buttonSize
        halfySize = snd buttonSize
        xCenter = windowWidth / 2 - 3 * halfxSize - 2 * boardPadding
        yCenter = -windowHeight / 2 + boardPadding + halfySize
    in (x <= xCenter + halfxSize) && (x >= xCenter - halfxSize)
       && (y <= yCenter + halfySize) && (y >= yCenter - halfySize)

isSaveButton :: Point -> Bool
isSaveButton (x, y) = 
    let halfxSize = fst buttonSize
        halfySize = snd buttonSize
        xCenter = windowWidth / 2 - halfxSize - boardPadding
        yCenter = -windowHeight / 2 + boardPadding + halfySize
    in (x <= xCenter + halfxSize) && (x >= xCenter - halfxSize)
       && (y <= yCenter + halfySize) && (y >= yCenter - halfySize)

buttonInCurTask :: Point -> KanbanWorld -> KanbanWorld
buttonInCurTask (x, y) world@KanbanWorld{curDisplayTask = curDisplayTask@DisplayTask{..}, ..} = 
    if isCancelButton (x, y)
    then world{displayBoard = True, displayAddTask = False}
    else
    if isSaveButton (x, y) && displayAddTask
    then world{displayBoard = True, displayAddTask = False, board = addTask curTask
               ((getNamesColumn board ++ [newColumn]) !! curDownColumn) board}
    else
    if isSaveButton (x, y) && displayModifyTask
    then world{displayBoard = True, displayModifyTask = False,
               board = modifyTask curTask oldModifyTask board}
    else let block = whatBlockTask (x, y) curDownBlockTask
             number = if displayAddTask
                      then whatColumn (x, y) board curDownColumn
                      else curDownColumn
             button = whereButtonDown (x, y) curDownButton
             dateDown = whatDateDown (x, y) block curDownDate
             newTask = whatUserDown (x, y) block users
                    $ whatTypeDown (x, y) block curTask
             tagDown = whatTagDown (x, y) block curTask curDownTag
         in world{curDisplayTask = curDisplayTask{curDownBlockTask = block,
                                                  curDownColumn = number,
                                                  curDownButton = button,
                                                  curDownDate = dateDown,
                                                  curTask = newTask,
                                                  curDownTag = tagDown}}

-----------------------------------------------------------------------------------