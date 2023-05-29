{-# LANGUAGE RecordWildCards #-}

-- Модуль с отрисовкой всего и вся

module Visual (kanban) where

import Graphics.Gloss.Interface.Pure.Game
import Types (
    Date(..),
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
    infoHeight,
    subInfoHeight,
    taskHeight,
    textHeight,
    textWidth,
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
    dateWidth,
    dateHeight,
    taskCoord,
    titleScale,
    addCoord,
    scaleAddCoord,
    boldFond,
    drawCross,
    drawPencil,
    drawCursor)
import KBoard (
    emptyTask,
    addTask,
    getMaxLenght,
    getNamesColumn,
    getNamePerson)
import ChangeText (addText)
import CursorButton (buttonInBoard, buttonInCurTask)




------------ Отрисовка текста (в определенной рамке)

findOptScale :: Point -> Float -> Float -> Float -> Float
findOptScale (width, height) leng needNumsLines scaleSize = 
    let lengLine = fromIntegral (round (width / textWidth / scaleSize) :: Int)
        numsLines = fromIntegral (round ((leng - 1) / lengLine + 1) :: Int)
        isFit = (lengLine /= 0) && (numsLines * distanceText * scaleSize <= height)
    in if isFit
       then scaleSize
       else findOptScale (width, height) leng (needNumsLines + 1)
                        (height / distanceText / (needNumsLines + 1))

drawLines :: Int -> String -> Picture
drawLines _ [] = Blank
drawLines leng str = pictures [text $ take leng str,
                               translate 0 (-distanceText) $ drawLines leng (drop leng str)]

drawText :: Point -> Float -> String -> Picture
drawText (tWidth, height) startScale str = 
    let width = tWidth - 2 * boardPadding
        scaleSize = findOptScale (width, height) (fromIntegral $ length str) 1
                                (min startScale (height / distanceText))
        lengLine = round (width / textWidth / scaleSize)
    in translate (-width / 2) (-textHeight * scaleSize + height / 2)
            $ scale scaleSize scaleSize $ drawLines lengLine str

---------------------------------------------------------------------------



drawButton :: Float -> Float -> Float -> String -> Picture
drawButton xSize ySize scaleSize str =
    pictures [rectangleWire xSize ySize,
              boldFond $ translate (-xSize / 2) 0
                       $ scale scaleSize scaleSize
                       $ translate boardPadding (-distanceText / 2 + textPadding) 
                       $ text str]



-------------- Отрисовка доски

drawTask :: Point -> Float -> Task -> Picture
drawTask (x, y) scaleSize Task{..} = 
    let titleText = translate 0 (-titleHeight / 2 + taskHeight / 2)
            $ pictures [rectangleWire taskWidth titleHeight,
                        rectangleWire (taskWidth - 2) (titleHeight - 2),
                        boldFond $ drawText (taskWidth, titleHeight) 1 title]
        descriptionText = translate 0 (-descriptionHeight / 2 - titleHeight + taskHeight / 2)
            $ pictures [rectangleWire taskWidth descriptionHeight,
                        drawText (taskWidth, descriptionHeight) 0.5 description]
        infoText = translate 0 (-infoHeight / 2 - descriptionHeight - titleHeight + taskHeight / 2)
            $ pictures [rectangleWire taskWidth infoHeight,
                        Line [(-taskWidth / 2, infoHeight / 2 - subInfoHeight),
                              (taskWidth / 2, infoHeight / 2 - subInfoHeight)],
                        Line [(-taskWidth / 2, infoHeight / 2 - 2 * subInfoHeight),
                              (taskWidth / 2, infoHeight / 2 - 2 * subInfoHeight)],
                        Line [(-taskWidth / 2, infoHeight / 2 - 3 * subInfoHeight),
                              (taskWidth / 2, infoHeight / 2 - 3 * subInfoHeight)],
                        translate (-taskWidth / 2 + boardPadding) (-textHeight / 2 + infoHeight / 2)
                        $ scale 0.5 0.5
                        $ pictures [text $ show date,
                                    translate 0 (-2*subInfoHeight) $ text $ show typeTask,
                                    translate 0 (-4*subInfoHeight) $ text $ show tags,
                                    translate 0 (-6*subInfoHeight)
                                        $ text ("User: " ++ taskUserName)]]
    in translate x y $ scale scaleSize scaleSize $ pictures [titleText, descriptionText, infoText]

drawAllTasks :: Point -> Float -> [Task] -> Picture
drawAllTasks _ _ [] = Blank
drawAllTasks (x, y) scaleSize (task : xs) =
    pictures [drawTask (x, y) scaleSize task,
              drawAllTasks (x, y - distanceTasks * scaleSize) scaleSize xs]

drawColumn :: Point -> Float -> Column -> Picture
drawColumn (x, _) scaleSize Column{..} = 
    let titleText = translate (x - (taskWidth / 2) * scaleSize)
                              (windowHeight / 2 - columnTitleHeight + boardPadding)
                    $ scale (2 * scaleSize) (2 * scaleSize) $ boldFond $ boldFond $ Text columnName
        yColumn = windowHeight / 2 - columnTitleHeight - (taskSpacing + taskHeight / 2) * scaleSize
    in pictures [titleText,
                 drawAllTasks (x, yColumn) scaleSize listTasks]

drawAllColumn :: Float -> Point -> Float -> KanbanBoard -> Picture
drawAllColumn _ _ _ [] = Blank
drawAllColumn _ (x, y) scaleSize (column : []) = drawColumn (x, y) scaleSize column
drawAllColumn linesDistance (x, y) scaleSize (column : xs) =
    let pathLine = [(x + linesDistance / 2 - 1, -windowHeight / 2 + funcTitleHeight),
                    (x + linesDistance / 2 - 1, windowHeight),
                    (x + linesDistance / 2, -windowHeight / 2 + funcTitleHeight),
                    (x + linesDistance / 2, windowHeight),
                    (x + linesDistance / 2 + 1, -windowHeight / 2 + funcTitleHeight),
                    (x + linesDistance / 2 + 1, windowHeight)]
    in pictures [Line pathLine,
                 drawColumn (x, y) scaleSize column,
                 drawAllColumn linesDistance (x + linesDistance, y) scaleSize xs]

drawFunc :: Picture     -- отрисовка нижней части доски (где Add, Delete, Modify)
drawFunc = 
    let xSize = fst buttonSize
        ySize = snd buttonSize
        scaleSize = ySize / textHeight / 2
        beginX = windowWidth / 2 - 0.5 * xSize - buttonPadding
        beginY = -windowHeight / 2 + buttonPadding + ySize / 2
        distance = xSize + buttonPadding
    in pictures [translate beginX beginY $ drawButton xSize ySize scaleSize "Add",
                 translate (beginX - distance) beginY $ drawButton xSize ySize scaleSize "Delete", 
                 translate (beginX - 2*distance) beginY $ drawButton xSize ySize scaleSize "Modify",
                 translate (beginX - 3*distance) beginY $ drawButton xSize ySize scaleSize "Move"]

drawColumnDMTask :: (Point -> Picture) -> Float -> Point -> [Task] -> Picture
drawColumnDMTask _ _ _ [] = Blank
drawColumnDMTask f yDist (x, y) (_ : xs) =
    pictures [f (x, y), drawColumnDMTask f yDist (x, y - yDist) xs]

drawBoardDMTask :: Bool -> (Point -> Picture) -> Float -> Float -> Point -> KanbanBoard -> Picture
drawBoardDMTask True _ _ _ _ [] = Blank
drawBoardDMTask True f xDist yDist (x, y) (Column{..} : xs) =
    pictures [drawColumnDMTask f yDist (x, y) listTasks,
              drawBoardDMTask True f xDist yDist (x + xDist, y) xs]
drawBoardDMTask False _ _ _ _ _ = Blank

drawBoard :: KanbanWorld -> Picture
drawBoard KanbanWorld{..} =
    let heightSize = distanceTasks * (fromIntegral $ getMaxLenght board) + taskSpacing
        numsColumns = (fromIntegral $ length board)
        widthSize = numsColumns * distanceColumn
        scaleSize = min ((windowHeight - columnTitleHeight - funcTitleHeight) / heightSize)
                        (windowWidth / widthSize)
        linesDistance = windowWidth / numsColumns
        titlePathLine = [(-windowWidth, windowHeight / 2 - columnTitleHeight),
                         (windowWidth, windowHeight / 2 - columnTitleHeight)]
        funcPathLine = [(-windowWidth, -windowHeight / 2 + funcTitleHeight),
                        (windowWidth, -windowHeight / 2 + funcTitleHeight)]
        firstColumnX = -windowWidth / 2 + linesDistance / 2
        ydistDM = (taskHeight + taskSpacing) * scaleSize
        pointDM = (firstColumnX + taskWidth / 2 * scaleSize,
                       windowHeight / 2 - columnTitleHeight - taskSpacing * scaleSize)
    in pictures [Line titlePathLine,
                 drawAllColumn linesDistance (firstColumnX, 0) scaleSize board,
                 Line funcPathLine,
                 drawFunc,
                 drawBoardDMTask activeDeleteTask drawCross linesDistance ydistDM pointDM board,
                 drawBoardDMTask activeModifyTask drawPencil linesDistance ydistDM pointDM board,
                 drawBoardDMTask activeMoveTask drawCursor linesDistance ydistDM pointDM board]

--------------------------------------------------------------











----------- Отрисовка добавления задачи



drawAllNamesColumn :: Float -> Point -> [String] -> Int -> Int -> Picture
drawAllNamesColumn _ _ [] _ _ = Blank
drawAllNamesColumn linesDistance (x, y) (colName : xs) number acc
    | number == acc =pictures [translate x (y - textPadding)
                                    $ Line [(0, 0), (linesDistance - 2 * boardPadding, 0)],
                               translate x y $ boldFond $ boldFond $ scale titleScale titleScale
                                    $ text colName,
                               drawAllNamesColumn linesDistance (x + linesDistance, y) xs
                                    number (acc + 1)]
    | otherwise = pictures [translate x y $ boldFond $ boldFond $ scale titleScale titleScale
                                $ text colName,
                            drawAllNamesColumn linesDistance (x + linesDistance, y) xs
                                number (acc + 1)]

drawNewColumn :: Float -> Point -> [String] -> String -> Int -> Picture
drawNewColumn linesDistance (x, y) allNames name number = 
    let pic = pictures [translate x (y + columnTitleHeight / 2)
                            $ scale (0.5 * titleScale) (0.5 * titleScale) $ text "New Column:",
                        translate x y $ scale titleScale titleScale $ boldFond $ boldFond
                            $ text name]
    in if length allNames == number
        then pictures [pic,
                       translate x (y - textPadding)
                            $ Line [(0, 0), (linesDistance - 2 * boardPadding, 0)]]
        else pic

saveCancelButtons :: Picture
saveCancelButtons =
    let xSize = fst buttonSize * 2
        ySize = snd buttonSize * 2
        scaleSize = ySize / textHeight / 2
        beginX = windowWidth / 2 - 0.5 * xSize - buttonPadding
        beginY = -windowHeight / 2 + buttonPadding + ySize / 2
        distance = xSize + buttonPadding
    in pictures [color green $ translate beginX beginY
                             $ drawButton xSize ySize scaleSize "Save",
                 color red $ translate (beginX - distance) beginY 
                           $ drawButton xSize ySize scaleSize "Cancel"]

drawDate :: BlockTask -> Task -> Picture
drawDate TaskDate Task{date = Date (d, m, y)} =
    translate (fst addCoord + dateWidth) (snd addCoord - scaleAddCoord * distanceText)
    $ scale scaleAddCoord scaleAddCoord
    $ pictures [translate (-1.5 * dateWidth) 0
                    $ pictures [rectangleWire dateWidth dateHeight,
                                translate (-dateWidth / 2 + boardPadding)
                                          (-textHeight + dateHeight / 2)
                                    $ text $ show d],
                translate (-0.5 * dateWidth) 0
                    $ pictures [rectangleWire dateWidth dateHeight,
                                translate (-dateWidth / 2 + boardPadding)
                                          (-textHeight + dateHeight / 2)
                                    $ text $ show m],
                translate dateWidth 0
                    $ pictures [rectangleWire (2 * dateWidth) dateHeight,
                                translate (-dateWidth + boardPadding)
                                          (-textHeight + dateHeight / 2)
                                    $ text $ show y],
                translate (-1.5 * dateWidth) distanceText $ boldFond $ text "Change Date:"]
drawDate _ _ = Blank

drawType :: BlockTask -> Task -> Picture
drawType TaskType Task{..} =
    let base = pictures [boldFond $ text "Change Type:",
                         translate 0 (-distanceText) $ text "Bag",
                         translate 0 (-2 * distanceText) $ text "Ficha",
                         translate 0 (-3 * distanceText) $ text "Question",
                         translate 0 (-4 * distanceText) $ text "Testing",
                         translate 0 (-5 * distanceText) $ text "Development"]
        chooseYcoord =  if typeTask == Bag
                        then (-textPadding / 2 - distanceText)
                        else if typeTask == Ficha
                        then (-textPadding / 2 - 2 * distanceText)
                        else if typeTask == Question
                        then (-textPadding / 2 - 3 * distanceText)
                        else if typeTask == Testing
                        then (-textPadding / 2 - 4 * distanceText)
                        else (-textPadding / 2 - 5 * distanceText)
    in translate (fst addCoord) (snd addCoord) $ scale scaleAddCoord scaleAddCoord
        $ pictures [base,
                    Line [(0, chooseYcoord), (800, chooseYcoord)]]
drawType _ _ = Blank

drawAllTags :: [String] -> Int -> Int -> Picture
drawAllTags [] curTag n =
    let tag = translate 0 (-(fromIntegral n) * distanceText) $ text (show n ++ ". ")
        chooseYcoord = -textPadding / 2 - (fromIntegral n) * distanceText
    in if curTag == n
        then pictures [tag, Line [(0, chooseYcoord), (800, chooseYcoord)]]
        else tag
drawAllTags (x : xs) curTag n =
    let tag = pictures [translate 0 (-(fromIntegral n) * distanceText) $ text (show n ++ ". " ++ x),
                        drawAllTags xs curTag (n + 1)]
        chooseYcoord = -textPadding / 2 - (fromIntegral n) * distanceText
    in if curTag == n
        then pictures [tag, Line [(0, chooseYcoord), (800, chooseYcoord)]]
        else tag

drawTags :: BlockTask -> Int -> Task -> Picture
drawTags TaskTags curTag Task{tags = Tags lst} =
    translate (fst addCoord) (snd addCoord) $ scale scaleAddCoord scaleAddCoord
    $ pictures [boldFond $ text "Change Tags:",
                drawAllTags lst curTag 1]
drawTags _ _ _ = Blank

drawAllUsers :: [Person] -> Int -> Int -> Picture
drawAllUsers [] _ _ = Blank
drawAllUsers (Person{..} : xs) curUserId n =
    let user = pictures [translate 0 (-(fromIntegral n) * distanceText)
                            $ text (show n ++ ". " ++ namePerson ++ " (" ++ rolePerson ++ ")"),
                        drawAllUsers xs curUserId (n + 1)]
        chooseYcoord = -textPadding / 2 - (fromIntegral n) * distanceText
    in if curUserId == userId
        then pictures [user, Line [(0, chooseYcoord), (800, chooseYcoord)]]
        else user

drawUsers :: BlockTask -> Persons -> Task -> Picture
drawUsers TaskUser users Task{..} =
    translate (fst addCoord) (snd addCoord) $ scale scaleAddCoord scaleAddCoord
    $ pictures [boldFond $ text "Change Users:",
                drawAllUsers users taskUserId 1]
drawUsers _ _ _ = Blank


drawCurTask :: KanbanWorld -> Picture
drawCurTask KanbanWorld{curDisplayTask = DisplayTask{..}, ..} =
    let numsColumns = (fromIntegral $ length board) + 1
        linesDistance = windowWidth / numsColumns
        firstColumnX = -windowWidth / 2 + boardPadding
        coloumnY = windowHeight / 2 - columnTitleHeight + boardPadding
        coloumns = drawAllNamesColumn linesDistance (firstColumnX, coloumnY)
                        (getNamesColumn board) curDownColumn 0
        newColumns = drawNewColumn linesDistance ((firstColumnX + windowWidth - linesDistance),
                        coloumnY) (getNamesColumn board) newColumn curDownColumn
        dateBlock = drawDate curDownBlockTask curTask
        typeBlock = drawType curDownBlockTask curTask
        tagsBlock = drawTags curDownBlockTask curDownTag curTask
        userBlock = drawUsers curDownBlockTask users curTask
    in pictures [drawTask taskCoord addTaskScale curTask,
                 coloumns,
                 newColumns,
                 dateBlock,
                 typeBlock,
                 tagsBlock,
                 userBlock,
                 saveCancelButtons]

----------------------------------------------------------------

















--------------------- Прорисовка мира


drawWorld :: KanbanWorld -> Picture
drawWorld world@KanbanWorld{displayBoard = True} = drawBoard world
drawWorld world@KanbanWorld{displayAddTask = True} = drawCurTask world
drawWorld world@KanbanWorld{displayModifyTask = True} = drawCurTask world
drawWorld _ = Blank

updater :: Float -> KanbanWorld -> KanbanWorld
updater _ = id

------------------------------------------------------------------------------


























-------------------- Изменение состояний

handleEvent :: Event -> KanbanWorld -> KanbanWorld
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) world@KanbanWorld{displayBoard=True} =
    buttonInBoard (x, y) world
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) world =
    buttonInCurTask (x, y) world
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) world =
    addText Nothing world
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) world =
    addText (Just ' ') world
handleEvent (EventKey (Char ch) Down _ _) world =
    addText (Just ch) world
handleEvent _ world = world











kanban :: IO ()
kanban = do
    let board0 = [Column {columnName = "do", listTasks = []},
                  Column {columnName = "back", listTasks = []}]
    let my_users = [Person {userId = 0,
                            namePerson = "Mursalimov Danil",
                            rolePerson = "first programmer"},
                    Person {userId = 1,
                            namePerson = "Moiseev Dima",
                            rolePerson = "second programmer"},
                    Person {userId = 2,
                            namePerson = "Yakovlev",
                            rolePerson = "main chiller"}]
    let board1 = addTask Task{taskId = 0,
                              title = "Haskell",
                              description = "Write program Haskell",
                              date = Date (21, 4, 2023),
                              typeTask = Bag,
                              tags = Tags ["#Haskell"],
                              taskUserId = 0,
                              taskUserName = getNamePerson 0 my_users}
                    "do" board0
    let board2 = addTask Task{taskId = 1,
                              title = "opi",
                              description = "Chilling",
                              date = Date (7, 3, 2023),
                              typeTask = Ficha,
                              tags = Tags [],
                              taskUserId = 1,
                              taskUserName = getNamePerson 1 my_users}
                    "back" board1
    let board3 = addTask Task{taskId = 2,
                              title = "formal", description = "Check algorithms",
                              date = Date (1, 1, 2020),
                              typeTask = Question,
                              tags = Tags ["#Haskell", "#Cool", "#yeah"],
                              taskUserId = 2,
                              taskUserName = getNamePerson 2 my_users}
                    "do" board2
    let board4 = addTask Task{taskId = 3,
                              title = "oki",
                              description = "Solve examples",
                              date = Date (1, 4, 2021),
                              typeTask = Testing,
                              tags = Tags ["#Haskell", "#Dan"],
                              taskUserId = 1,
                              taskUserName = getNamePerson 1 my_users}
                    "front" board3
    let board5 = addTask Task{taskId = 4,
                              title = "sa",
                              description = "Another chilling",
                              date = Date (9, 9, 2022),
                              typeTask = Development,
                              tags = Tags ["#Dan"],
                              taskUserId = 0,
                              taskUserName = getNamePerson 0 my_users}
                    "do" board4
    let kanWorld = KanbanWorld {displayBoard = True, displayAddTask = False,
                                activeModifyTask = False, oldModifyTask = emptyTask board5,
                                displayModifyTask = False, activeDeleteTask = False,
                                activeMoveTask = False, curMoveTask = Nothing,
                                curDisplayTask = DisplayTask {curTask = emptyTask board5,
                                curDownBlockTask = TaskNothing, curDownColumn = 0,
                                curDownButton = ButtonNothing, newColumn = "",
                                curDownDate = DateDay, curDownTag = 1},
                                board = board5,
                                users = my_users}
    let initialWindowSize = (round windowWidth, round windowHeight)
    let windowTitle = "Kanban Board"
    let backgroundColor = white

    play (InWindow windowTitle initialWindowSize (0, 0))
         backgroundColor
         60
         kanWorld
         drawWorld 
         handleEvent
         updater