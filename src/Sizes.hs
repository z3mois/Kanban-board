-- Модуль с записанными размерами и отрисовкой кнопок

module Sizes (
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
    columnSpace,
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
    titleScale,
    addCoord,
    scaleAddCoord,
    boldFond,
    drawCross,
    drawPencil,
    drawCursor
) where

import Graphics.Gloss.Interface.Pure.Game

taskWidth :: Float          -- ширина плашки задачи
taskWidth = 1600

titleHeight :: Float        -- высота отдела заголовка задачи
titleHeight = 200

descriptionHeight :: Float  -- высота отдела описания задачи
descriptionHeight = 200

infoHeight :: Float         -- высота отдела с дополнительной информацией задачи
infoHeight = 400

subInfoHeight :: Float      -- высота каждой части информации в задаче
subInfoHeight = infoHeight / 4

taskHeight :: Float         -- высота всей плашки задачи
taskHeight = titleHeight + descriptionHeight + infoHeight

textHeight :: Float         -- дефолтная высота символа при отрисовке с учетом добавления сверху
textHeight = 130

textWidth :: Float          -- дефолтная ширина символа при отрисовке
textWidth = 130

textPadding :: Float        -- дефолтная добавка пространства снизу буквы
textPadding = 30

distanceText :: Float       -- расстояние между символами по высоте с учетом добавок
distanceText = textHeight + textPadding

boardPadding :: Float       -- добваление пространства по краям (слева и справа) текста
boardPadding = 30

taskSpacing :: Float        -- добавочное пространство между задачами
taskSpacing = 40

distanceTasks :: Float      -- расстояние по высоте между задачами
distanceTasks = taskHeight + taskSpacing

columnSpace :: Float        -- добавочное пространство между колонками
columnSpace = 100

distanceColumn :: Float     -- расстояние между колнками
distanceColumn = taskWidth + 2 * columnSpace

columnTitleHeight :: Float  -- высота заголовка колонки
columnTitleHeight = distanceText

funcTitleHeight :: Float    -- высота нижней полосы с функциями
funcTitleHeight = 0.5 * distanceText

windowWidth :: Float        -- ширина окна
windowWidth = 1800

windowHeight :: Float       -- высота окна
windowHeight = 900

addTaskScale :: Float       -- уменьшение размера задачи в окне с добавлением задачи
addTaskScale = 0.5

buttonSize :: Point         -- Размеры кнопок снизу
buttonSize = (100, 50)

buttonPadding :: Float      -- отступ кнопок от краев
buttonPadding = 10

crossSize :: Float          -- размер крестика для удаления
crossSize = 10

pencilHeigth :: Float
pencilHeigth = 10

pencilWidth :: Float
pencilWidth = 15

pencilAddPad :: Float
pencilAddPad = 5

cursorTriangleHeight :: Float
cursorTriangleHeight = 15

cursorTriangleWidth :: Float
cursorTriangleWidth = 7

cursorSquareHeight :: Float
cursorSquareHeight = 7

cursorSquareWidth :: Float
cursorSquareWidth = 2

cursorOffseTriangle :: Float
cursorOffseTriangle = 3

dateWidth :: Float          -- ширина поля с датой в добавлении задачи
dateWidth = 2 * textWidth

dateHeight :: Float         -- высота поля с датой в добавлении задачи
dateHeight = distanceText

taskCoord :: Point          -- координаты, где находится задача
taskCoord = (-300, 0)

titleScale :: Float         -- размеры названия колонок
titleScale = 0.5

addCoord :: Point           -- координаты изменения инофрмации о задаче
addCoord = (200, 150)

scaleAddCoord :: Float      -- размеры измененния информации о задаче
scaleAddCoord = 0.25







boldFond :: Picture -> Picture  -- жирный шрифт
boldFond boldText = pictures [translate 0 0 boldText,
                              translate 1 0 boldText,
                              translate 0 1 boldText,
                              translate 1 1 boldText]

drawCross :: Point -> Picture
drawCross (x, y) = boldFond $ color red $ translate x y
    $ pictures [Line [(-crossSize, -crossSize), (crossSize, crossSize)],
                Line [(-crossSize, crossSize), (crossSize, -crossSize)]]

drawPencil :: Point -> Picture
drawPencil (x, y) = boldFond $ color blue
    $ translate (x - pencilAddPad / 2) (y - pencilAddPad / 2) $ rotate 315
    $ pictures [Line [(-pencilWidth / 2, pencilHeigth / 6), (pencilWidth / 2, pencilHeigth / 6)],
                Line [(-pencilWidth / 2, -pencilHeigth / 6), (pencilWidth / 2, -pencilHeigth / 6)],
                Line [(-pencilWidth / 2, pencilHeigth / 2), (-pencilWidth / 2 - pencilAddPad, 0)],
                Line [(-pencilWidth / 2, -pencilHeigth / 2), (-pencilWidth / 2 - pencilAddPad, 0)],
                rectangleWire pencilWidth pencilHeigth,
                translate (pencilWidth / 2 + pencilAddPad / 2) 0
                    $ rectangleSolid pencilAddPad pencilHeigth]

drawCursor :: Point -> Picture
drawCursor (x, y) = boldFond $ translate x (y - cursorOffseTriangle) $ rotate 315
    $ Line [(0, cursorTriangleHeight),
            (cursorTriangleWidth, -cursorOffseTriangle),
            (cursorSquareWidth, 0),
            (cursorSquareWidth, -cursorSquareHeight),
            (-cursorSquareWidth, -cursorSquareHeight),
            (-cursorSquareWidth, 0),
            (-cursorTriangleWidth, -cursorOffseTriangle),
            (0, cursorTriangleHeight)]