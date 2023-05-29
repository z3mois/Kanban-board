module Types (
    Date(..),
    TypeTask(..),
    Tags(..),
    Person(..),
    Task(..),
    ColumnName,
    Column(..),
    KanbanBoard,
    Persons,
    BlockTask(..),
    ButtonDown(..),
    BlockDate(..),
    DisplayTask(..),
    KanbanWorld(..)
) where



--------------------------------------------------------
-- Для основы Kanban-доски

data Date = Date (Int, Int, Int)
instance Show Date where
    show (Date (d, m, y)) = "Date: " ++ show d ++ "." ++ show m ++ "." ++ show y

data TypeTask = Bag | Ficha | Question | Testing | Development deriving (Eq)
instance Show TypeTask where
    show Bag = "Type: Bag"
    show Ficha = "Type: Ficha"
    show Question = "Type: Question"
    show Testing = "Type: Testing"
    show Development = "Type: Development"

myShowList :: [String] -> String
myShowList [] = ""
myShowList [x] = x
myShowList (x : xs) = x ++ ", " ++ myShowList xs

data Tags = Tags [String]
instance Show Tags where
    show (Tags lst) = "Tags: " ++ myShowList lst

data Person = Person {
    userId :: Int,
    namePerson :: String,
    rolePerson :: String
}

data Task = Task {
    taskId :: Int,
    title :: String,
    description :: String,
    date :: Date,
    typeTask :: TypeTask,
    tags :: Tags,
    taskUserId :: Int,
    taskUserName :: String
}
instance Eq Task where
    (==) task1 task2 | taskId task1 == taskId task2 = True
                     | otherwise = False

type ColumnName = String

data Column = Column {
    columnName :: ColumnName,
    listTasks :: [Task]
}

type KanbanBoard = [Column]

type Persons = [Person]


--------------------------------------------------------
-- Для проририсовки Kanban-доски

data BlockTask = TaskNothing | TaskTitle | TaskDescription
                | TaskDate | TaskType | TaskTags | TaskUser

data ButtonDown = ButtonNothing | ButtonTask | ButtonColumn

data BlockDate = DateDay | DateMonth | DateYear

data DisplayTask = DisplayTask {
    curTask :: Task,
    curDownBlockTask :: BlockTask,
    curDownColumn :: Int,
    curDownButton :: ButtonDown,
    newColumn :: String,
    curDownDate :: BlockDate,
    curDownTag :: Int
}

data KanbanWorld = KanbanWorld {
    displayBoard :: Bool,
    displayAddTask :: Bool,
    activeModifyTask :: Bool,
    displayModifyTask :: Bool,
    oldModifyTask :: Task,
    activeDeleteTask :: Bool,
    activeMoveTask :: Bool,
    curMoveTask :: Maybe Task,
    curDisplayTask :: DisplayTask,
    board :: KanbanBoard,
    users :: Persons
}