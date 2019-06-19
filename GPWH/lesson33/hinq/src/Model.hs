module Model where

data GradeLevel
  = Freshman
  | Sophmore
  | Junior
  | Senior
  deriving (Eq, Ord, Enum, Show)

data Name =
  Name
    { firstName :: String
    , lastName  :: String
    }

instance Eq Name where
  a == b = firstName a == firstName b && lastName a == lastName b

instance Show Name where
  show (Name first last) = mconcat [first, " ", last]

data Student =
  Student
    { studentId   :: Int
    , gradeLevel  :: GradeLevel
    , studentName :: Name
    }
  deriving (Show)

instance Eq Student where
  a == b = studentId a == studentId b && studentName a == studentName b

data Teacher =
  Teacher
    { teacherId   :: Int
    , teacherName :: Name
    }
  deriving (Show)

instance Eq Teacher where
  a == b = teacherId a == teacherId b && teacherName a == teacherName b

data Course =
  Course
    { courseId    :: Int
    , courseTitle :: String
    , teacher     :: Int
    }
  deriving (Show)

instance Eq Course where
  a == b = courseId a == courseId b && courseTitle a == courseTitle b


data HINQ m a b
  = HINQ (m a -> m b) (m a) (m a -> m a)
  | HINQ_ (m a -> m b) (m a)

data Enrollment =
  Enrollment
    { student :: Int
    , course  :: Int
    }
  deriving (Show)


--------------------------------------------------------------------------------------------------------------
-- mock data
--------------------------------------------------------------------------------------------------------------
students :: [Student]
students =
  [ Student 1 Senior (Name "Audre" "Lorde")
  , Student 2 Junior (Name "Leslie" "Silko")
  , Student 3 Freshman (Name "Judith" "Butler")
  , Student 4 Senior (Name "Guy" "Debord")
  , Student 5 Sophmore (Name "Jean" "Baudrillard")
  , Student 6 Junior (Name "Julia" "Kristeva")
  ]

teachers :: [Teacher]
teachers =
  [ Teacher 100 (Name "Simone" "De Beauvior")
  , Teacher 200 (Name "Susan" "Sonntag")
  , Teacher 300 (Name "Joe" "Nemeth")
  , Teacher 400 (Name "John" "Doe")
  , Teacher 500 (Name "Alice" "Goodman")
  ]

courses :: [Course]
courses =
  [ Course 101 "French" 100
  , Course 201 "English" 200
  , Course 301 "Russian" 300
  , Course 401 "Physics" 400
  , Course 501 "Chemistry" 500
  , Course 601 "Chemistry" 300
  , Course 701 "Biology" 400
  , Course 801 "Mathematics" 500
  ]

enrollments :: [Enrollment]
enrollments = [ Enrollment 1 101
              , Enrollment 2 101
              , Enrollment 2 201
              , Enrollment 3 101
              , Enrollment 4 201
              , Enrollment 4 101
              , Enrollment 5 101
              , Enrollment 6 201
              ]
