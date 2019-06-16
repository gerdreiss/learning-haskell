module Candidates where

import qualified Data.Map as Map

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)

data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
  { candidateId :: Int
  , codeReview  :: Grade
  , cultureFit  :: Grade
  , education   :: Degree
  } deriving Show

candidate1 :: Candidate
candidate1 = Candidate
  { candidateId = 1
  , codeReview = A
  , cultureFit = A
  , education = BA
  }

candidate2 :: Candidate
candidate2 = Candidate
  { candidateId = 2
  , codeReview = C
  , cultureFit = A
  , education = PhD
  }

candidate3 :: Candidate
candidate3 = Candidate
  { candidateId = 3
  , codeReview = A
  , cultureFit = B
  , education = MS
  }

candidates :: [Candidate]
candidates = [candidate1, candidate2, candidate3]

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList
  [ (candidateId candidate1, candidate1)
  , (candidateId candidate2, candidate2)
  , (candidateId candidate3, candidate3)
  ]
