module Grades where

import           Candidates
import qualified Data.Map   as Map
import           System.IO  (hFlush, stdout)


assessCandidateM :: Monad m =>  m Candidate -> m String
assessCandidateM candidates = do
   candidate <- candidates
   let passed = viable candidate
   let statement = if passed
                   then "passed"
                   else "failed"
   return statement

-- The function above works for all variants shown below:
--assessCandidateList :: [Candidate] -> [String]
--assessCandidateList candidates = do
--   candidate <- candidates
--   let passed = viable candidate
--   let statement = if passed
--                   then "passed"
--                   else "failed"
--   return statement

--assessCandidateMaybe :: Int -> Maybe String
--assessCandidateMaybe cId = do
--   candidate <- Map.lookup cId candidateDB
--   let passed = viable candidate
--   let statement = if passed
--                   then "passed"
--                   else "failed"
--   return statement

--assessCandidateIO :: IO String
--assessCandidateIO = do
--  candidate <- readCandidate
--  let passed = viable candidate
--  let statement = if passed
--                  then "passed"
--                  else "failed"
--  return statement

readCandidate :: IO Candidate
readCandidate = do
  putStrLn "Enter candidate details"
  putStr "               ID: "
  hFlush stdout
  id <- readInt
  putStr "       Code grade: "
  hFlush stdout
  codeGrade <- readGrade
  putStr "Culture fit grade: "
  hFlush stdout
  cultureGrade <- readGrade
  putStr "        Education: "
  hFlush stdout
  degree <- readDegree
  return Candidate { candidateId = id
                   , codeReview = codeGrade
                   , cultureFit = cultureGrade
                   , education = degree
                   }

viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where
    passedCoding = codeReview candidate > B
    passedCultureFit = cultureFit candidate > C
    educationMin = education candidate >= MS
    tests = [passedCoding, passedCultureFit, educationMin]

readInt :: IO Int
readInt = read <$> getLine

readGrade :: IO Grade
readGrade = read <$> getLine

readDegree :: IO Degree
readDegree = read <$> getLine
