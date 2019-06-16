module Main where

import           Candidates
import           Grades
import           System.IO  (hFlush, stdout)

main :: IO ()
main = do
  statement <- assessCandidateM readCandidate
  putStr "Candidate assessment: "
  hFlush stdout
  print statement
--      do
-- candidate <- readCandidate
-- print candidate
-- putStr "Candidate viable: "
-- hFlush stdout
-- print $ viable candidate

