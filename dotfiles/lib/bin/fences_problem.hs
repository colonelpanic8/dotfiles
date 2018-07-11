#!/usr/bin/env run_haskell_stack.sh
{-# LANGUAGE OverloadedStrings, AllowAmbiguousTypes #-}

import Data.Stack
import Data.Maybe
import Debug.Trace

data ProblemState = ProblemState
  { maxValue :: Int
  , maxHeight :: Int
  , maxStartIndex :: Int
  , maxEndIndex :: Int
  , heightStartStack :: Stack (Int, Int)
  } deriving Show

stackPeekDef theStack = fromMaybe (0, 0) $ stackPeek theStack

processItem index problemState =
  let (newStack, (itemIndex, itemHeight)) =
        fromMaybe (stackNew, (0, 0)) $ stackPop $ heightStartStack problemState
      rectSize = (index - itemIndex) * itemHeight
  in if rectSize > (maxValue problemState)
     then problemState
            { maxValue = rectSize
            , maxHeight = itemHeight
            , maxStartIndex = itemIndex
            , maxEndIndex = index
            , heightStartStack = newStack
            }
     else problemState { heightStartStack = newStack }

processFenceHeight _ [] problemState = problemState
processFenceHeight index allHeights@(height:heights) problemState =
  let hss = heightStartStack problemState
      (lastIndex, lastHeight) = stackPeekDef hss
      handleTaller = processFenceHeight (index + 1) heights $
                     problemState
                     { heightStartStack = stackPush hss (index, height) }
      handleShorter = let withNewMax = processItem index problemState
                          newProblemState =
                            withNewMax
                             { heightStartStack =
                                 addIfLarger (heightStartStack withNewMax)
                                             (height, lastIndex)
                             }
                      -- No increment because we may need to pop more off of the stack
                      in processFenceHeight index allHeights newProblemState
      addIfLarger theStack (index, height) =
        let (thisIndex, thisHeight) = stackPeekDef theStack
        in if thisHeight < height
           then stackPush theStack (index, height)
           else theStack
  in case lastHeight of
       _ | lastHeight < height -> handleTaller
       _ | lastHeight > height -> handleShorter
       _ -> processFenceHeight (index + 1) heights problemState

processFences fences =
  let lastState = processFenceHeight 0 fences
                  ProblemState { maxValue = -1
                               , maxHeight = -1
                               , maxStartIndex = -1
                               , maxEndIndex = -1
                               , heightStartStack = stackNew
                               }
      fenceCount = length fences
      processItems state
        | stackIsEmpty (heightStartStack state) = state
        | otherwise = processItems $ processItem fenceCount state
  in processItems lastState

main = do
  let finalState = processFences [2, 4, 6, 6, 6, 7, 6, 2, 2, 2, 2, 2, 2, 2]
  print $ maxValue finalState
  print $ maxHeight finalState
  print $ maxStartIndex finalState
  print $ maxEndIndex finalState
