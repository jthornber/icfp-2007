module GInstr (GInstr (..), GProgram, toByteCode) where

import FastString (FastString)
import qualified FastString as FS

import Data.Char

data GInstr = BLACK
            | RED
            | GREEN
            | YELLOW
            | BLUE
            | MAGENTA
            | CYAN
            | WHITE
            | TRANSPARENT
            | OPAQUE
            | EMPTY_BUCKET
            | MOVE
            | TURN_ANTI_CLOCKWISE
            | TURN_CLOCKWISE
            | MARK
            | LINE
            | FILL
            | ADD_BITMAP
            | COMPOSE
            | CLIP
            | NOOP
              deriving (Show, Eq, Enum)

type GProgram = [GInstr]

toByteCode :: GProgram -> FastString
toByteCode prog = bc' prog FS.empty
    where
      bc' [] acc = acc
      bc' (x:xs) acc = bc' xs $! (FS.snoc acc (chr (fromEnum x)))
