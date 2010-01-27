import GInstr

import FastString (FastString)
import qualified FastString as FS

----------------------------------------------------------------
-- RNA/GInstr conversion
table :: [(FastString, GInstr)]
table = map (\(str, instr) -> (FS.pack str, instr))
        [ ("PIPIIIC", BLACK)
        , ("PIPIIIP", RED)
        , ("PIPIICC", GREEN)
        , ("PIPIICF", YELLOW)
        , ("PIPIICP", BLUE)
        , ("PIPIIFC", MAGENTA)
        , ("PIPIIFF", CYAN)
        , ("PIPIIPC", WHITE)
        , ("PIPIIPF", TRANSPARENT)
        , ("PIPIIPP", OPAQUE)
        , ("PIIPICP", EMPTY_BUCKET)
        , ("PIIIIIP", MOVE)
        , ("PCCCCCP", TURN_ANTI_CLOCKWISE)
        , ("PFFFFFP", TURN_CLOCKWISE)
        , ("PCCIFFP", MARK)
        , ("PFFICCP", LINE)
        , ("PIIPIIP", FILL)
        , ("PCCPFFP", ADD_BITMAP)
        , ("PFFPCCP", COMPOSE)
        , ("PFFICCF", CLIP)
        ]

decodeInstr :: FastString -> GInstr
decodeInstr dna = case lookup dna table of
                    Nothing -> NOOP
                    Just i -> i

splitEvery :: Int -> FastString -> [FastString]
splitEvery n xs
    | xs == FS.empty = []
    | otherwise = case FS.splitAt n xs of
                    (x, rst) -> x : splitEvery n rst


----------------------------------------------------------------
-- Top level
main :: IO ()
main = FS.putStr . toByteCode . map decodeInstr . splitEvery 7 =<< FS.getContents
