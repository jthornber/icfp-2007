import System.Vacuum.OpenGL
import FastString (FastString)
import qualified FastString as F

-- You need to run vacuum-opengl-server first, then run this program.
-- You must also have TMPDIR set with a trailing '/' due to a bug inthe library.
--
--   > vacuum-opengl-server &
--   > TMPDIR=/tmp/ ./FastStringVisualise

test :: FastString -> FastString
test txt = ((F.take 100000 $ F.drop 1000 txt) `F.append` txt)

main = do
  txt <- F.readFile "dna/endo.dna"
  view $ test txt
