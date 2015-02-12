import RareAlleleHistogram (loadHistogram, showHistogram, parseHistogram)
import Control.Error.Script (runScript)
import Control.Monad.Trans.Either (hoistEither)

main = do
    let fn = "/lustre/scratch114/projects/ancient-hinxton/EUR.gonl.gdk.histogram.combined.txt"
    s <- readFile fn
    let Right hist = parseHistogram s
        Right outs = showHistogram hist
    putStrLn outs

