import RareAlleleHistogram (showHistogram, parseHistogram)

main = do
    let fn = "/lustre/scratch114/projects/ancient-hinxton/EUR.gonl.gdk.histogram.combined.txt"
    s <- readFile fn
    let Right hist = parseHistogram s
        Right outs = showHistogram hist
    putStrLn outs

