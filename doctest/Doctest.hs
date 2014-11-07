import           Test.DocTest

main = doctest ["-isrc", "src/Rho/Metainfo.hs",
                "-isrc", "src/Rho/Tracker.hs"]
