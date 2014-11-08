import           Test.DocTest

main = doctest ["-isrc", "src/Rho/Metainfo.hs",
                "-isrc", "src/Rho/Tracker.hs",
                "-isrc", "src/Rho/Magnet.hs",
                "-isrc", "src/Rho/Comms.hs"
                ]
