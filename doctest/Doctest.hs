import           Test.DocTest

main = doctest ["-isrc", "src/Rho/Metainfo.hs",
                "-isrc", "src/Rho/Tracker.hs",
                "-isrc", "src/Rho/Magnet.hs",
                "-isrc", "src/Rho/Comms.hs",
                "-isrc", "src/Rho/Utils.hs",
                "-isrc", "src/Rho/Parser.hs",
                "-isrc", "src/Rho/Bitfield.hs" ]
