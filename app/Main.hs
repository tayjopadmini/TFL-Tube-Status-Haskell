import Tfl_Tube_Json
import System.Environment


main = do
   args <- getArgs
   case args of
      --[status] -> do retrieveData
      ["status"]-> do retrieveLineStatus
      ["stations"]-> do retrieveNaptanId_with_Arrivals
       --retrieveArrivals ec
      _ -> inputError

inputError = putStrLn
   "Usage: Tfl-tube command [args]\n\
   \\n\
   \status           Create database urls.db\n\
   \stations         Shows the Arrivals of the trains for each platform\n\
   \(while inputting the station name plase make sure the first letter of each word\n\
   \the station name is capital)"
