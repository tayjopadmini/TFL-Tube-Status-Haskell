module Tfl_Tube_DB where
import Database.HDBC
import Database.HDBC.Sqlite3

-- |'storeStatus' function to store a list of line names and status to the table lines
storeStatus ::[(String, String)] -> IO()
storeStatus []= return ()
--storeStatus [x] = print ("ok")
storeStatus xs = do
 conn <- connectSqlite3 "tfl.db"
 quickQuery' conn "DROP TABLE IF EXISTS lines" []
 run conn "CREATE TABLE lines (lineName TEXT, lineStatus TEXT)" []
 stmt <- prepare conn "INSERT INTO lines (lineName,lineStatus) VALUES (?,?)"
 executeMany stmt (map (\x -> [toSql $ fst x, toSql $ snd x]) xs)
 commit conn

-- |'printStatus' get the data from the database and outputs to the terminal
printStatus = do
  conn <- connectSqlite3 "tfl.db"
  r <- quickQuery' conn "SELECT * FROM lines" []
  mapM_ putStrLn (map convRow r)
  disconnect conn
convRow :: [SqlValue] -> String
convRow [sqllineName, sqllineStatus] = lineName ++ "         - " ++ lineStatus
     where lineStatus =fromSql sqllineStatus
           lineName = case fromSql sqllineName of
                     Just x -> x
                     Nothing -> "Updated at "



-- |'storeNaptan' function to store a list of naptan ID and station name to the table Naptan
storeNaptan ::[(String, String)] -> IO()
storeNaptan []= return ()
storeNaptan ts = do
 conn <- connectSqlite3 "tfl.db"
 quickQuery' conn "DROP TABLE IF EXISTS naptan" []
 run conn "CREATE TABLE naptan (naptanId TEXT, stationName TEXT)" []
 stmt <- prepare conn "INSERT INTO naptan (naptanId,stationName) VALUES (?,?)"
 executeMany stmt (map (\x -> [toSql $ fst x, toSql $ snd x]) ts)
 commit conn

-- |'selectNaptanId' function to select the naptanID
selectNaptanId :: String -> IO[String]
--selectNaptanId _= return ()
selectNaptanId reqStation = do
  conn <- connectSqlite3 "tfl.db"
  r <- quickQuery' conn "SELECT naptanId FROM naptan WHERE stationName = (?) "  [toSql (reqStation++" Underground Station"::String)]
  return $ map fromSql (map head r)

-- |'storeArrivals' function which stores arrivals information for a given station to the table stationArrivals
storeArrivals ::[(((String, String), (String, String)),((String, String), (Int, String)))] -> IO()
storeArrivals []= return ()
storeArrivals vs = do
 conn <- connectSqlite3 "tfl.db"
 quickQuery' conn "DROP TABLE IF EXISTS stationArrivals" []
 run conn "CREATE TABLE stationArrivals (lineId TEXT,lineName TEXT,platformName TEXT,currentLocation TEXT,modeName TEXT,destinationName TEXT,timeToStation INT,towards TEXT)" []
 stmt <- prepare conn "INSERT INTO stationArrivals (lineId,lineName,platformName,currentLocation,modeName,destinationName,timeToStation,towards) VALUES (?,?,?,?,?,?,?,?)"
 executeMany stmt (map (\x -> [toSql $ fst$fst$fst x, toSql $ snd$fst$fst x,toSql $ fst$snd$fst x,toSql $ snd$snd$fst x,toSql $ fst$fst$snd x,toSql $ snd$fst$snd x, toSql $ fst$snd$snd x, toSql $ snd$snd$snd x]) vs)
 commit conn

-- |'printArrivals' get the arrivals data from the database and outputs to the terminal
printArrivals = do
  conn <- connectSqlite3 "tfl.db"
  r <- quickQuery' conn "SELECT lineName, platformName, timeToStation, towards FROM stationArrivals ORDER BY lineName, platformName, timeToStation ASC" []
  mapM_ putStrLn (map convRow3 r)
  disconnect conn
convRow3 :: [SqlValue] -> String
convRow3 [sqllineName,sqlplatformName,sqltimeToStation,sqltowards] = platformName ++": "++lineName++" line towards "++ towards ++" "++timeToStation
     where lineName = fromSql sqllineName
           platformName = fromSql sqlplatformName
     	   timeToStation = case fromSql sqltimeToStation of
     	                   "0" -> "Train Approaching"
     	                   "1" -> "in 1 min"
     	                   x -> "in "++x++ " mins"
           towards = case fromSql sqltowards of
                     Just x -> x
                     Nothing -> "Updated at "
