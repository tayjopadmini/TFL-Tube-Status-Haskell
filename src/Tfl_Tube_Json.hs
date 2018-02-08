module Tfl_Tube_Json where

import App_key
import Tfl_Tube_Data_Types
import Tfl_Tube_DB

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit

-- |The 'firstlevel' function take the parameter 'fstr' and a list of Json strings and gets the strings corespoding to the parameter 'fstr'. This is used for the main Json text.
firstlevel _[]=("")
firstlevel fstr (p:ps) = (fstr p) ++"," ++firstlevel fstr ps
-- |The 'secondlevel' function take the parameter 'sstr' and a list of Json strings and gets the strings corespoding to the parameter 'sstr'. this is used for Json text that is nested in the main text
secondlevel _[]=[]
secondlevel sstr (p:ps) = (sstr p): secondlevel sstr ps
-- |The 'firstLevelNum' function take the parameter 'nstr' and a list of Json intergers and gets the integers corespoding to the parameter 'nstr'. This is used for the main Json integer types.
firstLevelNum _[]=[]
firstLevelNum nstr (p:ps)=(nstr p):firstLevelNum nstr ps
-- |The 'convToMin' function converts a given 'Int' value to the lowest minute value
convToMin x = x `div` 60
-- |The 'listConvert' function converts a given String 'str' and converts it to a list of String arrays
listConvert :: (Char -> Bool) -> String -> [String]
listConvert p str =  case dropWhile p str of
                  "" -> []
                  str' -> w : listConvert p str''
                   where (w, str'') = break p str'
-- |The 'statusConversion' function zips given Strings for use in 'storeStatus'.
statusConversion x = do
 storeStatus (zip (listConvert (==',') $ firstlevel name x) (listConvert (==',') $ firstlevel statusSeverityDescription $ concat $ secondlevel lineStatuses x))
 return True
-- |The 'napId_Conversion' function zips given Strings for use in 'storeNaptan'.
napId_Conversion x =do
 storeNaptan (zip (listConvert (==',') $ firstlevel naptanId x) (listConvert (==',') $ firstlevel commonName x))
 return True
-- |The 'arrivals_Conversion' function zips given Strings for use in 'storeArrivals'.
arrivals_Conversion x = do
 storeArrivals (zip(zip (zip (listConvert (==',') $ firstlevel lineId x) (listConvert (==',') $ firstlevel lineName x)) (zip (listConvert (==',') $ firstlevel platformName x) (listConvert (==',') $ firstlevel currentLocation x))) (zip (zip (listConvert (==',') $ firstlevel modeName x) (listConvert (==',') $ firstlevel destinationName x)) (zip (map convToMin $ firstLevelNum timeToStation x) (listConvert (==',') $ firstlevel towards x))))
 return True

getJSON_Status:: IO B.ByteString
getJSON_Status = simpleHttp ("https://api.tfl.gov.uk/Line/Mode/tube/Status" ++ authentication)

-- |The 'getJSON_NaptanId' get Json data from the given naptanId retrival Url.
getJSON_NaptanId :: IO B.ByteString
getJSON_NaptanId = simpleHttp ("https://api.tfl.gov.uk/StopPoint/Type/NaptanMetroStation" ++ authentication)

-- |The 'retrieve_status' parses Json data from the given tube status Url.
retrieve_status= (eitherDecode <$> getJSON_Status) :: IO (Either String [Lines])
-- |The 'retrieve_NaptanId' parses Json data from the given naptanId retrival Url.
retrieve_NaptanId= (eitherDecode <$> getJSON_NaptanId) :: IO (Either String [Stoppoint])
-- |The 'retrieveLineStatus' function retrieves the line status and outputs the parsed data.
retrieveLineStatus :: IO ()
retrieveLineStatus = do
 d <- retrieve_status
 case d of
  Left err -> putStrLn err
  Right ps -> do
   --statusToJSON ps
   t <- statusConversion ps
   case t of
    True -> do
     ed <- printStatus
     print ed
    False -> print("error")
-- |The 'retrieveLineStatus' function retrieves all naptan ids then get the station name from the user using 'request' inputs it to 'retrieveArrivals' to get arrival data for user specified station.
retrieveNaptanId_with_Arrivals :: IO ()
retrieveNaptanId_with_Arrivals = do
 d <- retrieve_NaptanId
 case d of
  Left err -> putStrLn err
  Right ps -> do
   t <- napId_Conversion ps
   case t of
    True -> do
     putStrLn("Enter Station Name (capitalise each word)")
     stname<-getLine
     do
      es<-selectNaptanId stname
      do
       retrieveArrivals (head es)
    False -> print("error")
-- |The 'retrieveArrivals' function gets that arrival data for a specified station 'nap_Id'.
retrieveArrivals :: String -> IO ()
retrieveArrivals nap_Id = do
 let x="https://api.tfl.gov.uk/StopPoint/"++ nap_Id ++"/Arrivals" ++ authentication
 do
  d <- (eitherDecode <$> (simpleHttp x) :: IO (Either String [Arrivals]))
  case d of
   Left err -> putStrLn err
   Right ps -> do
    t <- arrivals_Conversion ps
    case t of
     True -> do
      ed <- printArrivals
      print ed
     False -> print("error")
