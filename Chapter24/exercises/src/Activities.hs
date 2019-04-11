module Activities where

import Control.Applicative
import Text.Trifecta
import Data.Maybe (fromMaybe)

type Hours = Integer
type HourOfDay24 = Integer
type Minute = Integer
type Year = Integer
type Day = Integer

data Activity =
    Activity
    { hourOfDay    :: HourOfDay24
    , minuteOfHour :: Minute
    , description  :: String
    } deriving Show

data Month =
      Jan
    | Feb
    | Mar 
    | Apr 
    | May 
    | Jun 
    | Jul 
    | Aug 
    | Sep 
    | Oct 
    | Nov 
    | Dec deriving Show

stringToMonth :: String -> Maybe Month
stringToMonth s = case s of
    "01" -> Just Jan
    "02" -> Just Feb
    "03" -> Just Mar
    "04" -> Just Apr
    "05" -> Just May
    "06" -> Just Jun
    "07" -> Just Jul
    "08" -> Just Aug
    "09" -> Just Sep
    "10" -> Just Oct
    "11" -> Just Nov
    "12" -> Just Dec
    _    -> Nothing

data Date =
    Date
    { year        :: Year
    , monthOfYear :: Month
    , dayOfMonth  :: Day
    } deriving Show

data DayOfActivities =
    DayOfActivities
    { date :: Date
    , activities :: [Activity]
    } deriving Show

parseDayOfActivities :: Parser DayOfActivities
parseDayOfActivities = DayOfActivities <$>
                       (optional newline *> char '#' *> optional (char ' ') *> 
                        date <*
                        many (noneOf "\n") <*
                        newline
                       ) <*>
                       many daysOfActivities
    where
        comment = optional (char ' ') *> string "--"
        date = Date <$> year <*> (char '-' *> month) <*> (char '-' *> day)
        year = read <$> count 4 alphaNum
        month = do
            m <- stringToMonth <$> (count 2 alphaNum)
            case m of
                Nothing -> fail "Error: month must be between 0 and 11"
                Just x -> return x
        day = read <$> count 2 alphaNum
        daysOfActivities :: Parser Activity 
        daysOfActivities = Activity <$>
                           hour <*>
                           (char ':' *> minute) <*>
                           (char ' ' *> description)
        hour = read <$> count 2 alphaNum
        minute = read <$> count 2 alphaNum
        description = (   (try (manyTill (noneOf "\n") (try comment)) <* many (noneOf "\n"))
                      <|> many (noneOf "\n")) <* newline

parseLogFile :: Parser [DayOfActivities]
parseLogFile = many parseDayOfActivities