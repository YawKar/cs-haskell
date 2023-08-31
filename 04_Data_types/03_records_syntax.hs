import Data.Char (toUpper)
import Data.Function ((&))
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

data PersonBad = PersonBad String String Int

firstNameBad :: PersonBad -> String
firstNameBad (PersonBad name _ _) = name

lastNameBad :: PersonBad -> String
lastNameBad (PersonBad _ lastName _) = lastName

ageBad :: PersonBad -> Int
ageBad (PersonBad _ _ age) = age

data PersonGood
    = PersonGood {
        firstName :: String,
        lastName :: String,
        age :: Int
    }
    deriving (Eq, Show)

withAge :: Int -> PersonGood -> PersonGood
withAge newAge person = person { age = newAge }

updateLastName :: PersonGood -> PersonGood -> PersonGood
updateLastName person1 person2 = person2 { lastName = lastName person1 }

raisonDetre :: PersonGood
raisonDetre = PersonGood { firstName = "Raison", lastName = "Detre", age = 20 }

raison :: String
raison = firstName raisonDetre

detre :: String
detre = lastName raisonDetre

twenty :: Int
twenty = age raisonDetre

emptyDetreWith21Age :: PersonGood
emptyDetreWith21Age = raisonDetre {age = 21, firstName = ""}

twentyOne :: Int
twentyOne = age emptyDetreWith21Age

upperCasedSurname :: String
upperCasedSurname = emptyDetreWith21Age & lastName & map toUpper

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving (Show)

data LogEntry
    = LogEntry {
        timestamp :: UTCTime,
        logLevel :: LogLevel,
        message :: String
    }

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString entry =
    timeToString (timestamp entry) ++ ": "
        ++ show (logLevel entry) ++ ": "
        ++ message entry

-- Different ways to use data and record syntax in pattern matching
name1 :: PersonGood -> String
name1 person = firstName person ++ " " ++ lastName person -- straight person

name2 :: PersonGood -> String
name2 (PersonGood fn ln _) = fn ++ " " ++ ln -- positional pattern matching

name3 :: PersonGood -> String
name3 (PersonGood {firstName, lastName = ln}) = firstName ++ " " ++ ln -- if we only care about some subset of fields

abbrFirstName :: PersonGood -> PersonGood
abbrFirstName p@(PersonGood { firstName = fn, lastName = ln }) =
    if length fn < 2
        then p
        else p { firstName = head fn : "." }

data SomeData a b = A a | B a b

isA :: SomeData a b -> Bool
isA A {} = True
isA _ = False
