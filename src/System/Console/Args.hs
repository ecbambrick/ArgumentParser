{-# LANGUAGE FlexibleInstances #-}

module System.Console.Args where

import qualified Control.Monad.State as State
import qualified System.Environment  as Environment

import Control.Monad.State ( State(..), when )
import Data.Either         ( lefts, rights )
import Data.List           ( intercalate, isPrefixOf, sort )
import Data.Maybe          ( fromJust, isJust, listToMaybe )
import System.IO           ( hPutStrLn, stderr )

----------------------------------------------------------------------- Classes

-- | Class of types that can be converted to an argument name where the name is
-- | either a short name, long name, or both.
class ToName a where
    toName :: a -> (Maybe Char, Maybe String)

instance ToName Char where
    toName x = (Just x, Nothing)

instance ToName String where
    toName x = (Nothing, Just x)

instance ToName (Char, String) where
    toName (x, y) = (Just x, Just y)

-- | Class of types that can be converted from a command-line argument value.
class FromArgument a where
    fromArgument :: String -> Maybe a

instance FromArgument Int where
    fromArgument arg = case reads arg of
        [(x, "")] -> Just x
        _         -> Nothing

instance FromArgument Char where
    fromArgument [x] = Just x
    fromArgument  _  = Nothing

instance FromArgument String where
    fromArgument []  = Nothing
    fromArgument arg = Just arg

------------------------------------------------------------------------- Types

-- | The command-line interface monad.
type CLI a = State (CommandInfo a)

-- | A command-line argument that can be parsed.
data Argument a = Command    String (CommandInfo a)
                | Positional String
                | Optional   (Maybe Char) (Maybe String) String
                | Flag       (Maybe Char) (Maybe String) String
                deriving (Show)

-- | An error that can occur while parsing a command. The Ord instance is used
-- | to determine the priority when reporting errors.
data CLIError = InvalidArgument String String
              | MissingArgument String
              | InvalidCommand
              | Unrecognized    String
              | NoMatch
              deriving (Eq, Ord)

-- | The details related to a command.
data CommandInfo a = CommandInfo
    { commandAction    :: Maybe a
    , commandError     :: Maybe CLIError
    , commandStack     :: [String]
    , commandArguments :: [Argument a] }

instance Show CLIError where
    show InvalidCommand               = "Invalid command."
    show (InvalidArgument name value) = "Invalid value for "  ++ format name ++ ": " ++ value
    show (MissingArgument name)       = "No value found for " ++ format name
    show (Unrecognized name)          = "Unrecognized flag: " ++ name

instance Show (CommandInfo a) where
    show (CommandInfo action err stack args) =
        let actionString = if isJust action then "Just (IO ())" else "Nothing"
            contents     = intercalate "," [ "commandAction = "    ++ actionString
                                           , "commandError = "     ++ show err
                                           , "commandStack = "     ++ show stack
                                           , "commandArguments = " ++ show args ]

            in "CommandInfo {" ++ contents ++ "}"

-------------------------------------------------------------------- Intepreter

-- | Evaluates a command-line interface and runs the IO action for the first
-- | valid command. If any errors occur, or if either -h or --help is passed
-- | in, the help screen will be shown.
cli :: String -> CLI (IO ()) () -> IO ()
cli description commands = do
    args <- Environment.getArgs

    case interface description commands args of
        Left  err -> hPutStrLn stderr (show err)
        Right act -> act

-- | Evaluates a pure command-line interface with the given list of arguments.
-- | If -h or --help is passed in, they will be ignored.
interface :: String  -> CLI a () -> [String] -> Either CLIError a
interface description commands args =
    let initialState = CommandInfo Nothing Nothing args []
        finalState   = State.execState commands initialState

    in processCommands finalState

-- | Recursively traverse the given command and return the result of the first
-- | successful command or an error if no valid command could be found.
processCommands :: CommandInfo a -> Either CLIError a
processCommands (CommandInfo action err stack arguments)
    | isJust validChild           = Right $ fromJust validChild
    | null stack && isJust err    = Left  $ fromJust err
    | null stack && isJust action = Right $ fromJust action
    | isJust errorChild           = Left  $ fromJust errorChild
    | isJust unrecognized         = Left  $ Unrecognized (fromJust unrecognized)
    | otherwise                   = Left  $ InvalidCommand
    where
        commands     = filter isCommand arguments
        children     = map (\(Command _ info) -> processCommands info) commands
        unrecognized = listToMaybe $ filter (isPrefixOf "-") stack
        errorChild   = listToMaybe $ take 1 $ sort $ lefts children
        validChild   = listToMaybe $ rights children

---------------------------------------------------------------------- Builders

-- | Gets the value of the named positional argument.
argument :: (FromArgument b) => String -> CLI a b
argument name = do
    stack <- State.gets commandStack

    let (arg, newStack) = popPositional stack
        parsedArg       = fromArgument =<< arg

    setStack newStack
    addArgument (Positional name)

    case (arg, parsedArg) of
        (Just _,  Just x) -> return x
        (Just x, Nothing) -> setError (InvalidArgument name x) >> return undefined
        (Nothing,      _) -> setError (MissingArgument name)   >> return undefined

-- | Creates a new command with the given name.
command :: String -> CLI a () -> CLI a ()
command name cli = do
    arguments <- State.gets commandArguments
    stack     <- State.gets commandStack
    err       <- State.gets commandError

    let (match, newStack) = popCommand name stack
        err'              = if match then err else Just NoMatch
        optionals         = filter isOptional arguments
        initialState      = CommandInfo Nothing err' newStack optionals
        finalState        = State.execState cli initialState

    addArgument (Command name finalState)

-- | Equivalent to the id function. Used for readability when a command contains
-- | an action as well as sub-commands.
noCommand :: CLI a () -> CLI a ()
noCommand = id

-- | Gets whether or not the flag with the given name(s) exists.
flag :: (ToName b) => b -> String -> CLI a Bool
flag name description = do
    stack <- State.gets commandStack

    let (shortName, longName) = toName name
        (found, newStack)     = popFlag shortName longName stack

    setStack newStack
    addArgument (Flag shortName longName description)
    return found

-- Runs the given IO action if the containing command is called.
run :: a -> CLI a ()
run action = State.modify $ \state -> state { commandAction = Just action }

------------------------------------------------------------ Stack manipulation

-- | Pops the given pattern from the given stack and returns whether or not the
-- | pop occured along with the popped stack.
popCommand :: String -> [String] -> (Bool, [String])
popCommand name args =
    let tokens  = words name
        match   = tokens `isPrefixOf` args
        newArgs = if match then drop (length tokens) args else args

    in (match, newArgs)

-- | Pops the flag with the either the given character or string name and
-- | returns whether or not the pop occured along with the popped stack.
popFlag :: Maybe Char -> Maybe String -> [String] -> (Bool, [String])
popFlag shortName longName stack =
    let shortName' = fmap (\x -> "-" ++ [x]) shortName
        longName'  = fmap (\x -> "--" ++ x)  longName
        newStack   = deleteAll longName' $ deleteAll shortName' stack
        changed    = length stack /= length newStack

    in (changed, newStack)

-- | Pops the first element from the given stack and returns the value along
-- | with the popped stack.
popPositional :: [String] -> (Maybe String, [String])
popPositional []     = (Nothing, [])
popPositional (x:xs) = (Just x, xs)

------------------------------------------------------------ State manipulation

-- | Adds the given argument to the current command's list of arguments.
addArgument :: Argument a -> CLI a ()
addArgument arg = do
    arguments <- State.gets commandArguments

    State.modify $ \state -> state { commandArguments = arguments ++ [arg] }

-- | Sets the error for the current command to the given error if it does not
-- | already contain an error.
setError :: CLIError -> CLI a ()
setError err = do
    hasErr <- fmap isJust (State.gets commandError)

    when (not hasErr) $
        State.modify $ \state -> state { commandError = Just err }

-- | Sets the stack for the current command to the given stack.
setStack :: [String] -> CLI a ()
setStack stack = State.modify $ \state -> state { commandStack = stack }

------------------------------------------------------------------------ Utlity

-- | Removes all occurences of the given value from the given list.
deleteAll :: (Eq a) => Maybe a -> [a] -> [a]
deleteAll Nothing  = id
deleteAll (Just x) = recurse [] x
    where recurse accum x []     = accum
          recurse accum x (y:ys) =
              if x == y then recurse (accum)        x ys
                        else recurse (accum ++ [y]) x ys

-- | Surrounds the given string with angle brackets.
format :: String -> String
format x = "<" ++ x ++ ">"

-- | Returns whether or not the given argument is a command.
isCommand :: Argument a -> Bool
isCommand (Command _ _) = True
isCommand _             = False

-- | Returns whether or not the given argument is positional.
isPositional :: Argument a -> Bool
isPositional (Positional _) = True
isPositional _              = False

-- | Returns whether or not the given argument is optional or a flag.
isOptional :: Argument a -> Bool
isOptional arg = not (isCommand arg || isPositional arg)
