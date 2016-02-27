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

-- | Class of types that can be converted from a command line argument value.
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
type CLI = State CommandInfo

-- | A command line argument that can be parsed.
data Argument = Command    String CommandInfo
              | Positional String
              | Optional   (Maybe Char) (Maybe String) String
              | Flag       (Maybe Char) (Maybe String) String
              deriving (Show)

-- | An error that can occur while parsing a command. The Ord instance is used
-- | to determine the priority when reporting errors.
data Error = InvalidArgument String String
           | MissingArgument String
           | InvalidCommand
           deriving (Eq, Ord)

-- | The details related to a command.
data CommandInfo = CommandInfo
    { commandAction    :: Maybe (IO ())
    , commandError     :: Maybe Error
    , commandStack     :: [String]
    , commandArguments :: [Argument] }

instance Show Error where
    show InvalidCommand               = "Invalid command."
    show (InvalidArgument name value) = "Invalid value for "  ++ format name ++ ": " ++ value
    show (MissingArgument name)       = "No value found for " ++ format name

instance Show CommandInfo where
    show (CommandInfo action err stack args) =
        let actionString = if isJust action then "Just (IO ())" else "Nothing"
            contents     = intercalate "," [ "commandAction = "    ++ actionString
                                           , "commandError = "     ++ show err
                                           , "commandStack = "     ++ show stack
                                           , "commandArguments = " ++ show args ]

            in "CommandInfo {" ++ contents ++ "}"

-------------------------------------------------------------------- Intepreter

-- | Generate and run a command-line interface. If any errors occur, or if
-- | either -h or --help is passed in, the help screen will be shown.
cli :: String -> CLI () -> IO ()
cli description commands = do
    args <- Environment.getArgs

    let initialState = CommandInfo Nothing Nothing args []
        finalState   = State.execState commands initialState
        result       = processCommands finalState

    case result of
        Left  err -> hPutStrLn stderr (show err)
        Right act -> act

-- | Recursively traverse the CLI commands and return an IO action to perform
-- | or an error if no valid action could be found.
processCommands :: CommandInfo -> Either Error (IO ())
processCommands (CommandInfo action err stack arguments)
    | isJust validChild           = Right $ fromJust validChild
    | null stack && isJust err    = Left  $ fromJust err
    | null stack && isJust action = Right $ fromJust action
    | isJust errorChild           = Left  $ fromJust errorChild
    | otherwise                   = Left  $ InvalidCommand
    where
        commands   = filter isCommand arguments
        children   = map (\(Command _ info) -> processCommands info) commands
        errorChild = listToMaybe $ take 1 $ sort $ lefts children
        validChild = listToMaybe $ rights children

---------------------------------------------------------------------- Builders

-- | Gets the value of the named positional argument.
argument :: (FromArgument a) => String -> CLI a
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
command :: String -> CLI () -> CLI ()
command name cli = do
    arguments <- State.gets commandArguments
    stack     <- State.gets commandStack
    err       <- State.gets commandError

    let (match, newStack)  = popCommand name stack
        err'               = if match then err else Just InvalidCommand
        optionals          = filter isOptional arguments
        initialState       = CommandInfo Nothing err' newStack optionals
        finalState         = State.execState cli initialState

    addArgument (Command name finalState)

-- | Equivalent to the id function. Used for readability when a command contains
-- | an action as well as sub-commands.
noCommand :: CLI () -> CLI ()
noCommand = id

-- Runs the given IO action if the containing command is called.
run :: IO () -> CLI ()
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

-- | Pops the first element from the given stack and returns the value along
-- | with the popped stack.
popPositional :: [String] -> (Maybe String, [String])
popPositional []     = (Nothing, [])
popPositional (x:xs) = (Just x, xs)

------------------------------------------------------------ State manipulation

-- | Adds the given argument to the current command's list of arguments.
addArgument :: Argument -> CLI ()
addArgument arg = do
    arguments <- State.gets commandArguments

    State.modify $ \state -> state { commandArguments = arguments ++ [arg] }

-- | Sets the error for the current command to the given error if it does not
-- | already contain an error.
setError :: Error -> CLI ()
setError err = do
    hasErr <- fmap isJust (State.gets commandError)

    when (not hasErr) $
        State.modify $ \state -> state { commandError = Just err }

-- | Sets the stack for the current command to the given stack.
setStack :: [String] -> CLI ()
setStack stack = State.modify $ \state -> state { commandStack = stack }

------------------------------------------------------------------------ Utlity

-- | Returns whether or not the given argument is a command.
isCommand :: Argument -> Bool
isCommand (Command _ _) = True
isCommand _             = False

-- | Returns whether or not the given argument is positional.
isPositional :: Argument -> Bool
isPositional (Positional _) = True
isPositional _              = False

-- | Returns whether or not the given argument is optional or a flag.
isOptional :: Argument -> Bool
isOptional arg = not (isCommand arg || isPositional arg)

-- | Surrounds the given string with angle brackets.
format :: String -> String
format x = "<" ++ x ++ ">"
