{-# LANGUAGE FlexibleInstances #-}

module System.Console.Args where

import qualified Control.Monad.State  as State
import qualified Control.Monad.Writer as Writer
import qualified System.Environment   as Environment

import Control.Monad.State ( State, forM_, unless, when )
import Data.Char           ( toLower )
import Data.Either         ( lefts, rights )
import Data.List           ( find, findIndex, intercalate, isPrefixOf, sort )
import Data.Maybe          ( fromJust, fromMaybe, isJust, listToMaybe )
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

instance FromArgument Bool where
    fromArgument arg
        | lowerArg == "true"  || lowerArg == "t" = Just True
        | lowerArg == "false" || lowerArg == "f" = Just False
        | otherwise                              = Nothing
        where lowerArg = map toLower arg

-- | Class of types that can be converted from a command-line option.
class FromOption a where
    parseOption :: Maybe String -> Maybe String -> [String] -> OptionResult a
    hasValue    :: a -> Bool

instance FromOption Bool where
    parseOption = popFlag
    hasValue  _ = False

instance FromArgument a => FromOption (Maybe a) where
    parseOption = popOption
    hasValue  _ = True

------------------------------------------------------------------------- Types

-- | The command-line interface monad.
type CLI a = State (CommandInfo a)

-- | The result of parsing a command-line option.
type OptionResult a = (Maybe CLIError, a, [String])

-- | A command-line argument that can be parsed.
data Argument a = Command    String (CommandInfo a)
                | Positional String
                | Option     (Maybe String) (Maybe String) String
                | Flag       (Maybe String) (Maybe String) String
                deriving (Show)

-- | An error that can occur while parsing a command. The Ord instance is used
-- | to determine the priority when reporting errors.
data CLIError = InvalidArgument  String String
              | MissingArgument  String
              | InvalidOption    String String
              | MissingValue     String
              | InvalidCommand
              | UnexpectedOption String
              deriving (Eq, Ord)

-- | The details related to a command.
data CommandInfo a = CommandInfo
    { commandAction    :: Maybe a
    , commandError     :: Maybe CLIError
    , commandIsMatch   :: Bool
    , commandStack     :: [String]
    , commandArguments :: [Argument a] }

instance Show CLIError where
    show InvalidCommand               = "Invalid command."
    show (InvalidArgument name value) = "Invalid value for "  ++ format name ++ ": " ++ value
    show (InvalidOption name value)   = "Invalid value for "  ++ name        ++ ": " ++ value
    show (MissingArgument name)       = "No value found for " ++ format name
    show (MissingValue name)          = "No value found for " ++ name
    show (UnexpectedOption name)      = "Unexpected option: " ++ name

instance Show (CommandInfo a) where
    show (CommandInfo action err isMatch stack args) =
        let actionString = if isJust action then "Just (IO ())" else "Nothing"
            contents     = intercalate "," [ "commandAction = "    ++ actionString
                                           , "commandError = "     ++ show err
                                           , "commandIsMatch = "   ++ show isMatch
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
    name <- Environment.getProgName

    case interface name description commands args of
        Left (err, help) -> hPutStrLn stderr (show err ++ "\n\n" ++ help)
        Right action     -> action

-- | Evaluates a pure command-line interface with the given program name,
-- | description, and list of arguments. If -h or --help is passed in, they
-- | will be ignored.
interface :: String -> String -> CLI a () -> [String] -> Either (CLIError, String) a
interface programName description commands args =
    let initialState = CommandInfo Nothing Nothing True args []
        finalState   = State.execState commands initialState
        help         = generateHelp programName description finalState
        results      = processCommands finalState

    in case results of
        Left  err -> Left (err, help)
        Right act -> Right act

-- | Recursively traverse the given command and return the result of the first
-- | successful command or an error if no valid command could be found.
processCommands :: CommandInfo a -> Either CLIError a
processCommands (CommandInfo action err _ stack arguments) =
    let thisResult = case (stack, err, action) of
            ([], Just x, _) -> [Left x]
            ([], _, Just x) -> [Right x]
            _               -> []

        unexpectedResults = case find (isPrefixOf "-") stack of
            Just x  -> [Left (UnexpectedOption x)]
            Nothing -> []

        commands     = filter isMatchingCommand arguments
        childResults = map (\(Command _ info) -> processCommands info) commands
        allResults   = childResults ++ thisResult ++ unexpectedResults
        success      = listToMaybe $ rights allResults
        failure      = listToMaybe $ sort (lefts allResults)

    in case (success, failure) of
        (Just x, _) -> Right x
        (_, Just x) -> Left x
        _           -> Left InvalidCommand

---------------------------------------------------------------------- Builders

-- | Retruns the value of the named positional argument.
argument :: (FromArgument b) => String -> CLI a b
argument name = do
    stack <- State.gets commandStack

    let (arg, newStack) = popPositional stack
        parsedArg       = fromArgument =<< arg

    setStack newStack
    addArgument (Positional name)

    case (arg, parsedArg) of
        (Just ('-':x), _) -> setError (UnexpectedOption ('-':x)) >> return undefined
        (Just _,  Just x) -> return x
        (Just x, Nothing) -> setError (InvalidArgument name x)   >> return undefined
        (Nothing,      _) -> setError (MissingArgument name)     >> return undefined

-- | Creates a new command with the given name.
command :: String -> CLI a () -> CLI a ()
command name cli = do
    arguments <- State.gets commandArguments
    stack     <- State.gets commandStack
    err       <- State.gets commandError

    let (isMatch, newStack) = popCommand name stack
        optionals           = filter isOptional arguments
        initialState        = CommandInfo Nothing err isMatch newStack optionals
        finalState          = State.execState cli initialState

    addArgument (Command name finalState)

-- | Equivalent to the id function. Used for readability when a command contains
-- | an action as well as sub-commands.
noCommand :: CLI a () -> CLI a ()
noCommand = id

-- | Returns the maybe value of the option with the given name(s). If the
-- | option has no value (i.e. a flag), whether or not the option exists is
-- | returned instead.
option :: (ToName n, FromOption b) => n -> String -> CLI a b
option name description = do
    stack <- State.gets commandStack

    let (shortName, longName)   = formatNames $ toName name
        (err, result, newStack) = parseOption shortName longName stack
        optionType              = if hasValue result then Option else Flag

    when (isJust err) $ setError (fromJust err)
    addArgument (optionType shortName longName description)
    setStack newStack
    return result

-- Runs the given IO action if the containing command is called.
run :: a -> CLI a ()
run action = State.modify $ \state -> state { commandAction = Just action }

--------------------------------------------------------------- Help generation

-- | Returns a help document for the given command information.
generateHelp :: String -> String -> CommandInfo a -> String
generateHelp programName description info = unlines $ Writer.execWriter $ do
    let helpCommand = programName ++ " --help"
        commands    = getCommandDescriptions info programName ++ [helpCommand]

    unless (null description) $ Writer.tell [description ++ "\n"]
    Writer.tell ["Usage:"]
    forM_ commands $ \x -> Writer.tell ["  " ++ x]

-- | Returns a list of command descriptions for a help document.
getCommandDescriptions :: CommandInfo a -> String -> [String]
getCommandDescriptions (CommandInfo action _ _ _ arguments) path =
    let (children, this) = flip State.execState ([], path) $ do
            forM_ arguments $ \x -> case x of
                Positional name   -> appendPositional name
                Command name info -> appendSubcommand name info
                _                 -> return ()

    in if isJust action then this:children else children

-- | Appends the given positional argument name to the current command
-- | description.
appendPositional :: String -> State ([String], String) ()
appendPositional name =
    State.modify $ \(commands, path) ->
        (commands, path ++ " " ++ format name)

-- | Appends a description for each subcommand of the given command to the list
-- | of command descriptions.
appendSubcommand :: String -> CommandInfo a -> State ([String], String) ()
appendSubcommand name info =
    State.modify $ \(commands, path) ->
        let currentPath = path ++ " " ++ name
            children    = getCommandDescriptions info currentPath

        in (commands ++ children, path)

------------------------------------------------------------ Stack manipulation

-- | Pops the given pattern from the given stack and returns whether or not the
-- | pop occured along with the popped stack.
popCommand :: String -> [String] -> (Bool, [String])
popCommand name args =
    let tokens  = words name
        match   = tokens `isPrefixOf` args
        newArgs = if match then drop (length tokens) args else args

    in (match, newArgs)

-- | Pops all flags with the either of the given names and returns whether or
-- | not the pop occured along with the popped stack.
popFlag :: Maybe String -> Maybe String -> [String] -> OptionResult Bool
popFlag shortName longName stack =
    let newStack   = deleteAll longName $ deleteAll shortName stack
        changed    = length stack /= length newStack

    in (Nothing, changed, newStack)

-- | Pops the first occurence of the option with either of the given names and
-- | returns the popped maybe value along with the popped stack and error
-- | details.
popOption :: (FromArgument a) => Maybe String -> Maybe String -> [String] -> OptionResult (Maybe a)
popOption shortName longName stack =
    let (shortResult, shortStack) = takeWhen (== shortName) 2 stack
        (longResult,   longStack) = takeWhen (==  longName) 2 stack

    in case (shortResult, longResult) of
        ([k,v], _) -> case fromArgument v of
            Just  x -> (Nothing, Just x, shortStack)
            Nothing -> (Just (InvalidOption k v), Nothing, shortStack)

        (_, [k,v]) -> case fromArgument v of
            Just  x -> (Nothing, Just x, longStack)
            Nothing -> (Just (InvalidOption k v), Nothing, longStack)

        ([k], _) -> (Just (MissingValue k), Nothing, shortStack)
        (_, [k]) -> (Just (MissingValue k), Nothing, longStack)
        (_,   _) -> (Nothing, Nothing, stack)

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

-- | Returns a list of n many elements from the given list starting at the first
-- | element that satisfies the given predicate, along with a list of the
-- | remaining elements.
takeWhen :: (Eq a) => (Maybe a -> Bool) -> Int -> [a] -> ([a], [a])
takeWhen eq n xs =
    let index  = length xs `fromMaybe` findIndex (eq . Just) xs
        start  = take index xs
        middle = take n $ drop index xs
        end    = drop (index + n) xs

    in (middle, start ++ end)

-- | Surrounds the given string with angle brackets.
format :: String -> String
format x = "<" ++ x ++ ">"

-- | Formats the given argument names to include the "-" or "--".
formatNames :: (Maybe Char, Maybe String) -> (Maybe String, Maybe String)
formatNames (shortName, longName) =
    let shortName' = fmap (\x -> "-" ++ [x]) shortName
        longName'  = fmap (\x -> "--" ++ x)  longName

    in (shortName', longName')

-- | Returns whether or not the given argument is a command.
isCommand :: Argument a -> Bool
isCommand (Command _ _) = True
isCommand _             = False

-- | Returns whether or not the given argument is a command that is a match.
isMatchingCommand :: Argument a -> Bool
isMatchingCommand (Command _ info) = commandIsMatch info
isMatchingCommand _                = False

-- | Returns whether or not the given argument is optional.
isOptional :: Argument a -> Bool
isOptional (Option _ _ _) = True
isOptional (Flag   _ _ _) = True
isOptional _              = False

-- | Returns whether or not the given argument is positional.
isPositional :: Argument a -> Bool
isPositional (Positional _) = True
isPositional _              = False
