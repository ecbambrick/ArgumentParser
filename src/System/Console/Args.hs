{-# LANGUAGE FlexibleInstances #-}

module System.Console.Args where

import Control.Monad.State ( State(..) )
import Data.Maybe          ( catMaybes )

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
    fromArgument arg = Just $ catMaybes $ map (\x -> fromArgument [x]) arg

------------------------------------------------------------------------- Types

-- | The command-line interface monad.
type CLI = State CommandInfo

-- | A command line argument that can be parsed.
data Argument = Command    String CommandInfo
              | Positional String
              | Optional   (Maybe Char) (Maybe String) String
              | Flag       (Maybe Char) (Maybe String) String

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
