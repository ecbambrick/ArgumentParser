import System.Console.Args
import Test.Hspec

type TestInterface = CLI (String, [String])

-- | Test cases.
main :: IO ()
main = hspec $ do
    describe "System.Console.Args.pureInterface (no command)" $ do
        testTodo "                       " $ Left  (MissingArgument "a")
        testTodo "a                      " $ Left  (MissingArgument "b")
        testTodo "a b                    " $ Right ("<a> <b>", ["a", "b"])
        testTodo "a b c                  " $ Left  (InvalidCommand)
        testTodo "-f                     " $ Left  (UnexpectedOption "-f")
        testTodo "a -f                   " $ Left  (UnexpectedOption "-f")
        testTodo "a b -f                 " $ Left  (UnexpectedOption "-f")
        testTodo "a b c -f               " $ Left  (UnexpectedOption "-f")

    describe "System.Console.Args.pureInterface (add text <text1> <text2>)" $ do
        testTodo "add text               " $ Right ("<a> <b>", ["add", "text"])
        testTodo "add text a             " $ Left  (MissingArgument "text2")
        testTodo "add text a b           " $ Right ("add text <text1> <text2>", ["a", "b"])
        testTodo "add text a b c         " $ Left  (InvalidCommand)

    describe "System.Console.Args.pureInterface (mark <id> (complete | incomplete) [--flag])" $ do
        testTodo "mark 10                " $ Right ("<a> <b>", ["mark", "10"])
        testTodo "mark 10 complete       " $ Right ("mark <id> complete [--flag]", ["10", "False"])
        testTodo "mark 10 incomplete     " $ Right ("mark <id> incomplete [--flag]", ["10", "False"])
        testTodo "mark 10 incomplete x   " $ Left  (InvalidCommand)
        testTodo "mark x incomplete      " $ Left  (InvalidArgument "id" "x")
        testTodo "mark 10 -f             " $ Left  (InvalidCommand)
        testTodo "mark 10 complete -f    " $ Right ("mark <id> complete [--flag]", ["10", "True"])
        testTodo "mark 10 incomplete -f  " $ Right ("mark <id> incomplete [--flag]", ["10", "True"])
        testTodo "mark 10 incomplete x -f" $ Left  (InvalidCommand)
        testTodo "mark x incomplete -f   " $ Left  (InvalidArgument "id" "x")

    describe "System.Console.Args.pureInterface (list)" $ do
        testTodo "list                   " $ Right ("list", [])
        testTodo "list a                 " $ Right ("<a> <b>", ["list", "a"])

    describe "System.Console.Args.pureInterface (export <id> to <file>)" $ do
        testTodo "export 10 to           " $ Left  (MissingArgument "file")
        testTodo "export 10 to file      " $ Right ("export <id> to <file>", ["10", "file"])
        testTodo "export x to file       " $ Left  (InvalidArgument "id" "x")
        testTodo "export x to            " $ Left  (InvalidArgument "id" "x")

    describe "System.Console.Args.pureInterface (deep <a> command <b> test <c>)" $ do
        testTodo "deep a command         " $ Left  (MissingArgument "b")
        testTodo "deep a command b       " $ Left  (InvalidCommand)
        testTodo "deep a command b test  " $ Left  (MissingArgument "c")
        testTodo "deep a command b test c" $ Right ("deep <a> command <b> test <c>", ["a", "b", "c"])

    describe "System.Console.Args.pureInterface (option [-x=><value>])" $ do
        testTodo "option                 " $ Right ("option [--flag=<value>]", ["Nothing"])
        testTodo "option -x              " $ Left  (UnexpectedOption "-x")
        testTodo "option -x y            " $ Left  (UnexpectedOption "-x")
        testTodo "option -f              " $ Left  (MissingValue "-f")
        testTodo "option -f x            " $ Right ("option [--flag=<value>]", ["Just \"x\""])
        testTodo "option --flag          " $ Left  (MissingValue "--flag")
        testTodo "option --flag x        " $ Right ("option [--flag=<value>]", ["Just \"x\""])
        testTodo "option --f             " $ Left  (UnexpectedOption "--f")
        testTodo "option --f x           " $ Left  (UnexpectedOption "--f")
        testTodo "option -flag           " $ Left  (UnexpectedOption "-flag")
        testTodo "option -flag x         " $ Left  (UnexpectedOption "-flag")
        testTodo "option -f x -f         " $ Left  (UnexpectedOption "-f")
        testTodo "option --flag x -f     " $ Left  (UnexpectedOption "-f")
        testTodo "option -f x --flag     " $ Left  (UnexpectedOption "--flag")
        testTodo "option --flag x --flag " $ Left  (UnexpectedOption "--flag")

-- | Tests the todo interface with the given input and compares its output with the
-- | given output.
testTodo :: String -> Either CLIError (String, [String]) -> Spec
testTodo input (Left  output) = it (input ++ " -> " ++ show output) $ todoInterface (words input) `shouldBe` (Left output)
testTodo input (Right output) = it (input ++ " -> " ++ show output) $ todoInterface (words input) `shouldBe` (Right output)

-- | A simple todo interface.
todoInterface :: [String] -> Either CLIError (String, [String])
todoInterface = interface "A simple todo application." $ do
    command "add text" $ do
        text1 <- argument "text1"
        text2 <- argument "text2"
        run ("add text <text1> <text2>", [text1, text2])

    command "option" $ do
        x <- option ('f', "flag") "something" :: TestInterface (Maybe String)
        run ("option [--flag=<value>]", [show x])

    command "mark" $ do
        x  <- option ('f', "flag") "Some flag" :: TestInterface Bool
        id <- argument "id" :: TestInterface Int

        command "complete" $ do
            run ("mark <id> complete [--flag]", [show id, show x])

        command "incomplete" $ do
            run ("mark <id> incomplete [--flag]", [show id, show x])

    command "list" $ do
        run ("list", [])

    command "export" $ do
        id <- argument "id" :: TestInterface Int
        command "to" $ do
            filePath <- argument "file"
            run ("export <id> to <file>", [show id, filePath])

    command "deep" $ do
        a <- argument "a"
        command "command" $ do
            b <- argument "b"
            command "test" $ do
                c <- argument "c"
                run ("deep <a> command <b> test <c>", [a,b,c])

    noCommand $ do
        a <- argument "a"
        b <- argument "b"
        run ("<a> <b>", [a, b])