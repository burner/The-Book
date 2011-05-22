>{{Haskell minitoc|chapter=Libraries Reference}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=Libraries Reference}}
== The IO Library ==

The IO Library (available by <tt>'''import'''</tt>ing the <code>System.IO</code> module)
contains many definitions, the most commonly used of which are listed below:

<pre>
data IOMode  = ReadMode   | WriteMode
             | AppendMode | ReadWriteMode

openFile     :: FilePath -> IOMode -> IO Handle
hClose       :: Handle -> IO ()

hIsEOF       :: Handle -> IO Bool

hGetChar     :: Handle -> IO Char
hGetLine     :: Handle -> IO String
hGetContents :: Handle -> IO String

getChar      :: IO Char
getLine      :: IO String
getContents  :: IO String

hPutChar     :: Handle -> Char -> IO ()
hPutStr      :: Handle -> String -> IO ()
hPutStrLn    :: Handle -> String -> IO ()

putChar      :: Char -> IO ()
putStr       :: String -> IO ()
putStrLn     :: String -> IO ()

readFile     :: FilePath -> IO String
writeFile    :: FilePath -> String -> IO ()

{- bracket must be imported from Control.Exception -}
bracket      ::
    IO a -> (a -> IO b) -> (a -> IO c) -> IO c
</pre>
{{body note|1=

The type <code>FilePath</code> is a ''type synonym'' for <code>String</code>.  That is, there is no difference between <code>FilePath</code> and <code>String</code>.  So, for instance, the <code>readFile</code> function takes a <code>String</code> (the file to read) and returns an action that, when run, produces the contents of that file.  See the section [[../../Type declarations/]] for more about type synonyms.
}}
Most of these functions are self-explanatory.  The <code>openFile</code> and <code>hClose</code> functions open and close a file, respectively, using the <code>IOMode</code> argument as the mode for opening the file.  <code>hIsEOF</code> tests for end-of file.  <code>hGetChar</code> and <code>hGetLine</code> read a character or line (respectively) from a file.  <code>hGetContents</code> reads the entire file.  The <code>getChar</code>, <code>getLine</code> and <code>getContents</code> variants read from standard input.  <code>hPutChar</code> prints a character to a file; <code>hPutStr</code> prints a string; and <code>hPutStrLn</code> prints a string with a newline character at the end. The variants without the <code>h</code> prefix work on standard output.  The <code>readFile</code> and <code>writeFile</code> functions read and write an entire file without
having to open it first.

The <code>bracket</code> function is used to perform actions safely.  Consider a function that opens a file, writes a character to it, and then closes the file.  When writing such a function, one needs to be
careful to ensure that, if there were an error at some point, the file is still successfully closed.  The <code>bracket</code> function makes this easy.  It takes three arguments: The first is the action to perform at the beginning.  The second is the action to perform at the end, regardless of whether there's an error or not.  The third is the action to perform in the middle, which might result in an error.  For
instance, our character-writing function might look like:

<pre>
writeChar :: FilePath -> Char -> IO ()
writeChar fp c =
    bracket
      (openFile fp WriteMode)
      hClose
      (\h -> hPutChar h c)
</pre>
This will open the file, write the character and then close the file. However, if writing the character fails, <code>hClose</code> will still be executed, and the exception will be reraised afterwards.  That way, you don't need to worry too much about catching the exceptions and about closing all of your handles.

== A File Reading Program ==

We can write a simple program that allows a user to read and write
files.  The interface is admittedly poor, and it does not catch all
errors (such as reading a non-existent file).  Nevertheless, it should
give a fairly complete example of how to use IO.  Enter the following
code into "FileRead.hs," and compile/run:

<pre>
module Main
    where

import System.IO
import Control.Exception

main = doLoop

doLoop = do
  putStrLn "Enter a command rFN wFN or q to quit:"
  command <- getLine
  case command of
    'q':_ -> return ()
    'r':filename -> do putStrLn ("Reading " ++ filename)
                       doRead filename
                       doLoop
    'w':filename -> do putStrLn ("Writing " ++ filename)
                       doWrite filename
                       doLoop
    _            -> doLoop

doRead filename =
    bracket (openFile filename ReadMode) hClose
            (\h -> do contents <- hGetContents h
                      putStrLn "The first 100 chars:"
                      putStrLn (take 100 contents))

doWrite filename = do
  putStrLn "Enter text to go into the file:"
  contents <- getLine
  bracket (openFile filename WriteMode) hClose
          (\h -> hPutStrLn h contents)
</pre>
What does this program do?  First, it issues a short string of
instructions and reads a command.  It then performs a <tt>'''case'''</tt> switch
on the command and checks first to see if the first character is a
`q.'  If it is, it returns a value of unit type.

{{body note|1=
The <code>return</code> function is a function that takes a value of type
<code>a</code> and returns an action of type <code>IO a</code>.  Thus, the type of
<code>return ()</code> is <code>IO ()</code>.
}}
If the first character of the command wasn't a `q,' the program checks
to see if it was an 'r' followed by some string that is bound to the
variable <code>filename</code>.  It then tells you that it's reading the file,
does the read and runs <code>doLoop</code> again.  The check for `w' is
nearly identical.  Otherwise, it matches <code>_</code>, the wildcard
character, and loops to <code>doLoop</code>.

The <code>doRead</code> function uses the <code>bracket</code> function to make sure
there are no problems reading the file.  It opens a file in
<code>ReadMode</code>, reads its contents and prints the first 100 characters
(the <code>take</code> function takes an integer <math>n</math> and a list and returns
the first <math>n</math> elements of the list).

The <code>doWrite</code> function asks for some text, reads it from the
keyboard, and then writes it to the file specified.

{{body note|1=
Both <code>doRead</code> and <code>doWrite</code> could have been made simpler by
using <code>readFile</code> and <code>writeFile</code>, but they were written in the
extended fashion to show how the more complex functions are used.
}}
The only major problem with this program is that it will die if you
try to read a file that doesn't already exists or if you specify some
bad filename like <code>*\bs^#_@</code>.  You may think that the calls
to <code>bracket</code> in <code>doRead</code> and <code>doWrite</code> should take care of
this, but they don't.  They only catch exceptions within the main
body, not within the startup or shutdown functions (<code>openFile</code> and
<code>hClose</code>, in these cases).  We would need to catch exceptions raised
by <code>openFile</code>, in order to make this complete.  We will do this
when we talk about exceptions in more detail in
the section on [[../Io advanced#Exceptions|Exceptions]].


{{Exercises|1=
Write a program that first asks whether the user wants to read from a
file, write to a file or quit.  If the user responds quit, the program
should exit.  If he responds read, the program should ask him for a
file name and print that file to the screen (if the file doesn't
exist, the program may crash).  If he responds write, it should ask
him for a file name and then ask him for text to write to the file,
with "."  signaling completion.  All but the "." should be written
to the file.

For example, running this program might produce:

<pre>
Do you want to [read] a file, [write] a file or [quit]?
read
Enter a file name to read:
foo
...contents of foo...
Do you want to [read] a file, [write] a file or [quit]?
write
Enter a file name to write:
foo
Enter text (dot on a line by itself to end):
this is some
text for
foo
.
Do you want to [read] a file, [write] a file or [quit]?
read
Enter a file name to read:
foo
this is some
text for
foo
Do you want to [read] a file, [write] a file or [quit]?
blech
I don't understand the command blech.
Do you want to [read] a file, [write] a file or [quit]?
quit
Goodbye!
</pre>
}}

== Another way of reading files ==

Sometimes (for example in parsers) we need to open a file from command line, by typing "program.exe input.in" . Of course, we could just use "< " to redirect standard input, but we can do it in more elegant way. We could do this like that:

<pre>
module Main where

import IO
import System
import List(isSuffixOf) 

parse fname str = str 

main::IO()
main = do 
    arguments <- getArgs
    if (length (arguments)==0) 
     then putStrLn "No input file given.\n Proper way of running program is: \n program input.in"
      else do
            let suffix = if List.isSuffixOf ".in" (head arguments) then "" else ".in" 
--Using this trick will allow users to type "program input" as well
            handle <- catch (openFile ((head arguments)++suffix) ReadMode) 
                (\e -> error $ show e)                                           
            readable<- hIsReadable handle
            if (not readable)
                then error "File is being used by another user or program"
                else do
                    unparsedString <- hGetContents handle
                    case parse (head arguments) unparsedString of
{-This is how it would look like, when our parser was based on Parsec
                        Left err -> error $ show err  
                        Right program -> do
                            {
                               outcome <- interprete program ;
                               case outcome of 
                                Abort -> do
                                            putStrLn "Program aborted"
                                            exitWith $ ExitFailure 1
                                _ -> do 
                                      putStrLn "Input interpreted"
                                      exitWith ExitSuccess
                            }
But to make this example less complicated I replaced these lines with:-}
                            {
                              putStrLn "Input interpreted";
                              exitWith ExitSuccess                                          
                            }
</pre>

{{Haskell navigation|chapter=Libraries Reference}}
{{Auto category}}
