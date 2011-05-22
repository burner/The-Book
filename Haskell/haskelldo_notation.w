>{{Haskell minitoc|chapter=Monads}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=Monads}}

The <code>do</code> notation is a different way of writing monadic code; it is especially useful with the <code>IO</code> monad, since that monad does not allow to extract pure values from it by design; in contrast, you can extract pure values from <code>Maybe</code> or lists using pattern matching or appropriate functions.

== Translating the ''then'' operator ==

The <code>(>>)</code> (''then'') operator is easy to translate between <code>do</code> notation and plain code, so we will see it first. For example, suppose we have a chain of monads like the following one:
<source lang="haskell">
putStr "Hello" >> 
putStr " " >> 
putStr "world!" >> 
putStr "\n"
</source>

We can rewrite it in <code>do</code> notation as follows:
<source lang="haskell">
do putStr "Hello"
   putStr " "
   putStr "world!"
   putStr "\n"
</source>

This sequence of instructions is very similar to what you would see in any imperative language such as C.

Since the <code>do</code> notation is used especially with input-output, monads are often called ''actions'' in this context; an action could be writing to a file, opening a network connection or asking the user for input. The general way we translate these actions from the <code>do</code> notation to standard Haskell code is:
<source lang="haskell">
do action
   other_action
   yet_another_action
</source>

which becomes
<source lang="haskell">
action >>
do other_action
   yet_another_action
</source>

and so on until the <code>do</code> block is empty.

== Translating the ''bind'' operator ==

The <code>(>>=)</code> is a bit more difficult to translate from and to <code>do</code> notation, essentially because it involves passing a value downstream in the binding sequence. These values can be stored using the <code><-</code> notation, and used downstream in the <code>do</code> block.
<source lang="haskell">
do result         <- action
   another_result <- another_action
   (action_based_on_previous_results result another_result)
</source>

This is translated back into monadic code substituting:
<source lang="haskell">
action >>= f
where f result = do another_result <- another_action
                    (action_based_on_previous_results result another_result)
      f _      = fail "..."
</source>

In words, the action brought outside of the <code>do</code> block is bound to a function, which is defined to take an argument (to make it easy to identify it, we named <code>result</code> just like in the complete <code>do</code> block). If the pattern matching is unsuccessful, the monad's implementation of <code>fail</code> will be called.

Notice that the variables left of the <code><-</code> in the <code>do</code> block have been extracted from the monad, so if <code>action</code> produces e.g. a <code>IO String</code>, the type of <code>result</code> will be <code>String</code>.

== Example: user-interactive program ==

Consider this simple program that asks the user for his or her first and last names:

<source lang="haskell">
nameDo :: IO ()
nameDo = do putStr "What is your first name? "
            first <- getLine
            putStr "And your last name? "
            last <- getLine
            let full = first++" "++last
            putStrLn ("Pleased to meet you, "++full++"!")
</source>

The code in <code>do</code> notation is quite readable, and it is easy to see where it is going to. The <code><-</code> notation makes it possible to store first and last names as if they were pure variables, though they never can be in reality: function <code>getLine</code> is not pure because it can give a different result every time it is run (in fact, it would be of very little help if it did not).

If we were to translate the code into standard monadic code, the result would be:
<source lang="haskell">
name :: IO ()
name = putStr "What is your first name? " >>
       getLine >>= f
       where
       f first = putStr "And your last name? " >>
                 getLine >>= g
                 where
                 g last = putStrLn ("Pleased to meet you, "++full++"!")
                          where
                          full = first++" "++last
</source>

The advantage of the <code>do</code> notation should now be apparent: the code in <code>nameDo</code> is much more readable, and does not run off the right edge of the screen. 

The indentation increase is mainly caused by <code>where</code> clauses related to <code>(>>=)</code> operators, and by the fact that we cannot simply extract a value from the <code>IO</code> monad but must define new functions instead, and take advantage of pattern matching. This explains why the <code>do</code> notation is so popular when dealing with the <code>IO</code> monad, which is often used to obtain values (user input, reading files, etc.) that cannot, by construction, be taken out of the monad.

To avoid the indentation increase and mimic the do-notation closely, you could also use lamdbas (anonymous functions) like so (compare this version with the original do-version):
<source lang="haskell">
nameLambda :: IO ()
nameLambda = putStr "What is your first name? " >>
             getLine >>=
             \firstName -> putStr "And your last name? " >>
             getLine >>=
             \lastName -> let full = firstName++" "++lastName
                          in  putStrLn ("Pleased to meet you, "++full++"!")
</source> 

== Returning values ==

The last statement in a <code>do</code> notation is the result of the <code>do</code> block. In the previous example, the result was of the type <code>IO ()</code>, that is an empty value in the <code>IO</code> monad.

Suppose that we want to rewrite the example, but returning a <code>IO String</code> with the acquired name. All we need to do is add a <code>return</code> instruction:

<source lang="haskell">
nameReturn :: IO String
nameReturn = do putStr "What is your first name? "
                first <- getLine
                putStr "And your last name? "
                last <- getLine
                let full = first++" "++last
                putStrLn ("Pleased to meet you, "++full++"!")
                return full
</source>

This example will "return" the full name as a string inside the <code>IO</code> monad, which can then be utilized downstream. This kind of code is probably the reason it is so easy to misunderstand the nature of <code>return</code>: it does not only share a name with C's keyword, it ''seems'' to have the same function here.

However, check this code now:
<source lang="haskell">
nameReturn' = do putStr "What is your first name? "
                 first <- getLine
                 putStr "And your last name? "
                 last <- getLine
                 let full = first++" "++last
                 putStrLn ("Pleased to meet you, "++full++"!")
                 return full
                 putStrLn "I am not finished yet!"
</source>

The last string ''will'' be printed out, meaning that a <code>return</code> is not a final statement interrupting the flow, as it is in C and other languages. Indeed, the type of <code>nameReturn'</code> is <code>IO ()</code>, meaning that the <code>IO String</code> created by the <code>return full</code> instruction has been completely removed: the result of the <code>do</code> block is now the result of the final <code>putStrLn</code> action, which is exactly <code>IO ()</code>.


{{Haskell navigation|chapter=Monads}}

[[Category:Haskell|The do Notation]]
