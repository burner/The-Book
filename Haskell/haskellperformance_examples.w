>{{Haskell minitoc|chapter=Haskell Performance}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=Haskell Performance}}

Goal: Explain optimizations step by step with examples that actually happened.

== Tight loop ==
[http://cgi.cse.unsw.edu.au/~dons/blog/2008/05/16#fast dons: Write Haskell as fast as C: exploiting strictness, laziness and recursion.]

== CSV Parsing ==
[http://thread.gmane.org/gmane.comp.lang.haskell.cafe/40114 haskell-cafe: another Newbie performance question]
I hope he doesn't mind if I post his code here, I still have to ask him. -- [[User:Apfelmus|apfe<b>&lambda;</b>mus]] 08:46, 18 May 2008 (UTC)
<pre>
type CSV = [[String]]

main = do
                  args <- getArgs
                  file <- readFile (head args)
                  writeFile (head args ++ "2") (processFile (args !! 1) file)

processFile s     = writeCSV . doInteraction s . readCSV
doInteraction line csv = insertLine (show line) (length csv - 1) csv
writeCSV          = (\x -> x ++ "\n") . concat . intersperse "\n" . (map (concat . intersperse "," . (map show)))
insertLine line pos csv = (take pos csv) ++ [readCSVLine line] ++ drop pos csv
readCSVLine       = read . (\x -> "["++x++"]")
readCSV           = map readCSVLine . lines
</pre>

I think there was another cvs parsing thread on the mailing list which I deemed appropriate, but I can't remember.

== Space Leak ==

jkff asked about some code in #haskell which was analyzing a logfile. Basically, it was building a histogram

<pre>
foldl' (\m (x,y) -> insertWith' x (\[y] ys -> y:ys) [y] m) M.empty
  [(ByteString.copy foo, ByteString.copy bar) | (foo,bar) <- map (match regex) lines]
</pre>

The input was a 1GB logfile and the program blew the available memory mainly because the <code>ByteString.copy</code> weren't forced and the whole file lingered around in memory.

{{Haskell navigation | chapter = Haskell Performance}}
