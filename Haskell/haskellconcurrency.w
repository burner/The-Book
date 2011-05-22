>﻿{{Haskell minitoc|chapter=Advanced Haskell}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">﻿{{Haskell minitoc|chapter=Advanced Haskell}}

== Concurrency ==

Concurrency in Haskell is mostly done with Haskell threads. Haskell threads are user-space threads that are implemented in the runtime. Haskell threads are much more efficient in terms of both time and space than Operating System threads. Apart from traditional synchronization primitives like semaphores, Haskell offers Software Transactional Memory which greatly simplifies concurrent access to shared memory.

The modules for concurrency are Control.Concurrent.* and Control.Monad.STM.

== When do you need it? ==

Perhaps more important than '''when''' is '''when not'''.  Concurrency in Haskell is not used to utilize multiple processor cores; you need another thing, "parallelism", for that.  Instead, concurrency is used for when a single core must divide its attention between various things, typically IO.

For example, consider a simple "static" webserver (i.e. serves only static content such as images).  Ideally, such a webserver should consume few processing resources; instead, it must be able to transfer data as fast as possible.  The bottleneck should be I/O, where you can throw more hardware at the problem.  So you must be able to efficiently utilize a single processor core among several connections.

In a C version of such a webserver, you'd use a big loop centered around <code>select()</code> on each connection and on the listening socket.  Each open connection would have an attached data structure specifying the state of that connection (i.e. receiving the HTTP header, parsing it, sending the file).  Such a big loop would be difficult and error-prone to code by hand.  However, using Concurrent Haskell, you would be able to write a much smaller loop concentrating solely on the listening socket, which would spawn a new "thread" for each accepted connection.  You can then write a new "thread" in the IO monad which, in sequence, receives the HTTP header, parses it, and sends the file.

Internally, the Haskell compiler will then convert the spawning of the thread to an allocation of a small structure specifying the state of the "thread", congruent to the data structure you would have defined in C.  It will then convert the various threads into a single big loop.  Thus, while you write as if each thread is independent, internally the compiler will convert it to a big loop centered around <code>select()</code> or whatever alternative is best on your system.

== Example ==

{{HaskellExample|Downloading files in parallel|<pre>
downloadFile :: URL -> IO ()
downloadFile = undefined

downloadFiles :: [URL] -> IO ()
downloadFiles = mapM_ (forkIO . downloadFile)</pre>
}}

== Software Transactional Memory ==

Software Transactional Memory (STM) is a mechanism that allows transactions on memory similar to database transactions. It greatly simplifies access to shared resources when programming in a multithreaded environment. By using STM, you no longer have to rely on locking.

To use STM, you have to include Control.Monad.STM. To change into the STM-Monad the atomically function is used. STM offers different primitives (TVar, TMVar, TChan and TArray) that can be used for communication.

The following example shows how to use a TChan to communicate between two threads. The channel is created in the main function and handed over to the reader/writerThread functions. The readerThread waits on the TChan for new input and prints it. The writerThread writes some Int-values to the channel and terminates.

{{HaskellExample|Communication with a TChan|<pre>
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan

oneSecond = 1000000

writerThread :: TChan Int -> IO ()
writerThread chan = do
        atomically $ writeTChan chan 1
        threadDelay oneSecond
        atomically $ writeTChan chan 2
        threadDelay oneSecond
        atomically $ writeTChan chan 3
        threadDelay oneSecond

readerThread :: TChan Int -> IO ()
readerThread chan = do
        newInt <- atomically $ readTChan chan
        putStrLn $ "read new value: " ++ show newInt
        readerThread chan

main = do
        chan <- atomically $ newTChan
        forkIO $ readerThread chan
        forkIO $ writerThread chan
        threadDelay $ 5 * oneSecond
</pre>}}

{{Haskell navigation|chapter=Advanced Haskell}}

[[Category:Haskell|{{SUBPAGENAME}}]]
