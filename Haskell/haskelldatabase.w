>{{Haskell minitoc|chapter=Specialised Tasks}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=Specialised Tasks}}

== Introduction ==
Haskell's most popular database module is [http://github.com/jgoerzen/hdbc/wiki HDBC]. HDBC provides an abstraction layer between Haskell programs and SQL relational databases. This lets you write database code once, in Haskell, and have it work with a number of backend SQL databases. 

HDBC is modeled loosely on [http://search.cpan.org/~timb/DBI/DBI.pm Perl's DBI interface], though it has also been influenced by Python's DB-API v2, JDBC in Java, and HSQL in Haskell. As DBI requires DBD in Perl, HDBC requires a driver module beneath it to work. These HDBC backend drivers exist: PostgreSQL, 
SQLite, and ODBC (for Windows and Unix/Linux/Mac). MySQL is the most popular open-sourced database, and there are two drivers for MySQL: [http://hackage.haskell.org/package/HDBC-mysql HDBC-mysql] (native) and [http://hackage.haskell.org/package/HDBC-odbc HDBC-odbc] (ODBC). MySQL users can use the ODBC driver on any MySQL-supported platform, including Linux. An advantage of using ODBC is that the syntax of the SQL statement is insulated from the different kinds of database engines. This increases the portability of the application should you have to move from one database to another. The same argument for preferring ODBC applies for other commercial databases, such as Oracle and DB2.

== Installation ==

=== SQLite ===
See [http://github.com/jgoerzen/hdbc/wiki/FrequentlyAskedQuestions here] for more information.

=== PostgreSQL ===
See [http://github.com/jgoerzen/hdbc/wiki/FrequentlyAskedQuestions here] for more information.

=== Native MySQL ===

The native ODBC-mysql library requires the C MySQL client library to be present.

You may need to [http://www.serpentine.com/blog/2010/09/04/dealing-with-fragile-c-libraries-e-g-mysql-from-haskell/ wrap your database accesses] to prevent runtime errors.

=== ODBC/MySQL ===
Instruction how to install ODBC/MySQL. It is somewhat involved to make HDBC work with MySQL via ODBC, especially if you do not have root privilege and have to install everything in a non-root account.

* If your platform doesn't already provide an ODBC library (and most do), install Unix-ODBC. See [http://sourceforge.net/projects/unixodbc/ here] for more information.
* Install MySQL-ODBC Connector. See [http://dev.mysql.com/downloads/connector/odbc/ here] for more information.
* Install Database.HDBC module
* Install Database.HDBC.ODBC module
* Add the mysql driver to odbcinst.ini file (under $ODBC_HOME/etc/) and your data source in $HOME/.odbc.ini.
* Create a test program

Since the ODBC driver is installed using shared library by default, you will need the following env:

 export LD_LIBRARY_PATH=$ODBC_HOME/lib

If you do not like adding an additional env variables, you should try to compile ODBC with static library option enabled.

The next task is to write a simple test program that connects to the database and print the names of all your tables, as shown below.

You may need to [http://www.serpentine.com/blog/2010/09/04/dealing-with-fragile-c-libraries-e-g-mysql-from-haskell/ wrap your database accesses] in order to prevent runtime errors.

  module Main where
  import Database.HDBC.ODBC
  import Database.HDBC
  main =
    do c  <- connectODBC "DSN=PSPDSN"
       xs <- getTables c
       putStr $ "tables "++(foldr jn "." xs)++"\n"
    where jn a b = a++" "++b

== General Workflow ==
=== Connect and Disconnect ===
The first step of any database operation is to connect to the target database. This is done via the driver-specific connect API, which has the type of:

 String -> IO Connection

Given a connect string, the connect API will return <code>Connection</code> and put you in the IO monad.

Although most program will garbage collect your connections when they are out of scope or when the program ends, it is a good practice to disconnect from the database explicitly.

  conn->Disconnect

=== Running Queries ===
Running a query generally involves the following steps:
* Prepare a statement
* Execute a statement with bind variables
* Fetch the result set (if any)
* Finish the statement

HDBC provides two ways for bind variables and returning result set: <code>[ SqlValue ]</code> and <code>[ Maybe String ]</code>. You need to use the functions with '''s''' prefix when using <code>[ Maybe String ]</code>, instead of <code>[ SqlValue ]</code>. <code>[ SqlValue ]</code> allows you to use strongly typed data if type safety is very important in your application; otherwise, <code>[ Maybe String ]</code> is more handy when dealing with lots of database queries. When you use <code>[ Maybe String ]</code>, you assume the database driver will perform automatic data conversion. Be aware there is a performance price for this convenience.

Sometimes, when the query is simple, there are simplified APIs that wrap multiple steps into one. For example, '''Run''' and '''sRun''' are wrappers of "prepare and execute". '''quickQuery''' is a wrapper of "prepare, execute, and fetch all rows".

== Running SQL Statements ==

=== Select ===

=== Insert ===

=== Update ===

=== Delete ===

== Transaction ==
Database transaction is controlled by <code>commit</code> and <code>rollback</code>. However, be aware some databases (such as mysql) do not support transaction. Therefore, every query is in its atomic transaction.

HDBC provides <code>withTransaction</code> to allow you automate the transaction control over a group of queries.

== Calling Procedure ==

{{Haskell navigation|chapter=Specialised Tasks}}
