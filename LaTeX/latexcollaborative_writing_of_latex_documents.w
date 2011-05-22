><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>

'''Note:''' 
This Wikibook is based on the article 
''Tools for Collaborative Writing of Scientific LaTeX Documents''
by [[User:Arnehe|Arne Henningsen]]
that is published in ''The PracTeX Journal'' 2007, number 3 
(http://www.tug.org/pracjourn/).


== Abstract ==

Collaborative writing of documents
requires a strong synchronisation among authors.
This Wikibook describes a possible way
to organise the collaborative preparation of LaTeX documents.
The presented solution is primarily based on the
version control system ''Subversion'' (http://subversion.tigris.org).
The Wikibook describes how ''Subversion'' can be used
together with several other software tools and LaTeX packages
to organise the collaborative preparation of LaTeX documents.

=== Other  Methods ===
*The online LaTeX editor [http://www.scribtex.com ScribTeX] makes sharing your document with others very easy. It provides a full LaTeX environment, with all the usual features of LaTeX like bibtex, images and custom style files. It also provides full version histories of your files, essential for collaborating. The free account allows only 3 projects and only one collaborator per project.
* [http://www.publications.li publications.li] is a real-time collaborative LaTeX editor.
* [http://www.verbosus.com Verbosus] is a professional Online LaTeX Editor that supports collaboration with other users and is free to use. Merge conflicts can easily resolved by using a built-in merge tool that uses an implementation of the diff-algorithm to generate information required for a successful merge.
* The [http://monkeytex.bradcater.webfactional.com Monkey TeX] is free and allows team sharing. 
*Another option for collaboration is [http://www.getdropbox.com dropbox]. It has 2Gb free storage and versioning system. Works like SVN, but more automated and therefore especially useful for beginning latex users.
*As the LaTeX system uses plain text, you can use synchronous collaborative editors like [[w:Gobby|Gobby]]. In Gobby you can write your documents in collaboration with anyone in real time.  It is strongly recommended that you use utf8 encoding (especially if there are users on multiple operating systems collaborating) and a stable network (typically wired networks).  
*[http://docs.google.com Google Documents] or [http://docs.latexlab.org LaTeX Lab] also allows real-time simultaneous collaborative editing of text files for anyone with a Google account (and its option to make the document available through a URL makes local download and compilation easily scriptable).
*[http://titanpad.com TitanPad] (or other [http://etherpad.org/etherpadsites.html clones] of [[w:EtherPad|EtherPad]]). To compile use the command line :<br/>& <code>wget -O filename.tex "http://titanpad.com/ep/pad/export/xxxx/latest?format=txt" && (latex filename.tex)</code><br/> where 'xxxx' should be replaced by the pad number (something like 'z7rSrfrYcH').
*You could use more modern distributed version control systems like Mercurial or Git.

== Introduction ==

The collaborative preparation of documents
requires a considerable amount of coordination among the authors.
This coordination can be organised in many different ways,
where the best way depends on the specific circumstances.

In this Wikibook, I describe 
how the collaborative writing of LaTeX documents is organised
at our department 
(Division of Agricultural Policy, Department of Agricultural Economics, University of Kiel, Germany).
I present our software tools, and describe how we use them.
Thus, this Wikibook provides some ideas and hints
that will be useful for other LaTeX users
who prepare documents together with their co-authors.


== Interchanging Documents ==

There are many ways to interchange documents among authors.
One possibility is to compose documents by interchanging e-mail messages.
This method has the advantage
that common users generally do not have to install and learn the usage of
any extra software,
because virtually all authors have an e-mail account.
Furthermore, the author who has modified the document
can easily attach the document and explain the changes by e-mail as well.
Unfortunately, there is a problem when two or more authors are working
at the same time on the same document.
So, how can authors synchronise these files?

A second possibility is to provide the document on a common file server,
which is available in most departments.
The risk of overwriting each others' modifications
can be eliminated by locking files that are currently edited.
However, generally the file server can be only accessed from within a department.
Hence, authors who are out of the building 
cannot use this method to update/commit their changes.
In this case,
they will have to use another way to overcome this problem.
So, how can authors access these files?

A third possibility is to use a version control system.
A comprehensive list of version control systems can be found at
http://en.wikipedia.org/wiki/List_of_revision_control_software
Version control systems keep track of all changes in files in a project.
If many authors modify a document at the same time,
the version control system tries to merge all modifications automatically.
However, if multiple authors have modified the same line,
the modifications cannot be merged automatically,
and the user has to resolve the conflict by deciding manually
which of the changes should be kept.
Authors can also comment their modifications
so that the co-authors can easily understand the workflow of this file.
As version control systems generally communicate over the internet
(e.g. through TCP/IP connections),
they can be used from different computers with internet connections.
A restrictive firewall policy might prevent
the version control system from connecting to the internet.
In this case, the network administrator has to be asked
to open the appropriate port.
The internet is only used for synchronising the files.
Hence, a permanent internet connection is not required.
The only drawback of a version control system could be that it has to be installed and configured.

Moreover, a version control system is useful
even if a single user is working on a project.
First, the user can track (and possibly revoke) all previous modifications.
Second, this is a convenient way to have a backup of the files
on other computers (e.g. on the version control server).
Third, this allows the user to easily switch between different computers
(e.g. office, laptop, home).

== The Version Control System ''Subversion'' ==

At our department, we decided to use the open source version control system
''Subversion'' (http://subversion.apache.org/).
This software was designed as a successor to the
popular version control system ''CVS''.
The ''Subversion'' ''(SVN)'' version control system
is based on a central ''Subversion'' server
that hosts the 'repositories'.
A Repository can be thought of as a library,
where authors keep successive revisions of one or more documents.
The version control systems acts as the librarian between
the author and the repository.
For instance, the authors can ask the librarian
to get the latest version of their projects
or to commit a new version to the librarian.
(see http://blogs.linux.ie/balor/2007/05/23/)

Each user has a local 'working copy' of (a part of)
a remote 'repository'.
For instance, users can
'update' changes from the repository to their working copy,
'commit' changes from their own working copy to the repository,
or (re)view the differences between working copy and repository.

To set up a ''Subversion'' version control system,
the ''Subversion'' '''server''' software has to be installed
on a (single) computer with permanent internet access.
(If this computer has no static IP address,
one can use a service like DynDNS (http://www.dyndns.com/)
to be able to access the server with a static hostname.)
It can run on many Unix, modern MS Windows,
and Mac OS X platforms.

Users do not have to install
the ''Subversion'' '''server''' software,
but a ''Subversion'' '''client''' software.
This is the unique way to access the 'repositories' on the server.
Besides the basic ''Subversion'' command-line client,
there are several Graphical User Interface Tools (GUIs) and plug-ins
for accessing the ''Subversion'' server
(see http://subversion.tigris.org/links.html).
Additionally, there are very good manuals about ''Subversion''
freely available on the internet
(e.g. http://svnbook.red-bean.com).

At our department, we run the ''Subversion'' server
on a ''GNU-Linux'' system,
because most ''Linux'' distributions include it.
In this sense,
installing, configuring, and maintaining ''Subversion''
is a very simple task.

Most MS Windows users access the ''Subversion'' server
by the ''TortoiseSVN'' client
(http://tortoisesvn.tigris.org/),
because it provides the most usual interface for common users.
Linux users usually use the ''Subversion'' command-line client
or ''eSvn'' GUI (http://zoneit.free.fr/esvn/)
with ''KDiff3'' (http://kdiff3.sourceforge.net/)
for showing complex differences.

== Hosting LaTeX files in ''Subversion'' ==

[[Image:ESvn-texmf.png|frame|right|Figure 1: 
Common <tt>texmf</tt> tree shown in ''eSvn'''s Repository Browser]]
On our ''Subversion'' server,
we have one repository for a common <tt>texmf</tt> tree.
Its structure complies with the
'''TeX Directory Structure''' guidelines
(TDS, http://www.tug.org/tds/tds.html,
see figure 1).
This repository provides LaTeX classes, LaTeX styles, and BibTeX styles
that are not available in the LaTeX distributions of the users,
e.g. because they were bought
or developed for the internal use at our department.
All users have a working copy of this repository
and have configured LaTeX to use this as their
personal <tt>texmf</tt> tree.
For instance, teTeX (http://www.tug.org/tetex/) users can edit their TeX configuration file
(e.g. <tt>/etc/texmf/web2c/texmf.cnf</tt>)
and set the variable <tt>TEXMFHOME</tt>
to the path of the working copy of the common <tt>texmf</tt> tree
(e.g. by <tt>TEXMFHOME = $HOME/texmf</tt>);
MiKTeX (http://www.miktex.org/) users can add
the path of the working copy of the common <tt>texmf</tt> tree
in the 'Roots' tab of the MiKTeX Options.

If a new class or style file has been added
(but not if these files have been modified),
the users have to update their 'file name data base' (FNDB)
before they can use these classes and styles.
For instance, teTeX users have to execute <tt>texhash</tt>;
MiKTeX users have to click on the button 'Refresh FNDB'
in the 'General' tab of the MiKTeX Options.

Furthermore, the repository contains manuals
explaining the specific LaTeX software solution
at our department (e.g. this document).

The ''Subversion'' server hosts a separate repository
for each project of our department.
Although branching, merging, and tagging is less important
for writing text documents than for writing source code for software,
our repository layouts follow the recommendations of 
the 'Subversion book' (http://svnbook.red-bean.com).
In this sense, each repository has the three directories
<tt>/trunk</tt>, <tt>/branches</tt>, and <tt>/tags</tt>.

The most important directory is <tt>/trunk</tt>.
If a single text document belongs to the project,
all files and subdirectories of this text document are in
<tt>/trunk</tt>.
If the project yields two or more different text documents,
<tt>/trunk</tt> contains a subdirectory for each text document.
A slightly different version (a '''branch''') of a text document
(e.g. for presentation at a conference)
can be prepared either in an additional subdirectory of <tt>/trunk</tt>
or in a new subdirectory of <tt>/branches</tt>.
When a text document is submitted to a journal or a conference,
we create a '''tag''' in the directory <tt>/tags</tt>
so that it is easy to identify the submitted version
of the document at a later date.
This feature has been proven very useful.
When creating branches and tags,
it is important always to use
the ''Subversion'' client (and not the tools of the local file system)
for these actions,
because this saves disk space on the server
and it preserves information about the same history of these documents.

Often the question arises,
which files should be put under version control.
Generally, all files that are directly modified by the user
and that are necessary for compiling the document
should be included in the version control system.
Typically, these are the LaTeX source code (<tt>*.tex</tt>) files
(the main document and possibly some subdocuments)
and all pictures that are inserted in the document
(<tt>*.eps</tt>, <tt>*.jpg</tt>, <tt>*.png</tt>, and <tt>*.pdf</tt> files).
All LaTeX classes (<tt>*.cls</tt>), LaTeX styles (<tt>*.sty</tt>),
BibTeX data bases (<tt>*.bib</tt>), and BibTeX styles (<tt>*.bst</tt>)
generally should be hosted
in the repository of the common <tt>texmf</tt> tree,
but they could be included in the respective repository,
if some (external) co-authors do not have access
to the common <tt>texmf</tt> tree.
On the other hand,
all files that are automatically created or modified during
the compilation process (e.g.
<tt>*.aut</tt>, <tt>*.aux</tt>, <tt>*.bbl</tt>,
<tt>*.bix</tt>, <tt>*.blg</tt>,
<tt>*.dvi</tt>, <tt>*.glo</tt>, <tt>*.gls</tt>, <tt>*.idx</tt>,
<tt>*.ilg</tt>, <tt>*.ind</tt>, <tt>*.ist</tt>,
<tt>*.lof</tt>, <tt>*.log</tt>, <tt>*.lot</tt>, <tt>*.nav</tt>, <tt>*.nlo</tt>,
<tt>*.out</tt>, <tt>*.pdf</tt>, <tt>*.ps</tt>,
<tt>*.snm</tt>,
and <tt>*.toc</tt> files)
or by the (LaTeX or BibTeX) editor (e.g.
<tt>*.bak</tt>, <tt>*.bib~</tt>, <tt>*.kilepr</tt>, <tt>*.prj</tt>,
<tt>*.sav</tt>, <tt>*.tcp</tt>, <tt>*.tmp</tt>, <tt>*.tps</tt>,
and <tt>*.tex~</tt> files)
generally should be '''not''' under version control,
because these files are not necessary for compilation
and generally do not include additional information.
Furthermore, these files are regularly modified
so that conflicts are very likely.

== ''Subversion'' really makes the '''diff'''erence ==

A great feature of a version control system is
that all authors can easily trace the workflow of a project
by viewing the differences between arbitrary versions of the files.
Authors are primarily interested in 'effective' modifications
of the source code
that change the compiled document,
but not in 'ineffective' modifications
that have no impact on the compiled document
(e.g. the position of line breaks).
Software tools for comparing text documents ('diff tools')
generally cannot differentiate
between 'effective' and 'ineffective' modifications;
they highlight both types of modifications.
This considerably increases the effort
to find and review the 'effective' modifications.
Therefore, 'ineffective' modifications should be avoided.

In this sense, it is very important
not to change the positions of line breaks without cause.
Hence, automatic line wrapping of the users' LaTeX editors
should be turned off
and line breaks should be added manually.
Otherwise, if a single word in the beginning of a paragraph is added
or removed,
all line breaks of this paragraph might change
so that most diff tools indicate the entire paragraph as modified,
because they compare the files line by line.
The diff tools ''wdiff'' (http://www.gnu.org/software/wdiff/)
and ''dwdiff'' (http://os.ghalkes.nl/dwdiff.html)
are not affected by the positions of line breaks,
because they compare documents word by word.
However, their output is less clear
so that modifications are more difficult to track.
Moreover, these tools cannot be used directly with the ''Subversion''
command-line switch <tt>--diff-cmd</tt>,
but a small wrapper script has to be used
(http://textsnippets.com/posts/show/1033).

A reasonable convention is to add a line break after each sentence
and start each new sentence in a new line.
Note that this has an advantage also beyond version control:
if you want to find a sentence in your LaTeX code
that you have seen in a compiled (DVI, PS, or PDF) file
or on a printout,
you can easily identify the first few words of this sentence
and screen for these words on the left border of your editor window.

Furthermore,
we split long sentences into several lines
so that each line has at most 80 characters,
because it is rather inconvenient to search for (small) differences
in long lines.
(Note:
For instance, the LaTeX editor ''Kile''
(http://kile.sourceforge.net/)
can assist the user in this task
when it is configured to add a vertical line
that marks the 80th column.)
We find it very useful to introduce
the additional line breaks at logical breaks of the sentence,
e.g. before a relative clause
or a new part of the sentence starts.
An example LaTeX code
that is formatted according to these guidelines
is the source code of the article 
''Tools for Collaborative Writing of Scientific LaTeX Documents''
by [[User:Arnehe|Arne Henningsen]]
that is published (including the source code) 
in ''The PracTeX Journal'' 2007, Number 3 (http://www.tug.org/pracjourn/2007-3/henningsen/).

If the authors work on different operating systems,
their LaTeX editors will probably save the files with different newline (end-of-line) characters
(http://en.wikipedia.org/wiki/Newline).
To avoid this type of 'ineffective' modifications,
all users can agree on a specific newline character
and configure their editor to use this newline character.
Another alternative is to add the subversion property 'svn:eol-style' and set it to 'native'.
In this case, ''Subversion'' automatically converts all newline characters of this file 
to the native newline character of the author's operating system
(http://svnbook.red-bean.com/en/1.4/svn.advanced.props.file-portability.html#svn.advanced.props.special.eol-style). 

There is also another important reason
for reducing the number of 'ineffective' modifications:
if several authors work on the same file,
the probability that the same line is modified by two or more authors
at the same time increases with the number of modified lines.
Hence, 'ineffective' modifications unnecessarily increase
the risk of conflicts (see section 
[[LaTeX/Collaborative Writing of LaTeX Documents#Interchanging Documents|Interchanging Documents]]).

[[Image:Kdiff3-modification.png|frame|right|Figure 2: Reviewing modifications in ''KDiff3'']]
Furthermore, version control systems
allow a very effective quality assurance measure:
all authors should critically review their own modifications
before they commit them to the repository
(see figure 2).
The differences between the user's working copy and the repository
can be easily inspected with a single ''Subversion'' command
or with one or two clicks in a graphical ''Subversion'' client.
Furthermore, authors should verify
that their code can be compiled flawlessly
before they commit their modifications to the repository.
Otherwise, the co-authors have to pay for these mistakes
when they want to compile the document.
However, this directive is not only reasonable for version control systems
but also for all other ways to interchange documents among authors.

''Subversion'' has a feature called 'Keyword Substitution'
that includes dynamic version information about a file
(e.g. the revision number or the last author)
into the contents of the file itself
(see e.g. http://svnbook.red-bean.com, chapter 3).
Sometimes, it is useful to include these information
not only as a comment in the LaTeX source code,
but also in the (compiled) DVI, PS, or PDF document.
This can be achieved with the LaTeX packages
''svn'' (http://www.ctan.org/tex-archive/macros/latex/contrib/svn/),
''svninfo'' (http://www.ctan.org/tex-archive/macros/latex/contrib/svninfo/),
or (preferably) ''svn-multi'' (http://www.ctan.org/tex-archive/macros/latex/contrib/svn-multi/).

The most important directives for collaborative writing of LaTeX documents
with version control systems are summarised in the following box.

<div style="border: 1px solid black; margin: 0.5em; padding:0.5em;">
'''Directives for using LaTeX with version control systems'''
# Avoid 'ineffective' modifications.
# Do not change line breaks without good reason.
# Turn off automatic line wrapping of your LaTeX editor.
# Start each new sentence in a new line.
# Split long sentences into several lines so that each line has at most 80 characters.
# Put only those files under version control that are directly modified by the user.
# Verify that your code can be compiled flawlessly before committing your modifications to the repository.
# Use ''Subversion'''s diff feature to critically review your modifications before committing them to the repository.
# Add a meaningful and descriptive comment when committing your modifications to the repository.
# Use the ''Subversion'' client for copying, moving, or renaming files and folders that are under revision control.
</div>

If the users are willing to let go of the built-in ''diff'' utility of SVN and use ''diff'' tools that are local on their workstations, they can put to use such tools that are more tailored to text documents. The ''diff'' tool that comes with SVN was designed with source code in mind. As such, it is built to be more useful for files of short lines. Other tools, such as '''Compare It!''' allows to conveniently compare text files where each line can span hundreds of characters (such as when each line represents a paragraph). When using a ''diff'' tool that allows convenient views of files with long lines, the users can author the TeX files without a strict line-breaking policy.

== Managing collaborative bibliographies ==

Writing of scientific articles, reports, and books requires
the citation of all relevant sources.
BibTeX is an excellent tool for citing references and creating bibliographies
(Markey 2005, Fenn 2006).
Many different BibTeX styles can be found
on CTAN (http://www.ctan.org)
and on the LaTeX Bibliography Styles Database
(http://jo.irisson.free.fr/bstdatabase/).
If no suitable BibTeX style can be found,
most desired styles can be conveniently assembled with
''custombib''/''makebst'' 
(http://www.ctan.org/tex-archive/macros/latex/contrib/custom-bib/).
Furthermore, BibTeX style files can be created or modified manually;
however this action requires knowledge of the (unnamed) postfix stack language
that is used in BibTeX style files
(Patashnik 1988).

At our department, we have a common bibliographic data base
in the BibTeX format (.bib file).
It resides in our common <tt>texmf</tt> tree
(see section 'Hosting LaTeX files in ''Subversion''')
in the subdirectory <tt>/bibtex/bib/</tt>
(see figure 1).
Hence, all users can specify this bibliography by only using the file name 
(without the full path) ---
no matter where the user's working copy of the common <tt>texmf</tt> tree
is located.

All users edit our bibliographic data base
with the graphical BibTeX editor ''JabRef''
(http://jabref.sourceforge.net/).
As ''JabRef'' is written in ''Java'',
it runs on all major operating systems.
As different versions of ''JabRef'' generally
save files in a slightly different way
(e.g. by introducing line breaks at different positions),
all users should use the same (e.g. last stable) version of ''JabRef''.
Otherwise, there would be many differences
between different versions of <tt>.bib</tt> files
that solely originate from using different version of ''JabRef''.
Hence, it would be hard to find the real differences
between the compared documents.
Furthermore, the probability of conflicts would be much higher
(see section 'Subversion really makes the difference').
As ''JabRef'' saves the BibTeX data base with the native newline character 
of the author's operating system,
it is recommended to add the ''Subversion'' property 'svn:eol-style' and set it to 'native'
(see section 'Subversion really makes the difference').

[[Image:JabRef-KeyPattern.png|frame|right|Figure 3:
Specify default key pattern in ''JabRef'']]
''JabRef'' is highly flexible
and can be configured in many details.
We make the following changes to the default configuration
of ''JabRef'' to simplify our work.
First, we specify the default pattern for BibTeX keys
so that ''JabRef'' can automatically generate keys
in our desired format.
This can be done by selecting
<tt>Options</tt>
&rarr; <tt>Preferences</tt>
&rarr; <tt>Key pattern</tt>
and modifying the desired pattern in the field
<tt>Default pattern</tt>.
For instance, we use <tt>[auth:lower][shortyear]</tt>
to get the last name of the first author in lower case
and the last two digits of the year of the publication
(see figure 3).

[[Image:JabRef-GeneralFields.png|frame|right|Figure 4:
Set up general fields in ''JabRef'']]
Second, we add the BibTeX field <tt>location</tt> for information
about the location,
where the publication is available as hard copy
(e.g. a book or a copy of an article).
This field can contain the name of the user who has the hard copy
and where he has it or the name of a library and the shelf-mark.
This field can be added in ''JabRef'' by selecting
<tt>Options</tt>
&rarr; <tt>Set up general fields</tt>
and adding the word <tt>location</tt>
(using the semicolon (<tt>;</tt>) as delimiter)
somewhere in the line that starts with <tt>General:</tt>
(see figure 4).

[[Image:JabRef-ExternalPrograms.png|frame|right|Figure 5:
Specify 'Main PDF directory' in ''JabRef'']]
Third, we put all PDF files of publications in a specific
subdirectory in our file server,
where we use the BibTeX key as file name.
We inform ''JabRef'' about this subdirectory
by selecting
<tt>Options</tt>
&rarr; <tt>Preferences</tt>
&rarr; <tt>External programs</tt>
and adding the path of the this subdirectory
in the field <tt>Main PDF directory</tt>
(see figure 5).
If a PDF file of a publication is available,
the user can push the <tt>Auto</tt> button
left of ''JabRef'''s <tt>Pdf</tt> field
to automatically add the file name of the PDF file.
Now, all users who have access to the file server
can open the PDF file of a publication
by simply clicking on ''JabRef'''s PDF icon.

If we send the LaTeX source code of a project
to a journal, publisher, or somebody else
who has no access to our common <tt>texmf</tt> tree,
we do not include our entire bibliographic data base,
but extract the relevant entries with
the Perl script ''aux2bib'' (http://www.ctan.org/tex-archive/biblio/bibtex/utils/bibtools/aux2bib).

==Conclusion==

This wikibook describes a possible way
to efficiently organise the collaborative preparation
of LaTeX documents.
The presented solution is based on the
''Subversion'' version control system
and several other software tools and LaTeX packages.
However, there are still a few issues
that can be improved.

First, we plan that all users install
the same LaTeX distribution.
As the ''TeX Live'' distribution (http://www.tug.org/texlive/)
is available both for Unix and MS Windows operating systems,
we might recommend our users to switch to this LaTeX distribution
in the future.
(Currently, our users have different LaTeX distributions
that provide a different selection of LaTeX packages
and different versions of some packages.
We solve this problem by providing some packages on
our common <tt>texmf</tt> tree.)

Second, we consider to simplify
the solution for a common bibliographic data base.
Currently it is based on the
version control system ''Subversion'',
the graphical BibTeX editor ''JabRef'',
and a file server for the PDF files of publications in the data base.
The usage of three different tools for one task
is rather challenging for infrequent users
and users that are not familiar with these tools.
Furthermore, the file server can be only accessed by local users.
Therefore, we consider to implement an integrated server solution
like ''WIKINDX'' (http://wikindx.sourceforge.net/),
''Aigaion'' (http://www.aigaion.nl/),
or ''refBASE'' (http://refbase.sourceforge.net/).
Using this solution only requires a computer with internet access
and a web browser,
which makes the usage of our data base considerably easier
for infrequent users.
Moreover, the stored PDF files are available
not only from within the department,
but throughout the world.
(Depending on the copy rights of the stored PDF files,
the access to the server
--- or least the access to the PDF files ---
has to be restricted to members of the department.)
Even Non-LaTeX users of our department might benefit
from a server-based solution,
because it should be easier to use this bibliographic data base
in (other) word processing software packages,
because these servers provide the data not only in BibTeX format,
but also in other formats.

All readers are encouraged to contribute to this wikibook
by adding further hints or ideas
or by providing further solutions to the problem
of collaborative writing of LaTeX documents.


==Acknowledgements==

Arne Henningsen thanks Francisco Reinaldo and Géraldine Henningsen
for comments and suggestions
that helped him to improve and clarify this paper,
Karsten Heymann for many hints and advices regarding LaTeX, BibTeX,
and ''Subversion'',
and Christian Henning as well as his colleagues
for supporting his intention to establish LaTeX
and ''Subversion'' at their department.


== References ==
*Fenn, Jürgen (2006): Managing citations and your bibliography with BibTeX. The PracTEX Journal, 4. http://www.tug.org/pracjourn/2006-4/fenn/.

*Markey, Nicolas (2005): Tame the BeaST. The B to X of BibTeX. http://www.ctan.org/tex-archive/info/bibtex/tamethebeast/ttb_en.pdf. Version 1.3.

*Oren Patashnik. Designing BibTeX styles. http://www.ctan.org/tex-archive/info/biblio/bibtex/contrib/doc/btxhak.pdf.

*[http://mathoverflow.net/questions/3044/tools-for-collaborative-paper-writing Tools for collaborative paper-writing]


<noinclude>
{{LaTeX/Bottom|Multiple files|Tips and Tricks}}
</noinclude>
