#!/usr/bin/python

import re;
import inspect
import sys
import unittest

def __line__ ():
    caller = inspect.stack()[1]
    return int (caller[2])

class Test(unittest.TestCase):
	def test_level6(self):
		self.assertEqual("\\subparagraph{test} \\label{test}", processLevel("======test======", None))
		self.assertEqual("\\subparagraph{Hello World} \\label{Hello World}", 
			processLevel("======Hello World======", None))
		self.assertEqual("\\subparagraph{ Hello World} \\label{ Hello World}", 
			processLevel("====== Hello World======", None))
		self.assertNotEqual('\\subparagraph{test} \\label{test}', processLevel(" ======test======", None))

	def test_level5(self):
		self.assertEqual("\\paragraph{ Hello World } \\label{ Hello World }", 
			processLevel("===== Hello World =====", None))
		self.assertNotEqual('\\paragraph{test} \\label{test}', processLevel(" =====test=====", None))

	def test_level4(self):
		self.assertEqual("\\subsubsection{ Hello World } \\label{ Hello World }", 
			processLevel("==== Hello World ====", None))
		self.assertNotEqual('\\subsubsection{test} \\label{test}', processLevel(" ====test====", None))

	def test_level3(self):
		self.assertEqual("\\subsection{ Hello World } \\label{ Hello World }", 
			processLevel("=== Hello World ===", None))
		self.assertNotEqual('\\subsection{test} \\label{test}', processLevel(" ====test====", None))

	def test_level2(self):
		self.assertEqual("\\section{ Hello World } \\label{ Hello World }", 
			processLevel("== Hello World ==", None))
		self.assertEqual("\\section{ Hello World ? 888 } \\label{ Hello World ? 888 }", 
			processLevel("== Hello World ? 888 ==", None))
		self.assertNotEqual('\\section{test} \\label{test}', processLevel(" ====test====", None))
	

	def test_processFormat(self):
		self.assertEqual(r"hello", processFormat("<nowiki>hello</nowiki>", None))
		self.assertEqual(r"\textbf{hello =}", processFormat("'''hello ='''", None))
		self.assertEqual(r"\emph{hello =}", processFormat("''hello =''", None))
		self.assertEqual(r"\emph{\textbf{hello =}}", processFormat("'''''hello ='''''", None))

	def test_hline(self):
		self.assertEqual("\\hline", processHLine("----", None))
		self.assertNotEqual("\\hline", processHLine(" ----", None))

	def test_numberOfTabs(self):
		self.assertEqual(2, numberOfTabs("\t\tsafasd"))
		self.assertEqual(0, numberOfTabs(None))
		self.assertEqual(0, numberOfTabs(""))

def replaceHtmlTags(line):
	underline = re.sub("<ins>(?<name>[\d\w\s\S]+</ins>", "\\uline{\g<name>}", line)
	underline2= re.sub("<span style=\"text-decoration: uline;ins\">(?<name>[\d\w\s\S]+</span>", 
		"\\underline{\g<name>}", underline)
	delete = re.sub("<del>(?<name>[\d\w\s\S]+</del>", "\\sout{\g<name>}", line)
	underline2 = re.sub("<span style=\"text-decoration: underline;ins\">(?<name>[\d\w\s\S]+</span>", 
		"\\sout{\g<name>}", underline)
	tt = re.sub("<tt>(?<name>[\d\w\s\S]+</tt>", 
		"\\texttt{\g<name>}", underline2)
	blockquote = re.sub("<blockquote>(?<name>[\d\w\s\S]+</blockquote>", 
		"\\begin{quote}\g<name>\\end{quote}", tt)


class Output():
	def __init__(self):
		self.lines = []

	def append(self, toAdd):
		self.lines.append(toAdd)
	
	def peek(self):
		if(len(self.lines) > 0):
			return self.lines[-1]
		else:
			return None

def numberOfTabs(line):
	if(line == None or len(line) == 0):
		return 0

	cnt = 0
	while(line[cnt] == '\t'):
		cnt+=1

	return cnt

def processItemize(Input, Output):
	return	

def processFormat(line, output):
	nowikisave = re.findall("<nowiki>['=\d\w\s\S]+</nowiki>", line)
	afterwikireplace = re.sub("<nowiki>[\d\w\s\S]+</nowiki>", "noZZWiki", line)

	bolditalic = re.sub("'''''(?P<name>[\d\w\s\S]+)'''''", r"\\emph{\\textbf{\g<name>}}",afterwikireplace , 0)
	bold = re.sub("'''(?P<name>[\d\w\s\S]+)'''", r"\\textbf{\g<name>}",bolditalic , 0)
	italic = re.sub("''(?P<name>[\d\w\s\S]+)''", r"\\emph{\g<name>}",bold , 0)

	nowiki = italic
	for it in nowikisave:
		tmp = re.sub(r"noZZWiki", it[8:len(it)-9], nowiki)
		nowiki = tmp

	return nowiki

def processHLine(line, output):
	hline = re.sub("\A----", "\\hline",line , 1)
	return hline

def processLevel(line, output):
	level6 = re.sub("\A======(?P<name>[\d\w\s\S]+)======", "\\subparagraph{\g<name>} \\label{\g<name>}",line , 1)
	if(line != level6):
		return (level6)

	level5 = re.sub("\A=====(?P<name>[\d\w\s\S]+)=====", "\\paragraph{\g<name>} \\label{\g<name>}",line , 1)
	if(line != level5):
		return (level5)

	level4 = re.sub("\A====(?P<name>[\d\w\s\S]+)====", "\\subsubsection{\g<name>} \\label{\g<name>}",line , 1)
	if(line != level4):
		return (level4)

	level3 = re.sub("\A===(?P<name>[\d\w\s\S]+)===", "\\subsection{\g<name>} \\label{\g<name>}",line , 1)
	if(line != level3):
		return (level3)

	level2 = re.sub("\A==(?P<name>[\d\w\s\S]+)==", "\\section{\g<name>} \\label{\g<name>}",line , 1)
	if(line != level2):
		return (level2)

def main():
	if(len(sys.argv) == 2):
		ifile = open(sys.argv[1] + ".w", "r")
		ofile = open(sys.argv[1] + ".tex", "w")
	else:
		print("Two few arguments")
		return

	out = Output()

if __name__ == "__main__":
	unittest.main()
	main()
