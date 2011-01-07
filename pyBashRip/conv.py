import os
import re

bold = re.compile("\'\'\'( [^}]* )\'\'\'", re.VERBOSE)
#ital = re.compile("''[a-zA-Z]*''")

def shortenString(line, ofile):
	if len(line) <= 75:
		ofile.write(line)
		return
		
	output = ""	
	outputtmp = ""
	words = line.split(" ")
	for it in words:
		outputtmp += it + " "
		output += it + " "
		if len(outputtmp) >= 70:
			ofile.write(output+"\n")
			outputtmp = ""
			output = ""

	ofile.write(output)

def form(line):
	print(bold.sub(r'\textbf{\1}', line))

def parseCode(ifile, ofile, line):
	hidx = line.rfind("<source lang=\"bash\">")
	if hidx != -1:
		print("code ", lidx, " ",hidx)
		cap += line[lidx:]
		#print(line)
	else:
		return False
	

def convFile(url, name):
	ifileStr = url[26:len(url)]
	ifileStr = ifileStr.replace("/", "%2F")
	print("input ",ifileStr)

	name = name.replace(" ", "").lower()
	name += ".tex"
	if os.path.exists(name):
		os.remove("./"+name)
	
	mainfile = open(name, "w")
	
	cnt = 0

	ifile = open(ifileStr, "r")
	for foo in ifile:
		#print(foo)
		if foo.find("textarea") != -1:
			print("fount textarea")
			cnt+=1
			continue
		if cnt == 1:
			# the symbol < needs to be displayed
			foo = foo.replace("&lt;","<")
			form(foo)
			#parseCode(ifile, mainfile, foo)
			shortenString(foo, mainfile)	
			mainfile.write(foo)		
		if cnt > 1:
			break
	
	mainfile.close()
	ifile.close()
	print("./"+ifileStr)
	os.remove("./"+ifileStr)
