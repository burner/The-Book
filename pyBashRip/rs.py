import subprocess
from conv import *

chapter = []

def extractChapter(line):
	lidx = line.find("[[/")
	uidx = line.find("/]]")
	if lidx != -1 and uidx != -1:
		lidx += 3
		chapter.append(line[lidx:uidx])
		print(line[lidx:uidx])
	

title="Bourne_Shell_Scripting"
url="http://en.wikibooks.org/w/index.php?title="+title+"&action=edit"
print(url)
prog="wget "+"\""+url+"\""
print(prog)
p = subprocess.Popen(prog, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
retval = p.wait()

ifileStr = url[26:len(url)]
print(ifileStr)

mainfile = open("bash.tex", "w")

cnt = 0

ifile = open(ifileStr, "r")
for foo in ifile:
	#print(foo)
	if foo.find("textarea") != -1:
		print("fount textarea")
		cnt+=1
		continue
	extractChapter(foo)	
	if cnt == 1:
		mainfile.write(foo)		
	if cnt > 1:
		break

mainfile.close()

for sec in chapter:
	foo = sec.replace(" ", "_")
	url = "http://en.wikibooks.org/w/index.php?title="+title+"/"+foo+"&action=edit"
	print(url)

	prog="wget "+"\""+url+"\""
	p = subprocess.Popen(prog, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
	retval = p.wait()
	convFile(url, sec)
