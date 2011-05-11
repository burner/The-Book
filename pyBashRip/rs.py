#!/usr/bin/python
import sys

ifile = ""
mainfile = ""

subs = { "section":["==","==","\section{","}"]}

def subtitute(toSubB, toSubE, toSubWithB, toSubWithE):
	line = readLine()
	retLine = ""
	lowIdx = line.find(toSubB)
	if lowIdx != -1:
		retLine += line[0:lowIdx] + toSubWithB
		highIdx = line.find(toSubE, lowIdx+len(toSubB))
		print(highIdx)
		while highIdx == -1:
			line += readLine()
			highIdx = line.find(toSubE)

		retLine += line[lowIdx+len(toSubB):highIdx]
		retLine += toSubWithE
		print(retLine)
		retLine += line[highIdx+len(toSubE):len(line)]
		print(retLine)
	else:
		return line

	return retLine

def sub(sub):
	return subtitute(sub[0], sub[1], sub[2], sub[3])
	

def done():
	ifile.close()
	ofile.close()
	sys.exit()

def readLine():
	global ifile
	while ifile:
		line = ifile.readline()		
		print(line)
		if not line:
			done()
		return line

def main():
	global ifile
	global ofile
	if(len(sys.argv) == 2):
		ifile = open(sys.argv[1] + ".w", "r")
		ofile = open(sys.argv[1] + ".tex", "w")

	while 1:
		ofile.write(sub(subs["section"]))


if __name__ == "__main__":
	main()
