import mwparserfromhell
import json
from urllib.request import urlopen
s = urlopen('http://en.wikipedia.org/w/index.php?action=raw&title=Computer').read()
mystr = s.decode("utf8")
print(mystr)
wikicode = mwparserfromhell.parse(str(s))
print(wikicode)
