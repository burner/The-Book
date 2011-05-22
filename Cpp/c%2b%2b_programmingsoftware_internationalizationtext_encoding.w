>=== Text encoding ===
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">=== Text encoding ===
Text, in particular the characters are used to generate readable text consists on the use of a character encoding scheme that pairs a sequence of characters from a given character set (sometimes referred to as code page) with something else, such as a sequence of natural numbers, octets or electrical pulses, in order to facilitate the use of its digital representation. 

A easy to understand example would be Morse code, which encodes letters of the Latin alphabet as series of long and short depressions of a telegraph key; this is similar to how ASCII, encodes letters, numerals, and other symbols, as integers.

;Text and data
Probably the most important use for a byte is holding a character code. Characters typed at the keyboard, displayed on the screen, and printed on the printer all have numeric values. To allow it to communicate with the rest of the world, the IBM PC uses a variant of the ASCII character set. There are 128 defined codes in the '''''[[C++ Programming/ASCII|ASCII character set]]'''''. IBM uses the remaining 128 possible values for extended character codes including European characters, graphic symbols, Greek letters, and math symbols.

In earlier days of computing, the introduction of coded character sets such as ASCII (1963) and EBCDIC (1964) began the process of standardization. The limitations of such sets soon became apparent, and a number of ad-hoc methods developed to extend them. The need to support multiple writing systems (Languages), including the CJK family of East Asian scripts, required support for a far larger number of characters and demanded a systematic approach to character encoding rather than the previous ad hoc approaches.

=== What's this about UNICODE? ===

{{TODO|Complete this section|C++ Programming}}

==== UTF-8 ====
UTF-8 is a variable-length encoding of unicode. It uses 1 to 4 bytes for each character. As a UTF-8 stream doesn't contain '\0', you may use it directly in your existing c++ code without any porting. (except when counting the 'actual' number of character in it)

==== UTF-16 ====

[[Category:C++ Programming|{{SUBPAGENAME}}]]
