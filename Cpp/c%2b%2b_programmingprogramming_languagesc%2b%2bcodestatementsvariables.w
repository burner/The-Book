>== Variables ==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">== Variables ==
Much like a person has a name that distinguishes him or her from other people, a ''variable'' assigns a particular instance of an object type, a ''name'' or ''label'' by which the instance can be referred to. The variable is the most important concept in programming, it is how the code can manipulate data. Depending on its use in the code a variable has a specific locality in relation to the hardware and based on the structure of the code it also has a specific scope where the compiler will recognize it as valid. All these characteristics are defined by a programmer.

=== Internal storage ===
When programming, we need a way to store data that can be manipulated by our program. Data comes in a variety of formats, so the compiler needs a way to differentiate between the different types. 

==== Bits and bytes ====
;The bit
The smallest unit of data on a binary computer is a single bit. Since a single bit is capable of representing only two different values (typically zero or one) you may get the impression that there are a very small number of items you can represent with a single bit. Not true! There are an infinite number of items you can represent with a single bit.

With a single bit, you can represent any two distinct items. Examples include zero or one, true or false, on or off, male or female, and right or wrong. However, by using more than one bit, you will not be limited to representing binary data types (that is, those objects which have only two distinct values).

To confuse things even more, different bits can represent different things. For example, one bit might be used to represent the values zero and one, while an adjacent bit might be used to represent the colors red or black. How can you tell by looking at the bits? The answer, of course, is that you can't. But this illustrates the whole idea behind computer data structures: data is what you define it to be.

If you use a bit to represent a boolean (true/false) value then that bit (by your definition) represents true or false. For the bit to have any true meaning, you must be consistent. That is, if you're using a bit to represent true or false at one point in your program, you shouldn't use the true/false value stored in that bit to represent red or black later.

Since most items you will be trying to model require more than two different values, single bit values aren't the most popular data type. However, since everything else consists of groups of bits, bits will play an important role in your programs. Of course, there are several data types that require two distinct values, so it would seem that bits are important by themselves. however, you will soon see that individual bits are difficult to manipulate, so we'll often use other data types to represent boolean values.

;The nibble
A nibble is a collection of bits on a 4-bit boundary. It would not be a particularly interesting data structure except for two items: BCD (binary coded decimal) numbers and hexadecimal (base 16) numbers. It takes four bits to represent a single BCD or hexadecimal digit.

With a nibble, we can represent up to 16 distinct values. In the case of hexadecimal numbers, the values 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, and F are represented with four bits.

BCD uses ten different digits (0, 1, 2, 3, 4, 5, 6, 7, 8, 9) and requires four bits. In fact, any sixteen distinct values can be represented with a nibble, but hexadecimal and BCD digits are the primary items we can represent with a single nibble.

;The byte
The byte is the smallest individual piece of data that we can access or modify on a computer, it is without question, the most important data structure used by microprocessors today. Main memory and I/O addresses in the PC are all byte addresses. 

A byte consists of eight bits and is the smallest addressable datum (data item) in the microprocessor, this is why processors only works on bytes or groups of bytes, never on bits. To access anything smaller requires that you read the byte containing the data and mask out the unwanted bits.

Since the computer is a byte addressable machine, it turns out to be more efficient to manipulate a whole byte than an individual bit or nibble. For this reason, most programmers use a whole byte to represent data types that require no more than 256 items, even if fewer than eight bits would suffice. For example, we will often represent the boolean values true and false by 00000001 and 00000000 (respectively).

{{NOTE|This is why the [[C++ Programming/ASCII|ASCII code]], is used in  in most computers, it is based in a 7-bit non-weighted binary code, that takes advantage of the byte boundary.}}

Probably the most important use for a byte is holding a character code. Characters typed at the keyboard, displayed on the screen, and printed on the printer all have numeric values.

[[Image:byte45.png|||left|||A byte contains 8 bits]]
A byte (usually) contains 8 bits.  A bit can only have the value of 0 or 1. If all bits are set to 1, 11111111 in binary equals to 255 decimal.

The bits in a byte are numbered from bit zero (b0) through seven (b7) as follows:
b7	b6	b5	b4	b3	b2	b1	b0

Bit 0 (b0) is the low order bit or least significant bit, bit 7 is the high order bit or most significant bit of the byte. We'll refer to all other bits by their number.

A byte also contains exactly two ''nibbles''. Bits b0 through b3 comprise the low order nibble, and bits b4 through b7 form the high order nibble.

Since a byte contains eight bits, exactly two nibbles, byte values require two hexadecimal digits. It can represent 2^8, or 256, different values. Generally, we'll use a byte to represent:
#unsigned numeric values in the range 0 => 255 
#signed numbers in the range -128 => +127 
#ASCII character codes 
#other special data types requiring no more than 256 different values. Many data types have fewer than 256 items so eight bits is usually sufficient. 

In this representation of a computer byte a bit number is used to label each bit in the byte.  The bits are labeled from 7 to 0 instead of 0 to 7 or even 1 to 8, because processors always start counting at 0. It is simply more convenient to use 0 for computers as we shall see. The bits are also shown in descending order because, like with decimal numbers (normal base 10), we put the more significant digits to the left. 

Consider the number 254 in decimal. The 2 here is more significant than the other digits because it represents hundreds as opposed to tens for the 5 or singles for the 4. The same is done in binary. The more significant digits are put towards the left. In binary, there are only 2 digits, instead of counting from 0 to 9, we only count from 0 to 1, but counting is done by exactly the same principles as counting in decimal. If we want to count higher than 1, then we need to add a more significant digit to the left. In decimal, when we count beyond 9, we need to add a 1 to the next significant digit. It sometimes may look confusing or different only because humans are used to counting with 10 digits. 

{{NOTE|The most significant digit in a byte is bit#7 and the least significant digit is bit#0. These are otherwise known as "msb" and "lsb" respectively in lowercase.  If written in uppercase, MSB will mean most significant BYTE. You will see these terms often in programming or hardware manuals. Also, lsb is always bit#0, but msb can vary depending on how many bytes we use to represent numbers. However, we won't look into that right now.
}}

In decimal, each digit represents multiple of a power of 10. So, in the decimal number 254.
* The ''4'' represents four multiples of one (<math>4 \times 10^0</math> since <math>10^0 = 1</math>).
* Since we're working in decimal (base 10), the ''5'' represents five multiples of 10 (<math>5 \times 10^1</math>)
* Finally the ''2'' represents two multiples of 100 (<math>2 \times 10^2</math>)

All this is elementary. The key point to recognize is that as we move from right to left in the number, the significance of the digits increases by a multiple of 10. This should be obvious when we look at the following equation:

<math>(2 \times 10^2) + (5 \times 10^1) + (4 \times 10^0) = 254</math>

In binary, each digit can only be one of two possibilities (0 or 1), therefore when we work with binary we work in base 2 instead of base 10. So, to convert the binary number 1101 to decimal we can use the following base 10 equation, which is very much like the one above:

<math>(1 \times 2^3) + (1 \times 2^2) + (0 \times 2^1) + (1 \times 2^0) = 8 + 4 + 0 + 1 = 13</math>

[[Image:byte45.png|||left|||A byte contains 8 bits]]
To convert the number we simply add the bit values (<math>2^n</math>) where a 1 shows up. Let's take a look at our example byte again, and try to find its value in decimal.

First off, we see that bit #5 is a 1, so we have <math>2^5 = 32</math> in our total. Next we have bit#3, so we add <math>2^3 = 8</math>. This gives us 40. Then next is bit#2, so 40 + 4 is 44. And finally is bit#0 to give 44 + 1 = 45. So this binary number is 45 in decimal.

As can be seen, it is impossible for different bit combinations to give the same decimal value. Here is a quick example to show the relationship between counting in binary (base 2) and counting in decimal (base 10).

<math>00_2</math> = <math>0_{10}</math>, <math>01_2</math> = <math>1_{10}</math>, <math>10_2</math> = <math>2_{10}</math>, <math>11_2</math> = <math>3_{10}</math>

The bases that these numbers are in are shown in subscript to the right of the number.

===== Carry bit =====
[[Image:byte256.png|||left|||]]
As a side note. What would happen if you added 1 to 255? No combination will represent 256 unless we add more bits. The next value (if we could have another digit) would be 256. So our byte would look like this.

But this <math>9^{th}</math> bit (bit#8) doesn't exist. So where does it go? To be precise it actually goes into the carry bit. The carry bit resides in the processor of the computer, has an internal bit used exclusively for carry operations such as this. So if one adds 1 to 255 stored in a byte, the result would be 0 with the carry bit set in the CPU. Of course, a C++ programmer, never gets to use this bit directly. You'll would need to learn assembler to do that.

===== Endianness =====
After examining a single byte, it is time to look at ways to represent numbers larger than 255. This is done by grouping bytes together, we can represent numbers that are much larger than 255. If we use 2 bytes together, we double the number of bits in our number. In effect, 16 bits allows the representation numbers up to 65535 ({{C++ Programming/kw|unsigned}}), and 32 bits allows the representation of numbers above 4 billion. 

[[Image:PrimitiveTypes.png||right|3 basic primitive types char,short int,long int.]]

Here are a few basic primitive types:

* char (1 byte (by definition), max {{C++ Programming/kw|unsigned}} value: at least 255)

* short int (at least 16 bits, max {{C++ Programming/kw|unsigned}} value: at least 65535)

* long int (at least 32 bits, max {{C++ Programming/kw|unsigned}} value: at least 4294967295)

* float (typically 4 bytes, floating point)

* double (typically 8 bytes, floating point)


When using 'short int' and 'long int', you can leave out the 'int' as the compiler will know what type you want. You can also use 'int' by itself and it will default to whatever your compiler is set at for an int. On most recent compilers, int defaults to a 32-bit type.

All the information already given about the byte is valid for the other primitive types. The difference is simply the number of bits used is different and the msb is now bit#15 for a short and bit#31 for a long (assuming a 32-bit long type).

In a short (16-bit), one may think that in memory the byte for bits 15 to 8 would be followed by the byte for bits 7 to 0. In other words, byte #0 would be the high byte and byte #1 would be the low byte. This is true for some other systems. For example, the Motorola 68000 series CPUs do use this byte ordering. However, on PCs (with 8088/286/386/486/Pentiums) this is not so. The ordering is reversed so that the low byte comes before the high byte. The byte that represents bits 0 to 7 always comes before all other bytes on PCs. This is called little-endian ordering. The other ordering, such as on the M68000, is called big-endian ordering. This is very important to remember when doing low level byte operations that aim to be portable across systems.

For big-endian computers, the basic idea is to keep the higher bits on the left or in front.  For little-endian computers, the idea is to keep the low bits in the low byte.  There is no inherent advantage to either scheme except perhaps for an oddity.  Using a little-endian long int as a smaller type of int is theoretically possible as the low byte(s) is/are always in the same location (first byte).  With big-endian the low byte is always located differently depending on the size of the type.  For example (in big-endian), the low byte is the <math>4^{th}</math> byte in a long int and the <math>2^{nd}</math> byte in a short int.  So a proper cast must be done and low level tricks become rather dangerous.

To convert from one endianness to the other, one reverses the values of the bytes, putting the highest bytes value in the lowest byte and the lowest bytes value in the highest byte, and swap all the values for the in between bytes, so that if you had a 4 byte little-endian integer 0x0A0B0C0D (the 0x signifies that the value is hexadecimal) then converting it to big-endian would change it to 0x0D0C0B0A.

Bit endianness, where the bit order inside the bytes changes, is rarely used in data storage and only really ever matters in serial communication links, where the hardware deals with it.

===== Understanding two's complement =====
Two's complement is a way to store negative numbers in a pure binary representation. The reason that the two's complement method of storing negative numbers was chosen is because this allows the CPU to use the same add and subtract instructions on both signed and {{C++ Programming/kw|unsigned}} numbers.

To convert a positive number into its negative two's complement format, you begin by flipping all the bits in the number (1's become 0's and 0's become 1's) and then add 1. (This also works to turn a negative number back into a positive number Ex: -34 into 34 or vice-versa).

[[Image:byte45.png|||left|||A byte contains 8 bits]]
Let's try to convert our number 45.
{{clear}}
[[Image:byte45flip.png|||left|||A byte contains 8 bits]]
First, we flip all the bits...
{{clear}}
[[Image:byte45flip1.png|||left|||A byte contains 8 bits]]
And add 1.
{{clear}}
Now if we add up the values for all the one bits, we get... 128+64+16+2+1=211? What happened here? Well, this number actually is 211. It all depends on how you interpret it. If you decide this number is {{C++ Programming/kw|unsigned}}, then it's value is 211. But if you decide it's signed, then it's value is -45. It is completely up to you how you treat the number.

If and only if you decide to treat it as a signed number, then look at the msb (most significant bit [bit#7]). '''If it's a 1, then it's a negative number.''' If it's a 0, then it's positive. In C++, using {{C++ Programming/kw|unsigned}} in front of a type will tell the compiler you want to use this variable as an {{C++ Programming/kw|unsigned}} number, otherwise it will be treated as signed number.

Now, if you see the msb is set, then you know it's negative. So convert it back to a positive number to find out it's real value using the process just described above.

Let's go through a few examples.

<center>Treat the following number as an {{C++ Programming/kw|unsigned}} byte. What is it's value in decimal?</center>
[[Image:byte228.png|||center|||A byte contains 8 bits]]

Since this is an {{C++ Programming/kw|unsigned}} number, no special handling is needed. Just add up all the values where there's a 1 bit. 128+64+32+4=228. So this binary number is 228 in decimal.
{{clear}}
<center>Now treat the number above as a signed byte. What is its value in decimal?</center>

Since this is now a signed number, we first have to check if the msb is set. Let's look. Yup, bit #7 is set. So we have to do a two's complement conversion to get its value as a positive number (then we'll add the negative sign afterwards).

[[Image:byte228flip.png|||left|||A byte contains 8 bits]]
Ok, so let's flip all the bits...
[[Image:byte228flip1.png|||left|||A byte contains 8 bits]]
And add 1. This is a little trickier since a carry propagates to the third bit. For bit#0, we do 1+1 = 10 in binary. So we have a 0 in bit#0. Now we have to add the carry to the second bit (bit#1). 1+1=10. bit#1 is 0 and again we carry a 1 over to the <math>3^{rd}</math> bit (bit#2). 0+1 = 1 and we're done the conversion.

Now we add the values where there's a one bit. 16+8+4 = 28. Since we did a conversion, we add the negative sign to give a value of -28. So if we treat 11100100 (base 2) as a signed number, it has a value of -28. If we treat it as an {{C++ Programming/kw|unsigned}} number, it has a value of 228.

Let's try one last example.

<center>Give the decimal value of the following binary number both as a signed and {{C++ Programming/kw|unsigned}} number.</center>
[[Image:byte5.png|||center|||A byte contains 8 bits]]

First as an {{C++ Programming/kw|unsigned}} number. So we add the values where there's a 1 bit set. 4+1 = 5. For an {{C++ Programming/kw|unsigned}} number, it has a value of 5.

Now for a signed number. We check if the msb is set. Nope, bit #7 is 0. So for a signed number, it also has a value of 5.

As you can see, if a signed number doesn't have its msb set, then you treat it exactly like an {{C++ Programming/kw|unsigned}} number.

{{NOTE|note=
A special case of two's complement is where the sign bit (msb or bit#7 in a byte) is set to one and all other bits are zero, then its two's complement will be itself.  It is a fact that two's complement notation (signed numbers) have 1 extra number than can be negative than positive.  So for bytes, you have a range of -128 to +127.  The reason for this is that the number zero uses a bit pattern (all zeros).  Out of all the 256 possibilities, this leaves 255 to be split between positive and negative numbers.  As you can see, this is an odd number and cannot be divided equally.  If you were to try and split them, you would be left with the bit pattern described above where the sign bit is set (to 1) and all other bits are zeros.  Since the sign bit is set, it has to be a negative number.

If you see this bit pattern of a sign bit set with everything else a zero, you cannot convert it to a positive number using two's complement conversion.  The way you find out its value is to figure out the maximum number of bit patterns the value or type can hold.  For a byte, this is 256 possibilities.  Divide that number by 2 and put a negative sign in front.  So -128 is this number for a byte.  The following will be discussed below, but if you had 16 bits to work with, you have 65536 possibilities.  Divide by 2 and add the negative sign gives a value of -32768.
}}

===== Floating point representation =====
A generic real number with a decimal part can also be expressed in binary format. 
For instance 110.01 in binary corresponds to:

<math> 1 \times 2^2 + 1 \times 2^1 + 0 \times 2^0 + 0 \times 2^{-1} + 1 \times 2^{-2} = {\color{Blue} 2^2 + 2^1 + 2^{-2}} = 6.25 </math>

Exponential notation (also known as scientific notation, or standard form, ''when used with base 10'', as in <font style="font-size:10px;"><math>3 \times 10^8</math></font>) can be also used and the same number expressed as:

<math> 1.1001 \times 2^2 \qquad ( = 11.001 \times 2^1 = 110.01 )</math>

When there is only one non-zero digit on the left of the decimal point, the notation is termed normalized. 

In computing applications a real number is represented by a sign bit (S) an exponent (e) and a mantissa (M). The exponent field needs to represent both positive and negative exponents. To do this, a bias E is added to the actual exponent in order to get the stored exponent, and the sign bit (S), which indicates whether or not the number is negative, is transformed into either +1 or -1, giving s. A real number is thus represented as:

<math> f = s \times M \times 2^{e-E} </math>

S, e and M are concatenated one after the other in a 32-bit word to create a '''single''' precision floating point number and in a 64-bit doubleword to create a '''double''' precision one. For the single float type, 8 bits are used for the exponent and 23 bits for the mantissa, and the exponent offset is E=127. For the double type 11 bits are used for the exponent and 52 for the mantissa, and the exponent offset is E=1023.

There are two types of floating point numbers. ''Normalized'' and ''denormalized''. A normalized number will have an exponent e in the range 0<e<2<sup>8</sup> - 1 (between 00000000 and 11111111, non-inclusive) in a single precision float, and an exponent e in the range 0<e<2<sup>11</sup> - 1 (between 00000000000 and 11111111111, non-inclusive) for a double float. Normalized numbers are represented as sign times 1.''Mantissa'' times 2<sup>e-E</sup>. Denormalized numbers are numbers where the exponent is 0. They are represented as sign times 0.''Mantissa'' times 2<sup>1-E</sup>. Denormalized numbers are used to store the value 0, where the exponent and mantissa are both 0. Floating point numbers can store both +0 and -0, depending on the sign. When the number isn't normalized or denormalized (it's exponent is all 1s) the number will be plus or minus infinity if the mantissa is zero and depending on the sign, or plus or minus NaN (Not a Number) if the mantissa isn't zero and depending on the sign.

For instance the binary representation of the number 5.0 (using float type) is:

0 10000001 01000000000000000000000 

The first bit is 0, meaning the number is positive, the exponent is 129-127=2, and the mantissa is 1.01 (note the leading one is not included in the binary representation). 1.01 corresponds to 1.25 in decimal representation. Hence 1.25*4=5.

Floating point numbers are not always exact representations of values. a number like 1010110110001110101001101 couldn't be represented by a single precision floating point number because, disregarding the leading 1 which isn't part of the mantissa, there are 24 bits, and a single precision float can only store 23 numbers in its mantissa, so the 1 at the end would have to be dropped because it is the least significant bit. Also, there are some value which simply cannot be represented in binary which can be easily represented in decimal, E.g. 0.3 in decimal would be 0.0010011001100110011... or something. A lot of other numbers cannot be exactly represented by a binary floating point number, no matter how many bits it use for it's mantissa, just because it would create a repeating pattern like this.

{{TODO|
*Add a few comments on different standards...
*Add some images showing the bit representations like [[w:IEEE_754]] has
|C++ Programming}}

=== Locality (hardware) ===
Variables have two distinct characteristics: those that are created on the stack (local variables), and those that are accessed via a hard-coded memory address (global variables).

==== Globals ====
Typically a variable is bound to a particular address in [[w:computer memory |computer memory]] that is automatically assigned to at runtime, with a fixed number of bytes determined by the size of the object type of a variable and any operations performed on the variable effects one or more [[w:value (computer science)|values]] stored in that particular memory location.

All global defined variables will have static lifetime. Only those not defined as <code>const</code> will permit external linkage by default.

==== Locals ====
If the size and location of a variable is unknown beforehand, the location in memory of that variable is stored in another variable instead, and the size of the original variable is determined by the size of the type of the second value storing the memory location of the first. This is called [[w:reference (computer science)|referencing]], and the variable holding the other variables memory location is called a pointer.

=== [[C++ Programming/Programming Languages/C++/Code/Statements/Scope|Scope]] ===
{{:C++ Programming/Programming Languages/C++/Code/Statements/Scope/Variables}}

=== Definition vs. declaration ===
There is an important concept, the distinction between the declaration of a variable and its definition. The declaration announces the properties (the type, size, etc.), on the other hand the definition causes storage to be allocated in accordance to the declaration.
{{BookCat}}
<noinclude>{{displaytitle|title=C++ Programming}}</noinclude>
