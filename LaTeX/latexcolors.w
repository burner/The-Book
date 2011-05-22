><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>
Adding colors to your text is supported by the <tt>color</tt> package. Using this package, you can set the color of the font of the text, and set the background color of the page. You can use one of the predefined colors such as ''white'', ''red'', or ''yellow'', or you can define your own named colors.

==Adding the color package==
To make use of these color features the color package must be inserted into the preamble.
<source lang="latex">
\usepackage{color}
</source>

==Entering colored text==

The simplest way to type colored text is by:
<source lang="latex">\textcolor{declared-color}{text}</source>
where ''declared-color'' is a color that was defined before by <source lang="latex" enclose="none">\definecolor</source>.

Another possible way is by 
<source lang="latex">{\color{declared-color} text}</source>
that will switch the standard text color to the color you want. It will work until the end of the current TeX group. For example:
<source lang="latex">\emph{some black text, \color{red} followed by a red fragment}, going black again.</source>

The difference between <source lang="latex" enclose="none">\textcolor</source> and <source lang="latex" enclose="none">\color</source> is the same as that between <source lang="latex" enclose="none">\texttt</source> and <source lang="latex" enclose="none">\ttfamily</source>, you can use the one you prefer.

You can change the background color of the whole page by:
<source lang="latex">\pagecolor{declared-color}</source>

==Entering colored background for the text==

<source lang="latex">
\colorbox{declared-color}{text}
</source>

If the background color and the text color is changed, then:
<source lang="latex">
\colorbox{declared-color1}{\color{declared-color2} text}
</source>

There is also \fcolorbox to make framed background color in yet another color:
<source lang="latex">
\fcolorbox{declared-color1}{declared-color2}{text}
</source>
==Predefined colors==

The predefined color names are <tt>white, black, red, green, blue, cyan, magenta, yellow</tt>.  There may be other pre-defined colors on your system, but these should be available on all systems.

If you would like a color not pre-defined, you can use one of the 68 dvips colors, or define your own. These options are discussed in the following sections

== The 68 standard colors known to dvips ==

Invoke the package with the usenames and dvipsnames option. If you are using TikZ package you must declare the color package before that, otherwise it will not work.

<source lang="latex">
\usepackage[usenames,dvipsnames]{color}
</source>

<table border="0" cellspacing="0" cellpadding="2">
<tr>
<td align="left" valign="middle">
<font color="#FBB982">'''Apricot'''</font></td>
<td align="left" valign="middle">
<font color="#00B5BE">'''Aquamarine'''</td>
<td align="left" valign="middle">
<font color="#C04F17">'''Bittersweet'''</td>
<td align="left" valign="middle">
<font color="#221E1F">'''Black'''</td>
</tr>

<tr>
<td align="left" valign="middle">
<font color="#2D2F92">'''Blue'''</td>
<td align="left" valign="middle">
<font color="#00B3B8">'''BlueGreen'''</td>
<td align="left" valign="middle">
<font color="#473992">'''BlueViolet'''</td>
<td align="left" valign="middle">
<font color="#B6321C">'''BrickRed'''</td>
</tr>

<tr>  
<td align="left" valign="middle">
<font color="#792500">'''Brown'''</td>
<td align="left" valign="middle">
<font color="#F7921D">'''BurntOrange'''</td>
<td align="left" valign="middle">
<font color="#74729A">'''CadetBlue'''</td>
<td align="left" valign="middle">
<font color="#F282B4">'''CarnationPink'''</td>
</tr>

<tr>  
<td align="left" valign="middle">
<font color="#00A2E3">'''Cerulean'''</td>
<td align="left" valign="middle">
<font color="#41B0E4">'''CornflowerBlue'''</td>
<td align="left" valign="middle">
<font color="#00AEEF">'''Cyan'''</td>
<td align="left" valign="middle">
<font color="#FDBC42">'''Dandelion'''</td>
</tr>

<tr>  
<td align="left" valign="middle">
<font color="#A4538A">'''DarkOrchid'''</td>
<td align="left" valign="middle">
<font color="#00A99D">'''Emerald'''</td>
<td align="left" valign="middle">
<font color="#009B55">'''ForestGreen'''</td>
<td align="left" valign="middle">
<font color="#8C368C">'''Fuchsia'''</td>
</tr>


<tr>  
<td align="left" valign="middle">
<font color="#FFDF42">'''Goldenrod'''</td>
<td align="left" valign="middle">
<font color="#949698">'''Gray'''</td>
<td align="left" valign="middle">
<font color="#00A64F">'''Green'''</td>
<td align="left" valign="middle">
<font color="#DFE674">'''GreenYellow'''</td>
</tr>

<tr>  
<td align="left" valign="middle">
<font color="#00A99A">'''JungleGreen'''</td>
<td align="left" valign="middle">
<font color="#F49EC4">'''Lavender'''</td>
<td align="left" valign="middle">
<font color="#8DC73E">'''LimeGreen'''</td>
<td align="left" valign="middle">
<font color="#EC008C">'''Magenta'''</td>
</tr>

<tr>  
<td align="left" valign="middle">
<font color="#A9341F">'''Mahogany'''</td>
<td align="left" valign="middle">
<font color="#AF3235">'''Maroon'''</td>
<td align="left" valign="middle">
<font color="#F89E7B">'''Melon'''</td>
<td align="left" valign="middle">
<font color="#006795">'''MidnightBlue'''</td>
</tr>

<tr>  
<td align="left" valign="middle">
<font color="#A93C93">'''Mulberry'''</td>
<td align="left" valign="middle">
<font color="#006EB8">'''NavyBlue'''</td>
<td align="left" valign="middle">
<font color="#3C8031">'''OliveGreen'''</td>
<td align="left" valign="middle">
<font color="#F58137">'''Orange'''</td>
</tr>

<tr>  
<td align="left" valign="middle">
<font color="#ED135A">'''OrangeRed'''</td>
<td align="left" valign="middle">
<font color="#AF72B0">'''Orchid'''</td>
<td align="left" valign="middle">
<font color="#F7965A">'''Peach'''</td>
<td align="left" valign="middle">
<font color="#7977B8">'''Periwinkle'''</td>
</tr>

<tr>  
<td align="left" valign="middle">
<font color="#008B72">'''PineGreen'''</td>
<td align="left" valign="middle">
<font color="#92268F">'''Plum'''</td>
<td align="left" valign="middle">
<font color="#00B0F0">'''ProcessBlue'''</td>
<td align="left" valign="middle">
<font color="#99479B">'''Purple'''</td>
</tr>

<tr>  
<td align="left" valign="middle">
<font color="#974006">'''RawSienna'''</td>
<td align="left" valign="middle">
<font color="#ED1B23">'''Red'''</td>
<td align="left" valign="middle">
<font color="#F26035">'''RedOrange'''</td>
<td align="left" valign="middle">
<font color="#A1246B">'''RedViolet'''</td>
</tr>

<tr>  
<td align="left" valign="middle">
<font color="#EF559F">'''Rhodamine'''</td>
<td align="left" valign="middle">
<font color="#0071BC">'''RoyalBlue'''</td>
<td align="left" valign="middle">
<font color="#613F99">'''RoyalPurple'''</td>
<td align="left" valign="middle">
<font color="#ED017D">'''RubineRed'''</td>
</tr>
<tr>  

<td align="left" valign="middle">
<font color="#F69289">'''Salmon'''</td>
<td align="left" valign="middle">
<font color="#3FBC9D">'''SeaGreen'''</td>
<td align="left" valign="middle">
<font color="#671800">'''Sepia'''</td>
<td align="left" valign="middle">
<font color="#46C5DD">'''SkyBlue'''</td>
</tr>

<tr>  
<td align="left" valign="middle">
<font color="#C6DC67">'''SpringGreen'''</td>
<td align="left" valign="middle">
<font color="#DA9D76">'''Tan'''</td>
<td align="left" valign="middle">
<font color="#00AEB3">'''TealBlue'''</td>
<td align="left" valign="middle">
<font color="#D883B7">'''Thistle'''</td>
</tr>

<tr>  
<td align="left" valign="middle">
<font color="#00B4CE">'''Turquoise'''</td>
<td align="left" valign="middle">
<font color="#58429B">'''Violet'''</td>
<td align="left" valign="middle">
<font color="#EF58A0">'''VioletRed'''</td>
<td align="left" valign="middle" bgcolor="#999999">
<font color="#FFFFFF">'''White'''</td>
</tr>

<tr>  
<td align="left" valign="middle">
<font color="#EE2967">'''WildStrawberry'''</td>
<td align="left" valign="middle">
<font color="#FFF200">'''Yellow'''</td>
<td align="left" valign="middle">
<font color="#98CC70">'''YellowGreen'''</td>
<td align="left" valign="middle">
<font color="#FAA21A">'''YellowOrange'''</td>
</tr>
</table>

==Defining new colors==
If the predefined colors are not adequate, you may wish to define your own.
===Place===
Define the colors in the ''preamble'' of your document. (Reason: Do so in the preamble, so that you can already refer to them in the preamble, which is useful, for instance, in an argument of another package that supports colors as arguments, such as [[LaTeX/Packages/Listings|listings]] package.)
===Method===
To define a new color, follow the following example, which defines orange for you, by setting the red to the maximum, the green to one half (0.5), and the blue to the minimum:

<source lang="latex">
\definecolor{orange}{rgb}{1,0.5,0}
</source>

You can use small letters '''rgb''' and choose a value between 0 and 1 or use capital letters '''RGB''' and choose a value between 0 and 255. The following code should give a similar results to the last code chunk.

<source lang="latex">
\definecolor{orange}{RGB}{255,127,0}
</source>

Trick : When surfing on the web, you can get hexadecimal code for each color on a web page using the ''colorzilla'' extension to Firefox. 

In the abstract, the colors are defined following this scheme:

<source lang="latex">
\definecolor{''name''}{''model''}{''color-spec''}
</source>

where:
* ''name'' is the name of the color; you can call it as you like
* ''model'' is the way you ''describe'' the color, and is one of ''gray'', ''rgb'' and ''cmyk''.
* ''color-spec'' is the description of the color

===Color Models===

Among the models you can use to describe the color are the following (several more are described in the [http://mirror.ctan.org/macros/latex/contrib/xcolor/xcolor.pdf xcolor manual]):
{| class="wikitable"
|+ Color Models
|-
! Model
! Description
! Color Specification
! Example
|-
|<tt>gray</tt> || Shades of gray. || just one number between 0 (black) and 1 (white), so 0.95 will be very light gray, 0.30 will be dark gray || <tt>\definecolor{light-gray}{gray}{0.95}</tt> 
|-
|<tt>rgb</tt> || Red, Green, Blue || three numbers given in the form ''red,green,blue''; the quantity of each color is represented with a number between 0 and 1 || <tt>\definecolor{orange}{rgb}{1,0.5,0}</tt>
|-
|<tt>RGB</tt> || Red, Green, Blue || three numbers given in the form ''red,green,blue''; the quantity of each color is represented with a number between 0 and 255 || <tt>\definecolor{orange}{RGB}{255,127,0}</tt>
|-
|<tt>HTML</tt> || Red, Green, Blue || six hexadecimal numbers given in the form ''RRGGBB''; similar to what is used in HTML || <tt>\definecolor{orange}{HTML}{FF7F00}</tt>
|-
|<tt>cmyk</tt> || Cyan, Magenta, Yellow, Black || four numbers given in the form ''cyan,magenta,yellow,black'' || <tt>\definecolor{orange}{cmyk}{0,0.5,1,0}</tt>
|-
|}

== Advanced color settings ==

The {{LaTeX/Package|xcolor}} package provides extended versions of the described color related commands. Tints can be defined as follow:
{{LaTeX/Usage|code=
\color{blue!20}
\color{blue!20!black}
\color{blue!20!black!30!green}
}}

The first specifies 20 percent blue, the second is a mixture of 20 percent blue and 80 percent black and the last one is a mixture of 20 percent blue, 30 percent black and 50 percent green.

Other features include: support of hsb color model, html style notation, special row coloring support in tables and more.

== Sources ==
* [http://mirror.ctan.org/macros/latex/contrib/xcolor/xcolor.pdf The xcolor manual]

<noinclude>
{{LaTeX/Bottom|Hyperlinks|Packages}}
</noinclude>
