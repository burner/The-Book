><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>

In the previous chapter, you learned that you can import or link graphics into LaTeX, such as graphics that you have created in another program or obtained elsewhere. In this chapter, you will learn how to create or embed graphics directly in a LaTeX document. The graphics is marked up using commands similar to those for typesetting bold text or creating mathematical formulas, as the following example of embedded graphics shows:

<source lang="latex">
\begin{displaymath}
    \xymatrix{ \bullet \ar[r] \ar@{.>}[r] & \bullet }
\end{displaymath}
</source>

There are several packages supporting the creation of graphics directly in LaTeX, including <tt>picture</tt>, <tt>xy-Pic</tt>, and <tt>PGF/TikZ</tt>, described in the following sections. 

==Overview==

The <tt>picture</tt> environment allows programming pictures directly in LaTeX. On the one hand, there are rather severe constraints, as the slopes of line segments as well as the radii of circles are restricted to a narrow choice of values. On the other hand, the picture environment of LaTeX2e brings with it the <tt>\qbezier</tt> command, "q" meaning ''quadratic''. Many frequently-used curves such as circles, ellipses, and [[wikipedia:catenary|catenaries]] can be satisfactorily approximated by quadratic Bézier curves, although this may require some mathematical toil. If a programming language like Java is used to generate <tt>\qbezier</tt> blocks of LaTeX input files, the picture environment becomes quite powerful.

Although programming pictures directly in LaTeX is severely restricted, and often rather tiresome, there are still reasons for doing so. The documents thus produced are "small" with respect to bytes, and there are no additional graphics files to be dragged along.

Packages like <tt>epic</tt>, <tt>eepic</tt> or <tt>pstricks</tt> enhance the original picture environment, and greatly strengthen the graphical power of LaTeX.

While the former two packages just enhance the picture environment, the <tt>pstricks</tt> package has its own drawing environment, <tt>pspicture</tt>. The power of <tt>pstricks</tt> stems from the fact that this package makes extensive use of PostScript possibilities. Unfortunately it has one big shortcoming: it doesn't work together with pdfLaTeX, as such to generate a PDF document form TeX source you have to go TeX→DVI→PDF; losing hyperlinks, metadata and microtypographic features of pdflatex. In addition, numerous packages have been written for specific purposes. One of them is ''XY-pic,'' described at the end of this chapter. A wide variety of these packages is described in detail in ''The LaTeX Graphics Companion'' (not to be confused with ''The LaTeX Companion'').

Perhaps the most powerful graphical tool related with LaTeX is MetaPost, the twin of Donald E. Knuth’s METAFONT. MetaPost has the very powerful and mathematically sophisticated programming language of METAFONT. Contrary to METAFONT, which generates bitmaps, MetaPost generates encapsulated PostScript files, which can be imported in LaTeX. For an introduction, see ''A User’s Manual for MetaPost.''  A very thorough discussion of LaTeX and TEX strategies for graphics (and fonts) can be found in ''TEX Unbound.''

The last but certainly not least is the PGF/TikZ system. While the previous systems (<tt>picture</tt>, <tt>epic</tt>, <tt>pstricks</tt> or <tt>metapost</tt>) focus on the ''how'' to draw, TikZ focuses more on the ''what'' to draw. One could say that TikZ is to drawing in LaTeX as LaTeX is to digital typesetting. It's recommended to use it if your LaTeX distribution includes it.

==The picture Environment==

===Basic Commands===
A <tt>picture</tt> environment is available in any LaTeX distribution, without the need of loading any external package. This environment is created with one of the two commands
<source lang="latex">
\begin{picture}(x, y) ... \end{picture}
</source>
or
<source lang="latex">
\begin{picture}(x, y)(x0, y0) ... \end{picture}
</source>

The numbers ''x, y, x0, y0'' are numbers (lengths) in the units of <tt>\unitlength</tt>, which can be reset any time (but not within a picture environment) with a command such as
<source lang="latex">
\setlength{\unitlength}{1.2cm}
</source>

The default value of <tt>\unitlength</tt> is <tt>1pt</tt>. The first pair, <math>(x, y)</math>, effects the reservation, within the document, of rectangular space for the picture. The optional second pair, <math>(x_0, y_0)</math>, assigns arbitrary coordinates to the bottom left corner of the reserved rectangle.

Most drawing commands have one of the two forms
<source lang="latex">
\put(x, y){object}
</source>
or
<source lang="latex">
\multiput(x, y)(dx, dy){n}{object}
</source>

Bézier curves are an exception. They are drawn with the command

<source lang="latex">
\qbezier(x1, y1)(x2, y2)(x3, y3)
</source>

===Line Segments===

Line segments are drawn with the command:
<source lang="latex">
\put(x, y){\line(x1, y1){length}}
</source>

The <tt>\line</tt> command has two arguments:
# a direction vector,
# a "length" (sort of: this argument is the length of the vertical coordinate in the case of a vertical line segment and of the horizontal coordinate in all other cases, rather than the length of the segment itself).

The components of the direction vector are restricted to the integers (''−6, −5, ... , 5, 6'') and they have to be coprime (no common divisor except 1). The figure below illustrates all 25 possible slope values in the first quadrant. The length is relative to <tt>\unitlength</tt>.

{|
|-
|<source lang="latex">
\setlength{\unitlength}{5cm}
\begin{picture}(1,1)
\put(0,0){\line(0,1){1}}
\put(0,0){\line(1,0){1}}
\put(0,0){\line(1,1){1}}
\put(0,0){\line(1,2){.5}}
\put(0,0){\line(1,3){.3333}}
\put(0,0){\line(1,4){.25}}
\put(0,0){\line(1,5){.2}}
\put(0,0){\line(1,6){.1667}}
\put(0,0){\line(2,1){1}}
\put(0,0){\line(2,3){.6667}}
\put(0,0){\line(2,5){.4}}
\put(0,0){\line(3,1){1}}
\put(0,0){\line(3,2){1}}
\put(0,0){\line(3,4){.75}}
\put(0,0){\line(3,5){.6}}
\put(0,0){\line(4,1){1}}
\put(0,0){\line(4,3){1}}
\put(0,0){\line(4,5){.8}}
\put(0,0){\line(5,1){1}}
\put(0,0){\line(5,2){1}}
\put(0,0){\line(5,3){1}}
\put(0,0){\line(5,4){1}}
\put(0,0){\line(5,6){.8333}}
\put(0,0){\line(6,1){1}}
\put(0,0){\line(6,5){1}}
\end{picture}
</source>
|[[Image:Latex_example_line_segments.png|400px]]
|}

===Arrows===

Arrows are drawn with the command
<source lang="latex">
\put(x, y){\vector(x1, y1){length}}
</source>

For arrows, the components of the direction vector are even more narrowly restricted than for line segments, namely to the integers (''−4, −3, ... , 3, 4''). Components also have to be coprime (no common divisor except 1). Notice the effect of the <tt>\thicklines</tt> command on the two arrows pointing to the upper left.

{|
|-
|<source lang="latex">
\setlength{\unitlength}{0.75mm}
\begin{picture}(60,40)
\put(30,20){\vector(1,0){30}}
\put(30,20){\vector(4,1){20}}
\put(30,20){\vector(3,1){25}}
\put(30,20){\vector(2,1){30}}
\put(30,20){\vector(1,2){10}}
\thicklines
\put(30,20){\vector(-4,1){30}}
\put(30,20){\vector(-1,4){5}}
\thinlines
\put(30,20){\vector(-1,-1){5}}
\put(30,20){\vector(-1,-4){5}}
\end{picture}
</source>
|[[Image:Latex_example_arrows.png|400px]]
|}

===Circles===

The command
<source lang="latex">
\put(x, y){\circle{diameter}}
</source>

draws a circle with center (x, y) and diameter (not radius) specified by ''diameter''. The picture environment only admits diameters up to approximately 14mm, and even below this limit, not all diameters are possible. The <tt>\circle*</tt> command produces disks (filled circles). As in the case of line segments, one may have to resort to additional packages, such as <tt>eepic</tt> or <tt>pstricks</tt>.

{|
|-
|<source lang="latex">
\setlength{\unitlength}{1mm}
\begin{picture}(60, 40)
\put(20,30){\circle{1}}
\put(20,30){\circle{2}}
\put(20,30){\circle{4}}
\put(20,30){\circle{8}}
\put(20,30){\circle{16}}
\put(20,30){\circle{32}}
\put(40,30){\circle{1}}
\put(40,30){\circle{2}}
\put(40,30){\circle{3}}
\put(40,30){\circle{4}}
\put(40,30){\circle{5}}
\put(40,30){\circle{6}}
\put(40,30){\circle{7}}
\put(40,30){\circle{8}}
\put(40,30){\circle{9}}
\put(40,30){\circle{10}}
\put(40,30){\circle{11}}
\put(40,30){\circle{12}}
\put(40,30){\circle{13}}
\put(40,30){\circle{14}}
\put(15,10){\circle*{1}}
\put(20,10){\circle*{2}}
\put(25,10){\circle*{3}}
\put(30,10){\circle*{4}}
\put(35,10){\circle*{5}}
\end{picture}
</source>
|[[Image:Latex_example_circles.png|400px]]
|}

There is another possibility within the picture environment. If one is not afraid of doing the necessary calculations (or leaving them to a program), arbitrary circles and ellipses can be patched together from quadratic Bézier curves. See ''Graphics in LaTeX2e'' for examples and Java source files.

===Text and formulas===

As this example shows, text and formulas can be written in the environment with the <tt>\put</tt> command in the usual way:

{|
|-
|<source lang="latex">
\setlength{\unitlength}{0.8cm}
\begin{picture}(6,5)
\thicklines
\put(1,0.5){\line(2,1){3}}
\put(4,2){\line(-2,1){2}}
\put(2,3){\line(-2,-5){1}}
\put(0.7,0.3){$A$}
\put(4.05,1.9){$B$}
\put(1.7,2.95){$C$}
\put(3.1,2.5){$a$}
\put(1.3,1.7){$b$}
\put(2.5,1.05){$c$}
\put(0.3,4){$F=
\sqrt{s(s-a)(s-b)(s-c)}$}
\put(3.5,0.4){$\displaystyle
s:=\frac{a+b+c}{2}$}
\end{picture}
</source>
|[[Image:Latex_example_text_formulas.png|400px]]
|}

===<tt>\multiput</tt> and <tt>\linethickness</tt>===

The command
<source lang="latex">
\multiput(x, y)(dx, dy ){n}{object}
</source>

has 4 arguments: the starting point, the translation vector from one object to the next, the number of objects, and the object to be drawn. The <tt>\linethickness</tt> command applies to horizontal and vertical line segments, but neither to oblique line segments, nor to circles. It does, however, apply to quadratic Bézier curves!

{|
|-
|<source lang="latex">
\setlength{\unitlength}{2mm}
\begin{picture}(30,20)
\linethickness{0.075mm}
\multiput(0,0)(1,0){26}%
{\line(0,1){20}}
\multiput(0,0)(0,1){21}%
{\line(1,0){25}}
\linethickness{0.15mm}
\multiput(0,0)(5,0){6}%
{\line(0,1){20}}
\multiput(0,0)(0,5){5}%
{\line(1,0){25}}
\linethickness{0.3mm}
\multiput(5,0)(10,0){2}%
{\line(0,1){20}}
\multiput(0,5)(0,10){2}%
{\line(1,0){25}}
\end{picture}
</source>
|[[Image:Latex_example_multiput.png|400px]]
|}

===Ovals===

The command
<source lang="latex">
\put(x, y){\oval(w, h)}
</source>

or
<source lang="latex"> 
\put(x, y){\oval(w, h)[position]}
</source>

produces an oval centered at ''(x, y)'' and having width ''w'' and height ''h''. The optional position arguments ''b, t, l, r'' refer to "top", "bottom", "left", "right", and can be combined, as the example illustrates. Line thickness can be controlled by two kinds of commands: <tt>\linethickness{''length''}</tt> on the one hand, <tt>\thinlines</tt> and <tt>\thicklines</tt> on the other. While <tt>\linethickness{''length''}</tt> applies only to horizontal and vertical lines (and quadratic Bézier curves), <tt>\thinlines</tt> and <tt>\thicklines</tt> apply to oblique line segments as well as to circles and ovals.

{|
|-
|<source lang="latex">
\setlength{\unitlength}{0.75cm}
\begin{picture}(6,4)
\linethickness{0.075mm}
\multiput(0,0)(1,0){7}%
{\line(0,1){4}}
\multiput(0,0)(0,1){5}%
{\line(1,0){6}}
\thicklines
\put(2,3){\oval(3,1.8)}
\thinlines
\put(3,2){\oval(3,1.8)}
\thicklines
\put(2,1){\oval(3,1.8)[tl]}
\put(4,1){\oval(3,1.8)[b]}
\put(4,3){\oval(3,1.8)[r]}
\put(3,1.5){\oval(1.8,0.4)}
\end{picture}
</source>
|[[Image:Latex_example_ovals.png|400px]]
|}

===Multiple Use of Predefined Picture Boxes===

A picture box can be ''declared'' by the command
<source lang="latex">
\newsavebox{name}
</source>
then ''defined'' by
<source lang="latex">
\savebox{name}(width,height)[position]{content}
</source>

and finally arbitrarily often be ''drawn'' by
<source lang="latex"> 
\put(x, y){\usebox{name}}
</source>

The optional position parameter has the effect of defining the "anchor point" of the savebox. In the example it is set to "bl" which puts the anchor point into the bottom left corner of the savebox. The other position specifiers are top and right.

The ''name'' argument refers to a LaTeX storage bin and therefore is of a command nature (which accounts for the backslashes in the current example). Boxed pictures can be nested: In this example, <tt>\foldera</tt> is used within the definition of <tt>\folderb</tt>. The <tt>\oval</tt> command had to be used as the <tt>\line</tt> command does not work if the segment length is less than about 3 mm.

{|
|-
|<source lang="latex">
\setlength{\unitlength}{0.5mm}
\begin{picture}(120,168)
\newsavebox{\foldera}
\savebox{\foldera}
  (40,32)[bl]{% definition
  \multiput(0,0)(0,28){2}
    {\line(1,0){40}}
  \multiput(0,0)(40,0){2}
    {\line(0,1){28}}
  \put(1,28){\oval(2,2)[tl]}
  \put(1,29){\line(1,0){5}}
  \put(9,29){\oval(6,6)[tl]}
  \put(9,32){\line(1,0){8}}
  \put(17,29){\oval(6,6)[tr]}
  \put(20,29){\line(1,0){19}}
  \put(39,28){\oval(2,2)[tr]}
}

\newsavebox{\folderb}
\savebox{\folderb}
  (40,32)[l]{% definition
  \put(0,14){\line(1,0){8}}
  \put(8,0){\usebox{\foldera}}
}

\put(34,26){\line(0,1){102}}
\put(14,128){\usebox{\foldera}}
\multiput(34,86)(0,-37){3}
{\usebox{\folderb}}
\end{picture}
</source>
|[[Image:Latex_example_multiple_pics.png|300px]]
|}

===Quadratic Bézier Curves===
The command 
<source lang="latex">
\qbezier(x1, y1)(x, y)(x2, y2)
</source>
draws a quadratic bezier curve where <math>P_1 = (x_1, y_1)</math>, <math>P_2 = (x_2, y_2)</math> denote the end points, and <math>S = (x, y)</math> denotes the intermediate control point. The respective tangent slopes, <math>m_1</math> and <math>m_2</math>, can be obtained from the equations
:<math>
\begin{cases}
x= \frac{m_2 x_2 - m_1 x_1  - (y_2 - y_1)}{m_2 - m_1} \\
y= y_i + m_i (x - x_i); \quad ( i=1,2 )
\end{cases}
</math>

See ''Graphics in LaTeX2e'' for a Java program which generates the necessary <tt>\qbezier</tt> command line.

{|
|-
|<source lang="latex">
\setlength{\unitlength}{0.8cm}
\begin{picture}(6,4)
\linethickness{0.075mm}
\multiput(0,0)(1,0){7}
{\line(0,1){4}}
\multiput(0,0)(0,1){5}
{\line(1,0){6}}
\thicklines
\put(0.5,0.5){\line(1,5){0.5}}
\put(1,3){\line(4,1){2}}
\qbezier(0.5,0.5)(1,3)(3,3.5)
\thinlines
\put(2.5,2){\line(2,-1){3}}
\put(5.5,0.5){\line(-1,5){0.5}}
\linethickness{1mm}
\qbezier(2.5,2)(5.5,0.5)(5,3)
\thinlines
\qbezier(4,2)(4,3)(3,3)
\qbezier(3,3)(2,3)(2,2)
\qbezier(2,2)(2,1)(3,1)
\qbezier(3,1)(4,1)(4,2)
\end{picture}
</source>
|[[Image:Latex_example_bezier.png|400px]]
|}

As this example illustrates, splitting up a circle into 4 quadratic Bézier curves is not satisfactory. At least 8 are needed. The figure again shows the effect of the <tt>\linethickness</tt> command on horizontal or vertical lines, and of the <tt>\thinlines</tt> and the <tt>\thicklines</tt> commands on oblique line segments. It also shows that both kinds of commands affect quadratic Bézier curves, each command overriding all previous ones.

===Catenary===

{|
|-
|<source lang="latex">
\setlength{\unitlength}{1cm}
\begin{picture}(4.3,3.6)(-2.5,-0.25)
\put(-2,0){\vector(1,0){4.4}}
\put(2.45,-.05){$x$}
\put(0,0){\vector(0,1){3.2}}
\put(0,3.35){\makebox(0,0){$y$}}
\qbezier(0.0,0.0)(1.2384,0.0)
(2.0,2.7622)
\qbezier(0.0,0.0)(-1.2384,0.0)
(-2.0,2.7622)
\linethickness{.075mm}
\multiput(-2,0)(1,0){5}
{\line(0,1){3}}
\multiput(-2,0)(0,1){4}
{\line(1,0){4}}
\linethickness{.2mm}
\put( .3,.12763){\line(1,0){.4}}
\put(.5,-.07237){\line(0,1){.4}}
\put(-.7,.12763){\line(1,0){.4}}
\put(-.5,-.07237){\line(0,1){.4}}
\put(.8,.54308){\line(1,0){.4}}
\put(1,.34308){\line(0,1){.4}}
\put(-1.2,.54308){\line(1,0){.4}}
\put(-1,.34308){\line(0,1){.4}}
\put(1.3,1.35241){\line(1,0){.4}}
\put(1.5,1.15241){\line(0,1){.4}}
\put(-1.7,1.35241){\line(1,0){.4}}
\put(-1.5,1.15241){\line(0,1){.4}}
\put(-2.5,-0.25){\circle*{0.2}}
\end{picture}
</source>
|[[Image:Latex_example_catenary.png|400px]]
|}

In this figure, each symmetric half of the catenary <math>y= \cosh x - 1</math> is approximated by a quadratic Bézier curve. The right half of the curve ends in the point (2, 2.7622), the slope there having the value m = 3.6269. Using again equation (*), we can calculate the intermediate control points. They turn out to be (1.2384, 0) and (−1.2384, 0). The crosses indicate points of the real catenary. The error is barely noticeable, being less than one percent. This example points out the use of the optional argument of the <tt>\begin{picture}</tt> command. The picture is defined in convenient "mathematical" coordinates, whereas by the command
<source lang="latex">
\begin{picture}(4.3,3.6)(-2.5,-0.25)
</source>

its lower left corner (marked by the black disk) is assigned the coordinates (−2.5,−0.25).

===Plotting graphs===

{|
|-
|<source lang="latex">
\setlength{\unitlength}{0.8cm}
\begin{picture}(6,4)(-3,-2)
\put(-2.5,0){\vector(1,0){5}}
\put(2.7,-0.1){$\chi$}
\put(0,-1.5){\vector(0,1){3}}
\multiput(-2.5,1)(0.4,0){13}
{\line(1,0){0.2}}
\multiput(-2.5,-1)(0.4,0){13}
{\line(1,0){0.2}}
\put(0.2,1.4)
{$\beta=v/c=\tanh\chi$}
\qbezier(0,0)(0.8853,0.8853)
(2,0.9640)
\qbezier(0,0)(-0.8853,-0.8853)
(-2,-0.9640)
\put(-3,-2){\circle*{0.2}}
\end{picture}
</source>
|[[Image:Latex_example_rapidity.png|400px]]
|}

The control points of the two Bézier curves were calculated with formulas (*). The positive branch is determined by <math>P_1 = (0, 0)</math>, <math>m_1 = 1</math> and <math>P_2 = (2, \tanh 2)</math>, <math>m_2 = 1/ \cosh^2 2</math>. Again, the picture is defined in mathematically convenient coordinates, and the lower left corner is assigned the mathematical coordinates (−3,−2) (black disk).


=== The <tt>picture</tt> environment and gnuplot ===

The powerful scientific plotting package [[Wikipedia:gnuplot|gnuplot]] has the capability to output directly to a LaTeX <tt>picture</tt> environment. It is often far more convenient to plot directly to LaTeX, since this saves having to deal with potentially troublesome postscript files. Plotting scientific data (or, indeed, mathematical figures) this way gives much greater control, and of course typesetting ability, than is available from other means (such as postscript).
Such pictures can then be added to a document by an <tt>\include{}</tt> command.

N.B. gnuplot is a powerful piece of software with a vast array of commands. A full discussion of gnuplot lies beyond the scope of this note.

==Xy-pic==

<tt>xy</tt> is a special package for drawing diagrams. To use it, simply add the
following line to the preamble of your document:
<source lang="latex">
\usepackage[all]{xy}
</source>
where "all" means you want to load a large standard set of functions from ''Xy-pic'', suitable for developing the kind of diagrams discussed here.

The primary way to draw ''Xy-pic'' diagrams is over a matrix-oriented canvas, where each diagram element is placed in a matrix slot:

{|
|-
|<source lang="latex">
\begin{displaymath}
    \xymatrix{A & B \\
              C & D }
\end{displaymath}
</source>
|[[Image:Latex_example_xypics_basic.png|150px]]
|}

The <tt>\xymatrix</tt> command must be used in math mode. Here, we specified two lines and two columns. To make this matrix a diagram we just add directed arrows using the <tt>\ar</tt> command.

{|
|-
|<source lang="latex">
\begin{displaymath}
    \xymatrix{ A \ar[r] & B \ar[d] \\
               D \ar[u] & C \ar[l] }
\end{displaymath}
</source>
|[[Image:Latex_example_xypics_arrows_1.png|150px]]
|}

The arrow command is placed on the origin cell for the arrow. The arguments are the direction the arrow should point to (up, down, right and left).

{|
|-
|<source lang="latex">
\begin{displaymath}
    \xymatrix{
        A \ar[d] \ar[dr] \ar[r] & B \\
        D                       & C }
\end{displaymath}
</source>
|[[Image:Latex_example_xypics_arrows_2.png|150px]]
|}

To make diagonals, just use more than one direction. In fact, you can repeat directions to make bigger arrows.

{|
|-
|<source lang="latex">
\begin{displaymath}
    \xymatrix{
        A \ar[d] \ar[dr] \ar[drr] &   &   \\
        B                         & C & D }
\end{displaymath}
</source>
|[[Image:Latex_example_xypics_arrows_3.png|200px]]
|}

We can draw even more interesting diagrams by adding labels to the arrows. To do this, we use the common superscript and subscript operators.

{|
|-
|<source lang="latex">
\begin{displaymath}
    \xymatrix{
        A \ar[r]^f \ar[d]_g & B \ar[d]^{g'} \\
        D \ar[r]_{f'}       & C }
\end{displaymath}
</source>
|[[Image:Latex_example_xypics_arrows_labels.png|150px]]
|}

As shown, you use these operators as in math mode. The only difference is that that superscript means "on top of the arrow", and subscript means "under the arrow". There is a third operator, the vertical bar: | It causes text to be placed in the arrow.

{|
|-
|<source lang="latex">
\begin{displaymath}
    \xymatrix{
        A \ar[r]|f \ar[d]|g & B \ar[d]|{g'} \\
        D \ar[r]|{f'}       & C }
\end{displaymath}
</source>
|[[Image:Latex_example_xypics_inarrow_labels.png|150px]]
|}

To draw an arrow with a hole in it, use <tt>\ar[...]|\hole</tt>. In some situations, it is important to distinguish between different types of arrows. This can be done by putting labels on them, or changing their appearance

{|
|-
|<source lang="latex">
\shorthandoff{"}
\begin{displaymath}
    \xymatrix{
        \bullet\ar@{->}[rr]     && \bullet\\
        \bullet\ar@{.<}[rr]     && \bullet\\
        \bullet\ar@{~)}[rr]     && \bullet\\
        \bullet\ar@{=(}[rr]     && \bullet\\
        \bullet\ar@{~/}[rr]     && \bullet\\
        \bullet\ar@{^{(}->}[rr] && \bullet\\
        \bullet\ar@2{->}[rr]    && \bullet\\
        \bullet\ar@3{->}[rr]    && \bullet\\
        \bullet\ar@{=+}[rr]     && \bullet }
\end{displaymath}
\shorthandon{"}
</source>
|[[Image:Latex_example_xypics_arrow_list.png|140px]]
|}

Notice the difference between the following two diagrams:

{|
|-
|<source lang="latex">
\begin{displaymath}
    \xymatrix{ \bullet \ar[r] \ar@{.>}[r] & \bullet }
\end{displaymath}
</source>
|[[Image:Latex_example_xypics_standard_arrow.png|150px]]
|-
|<source lang="latex">
\begin{displaymath}
    \xymatrix{
        \bullet \ar@/^/[r]
        \ar@/_/@{.>}[r] &
        \bullet }
\end{displaymath}
</source>
|[[Image:Latex example xypics curved arrow.png|150px]]
|}

The modifiers between the slashes define how the curves are drawn. ''Xy-pic'' offers many ways to influence the drawing of curves; for more information, check the ''Xy-pic'' documentation.

If you are interested in a more thorough introduction then consult the [http://xy-pic.sourceforge.net Xy-pic Home Page], which contains links to several other tutorials as well as the reference documentation.

==PGF/TikZ==

One possible solution how to draw graphics directly with TeX commands is [http://en.wikipedia.org/wiki/PGF/TikZ PGF/TikZ]. Ti''k''Z  can produce portable graphics in both PDF and PostScript formats using either plain (pdf)TEX, (pdf)Latex or ConTEXt. It comes with very good documentation and an extensive collection of examples: http://www.texample.net/tikz/

PGF ("portable graphics format") is the basic layer, providing a set of basic commands for producing graphics, and Ti''k''Z ("Ti''k''Z ist kein Zeichenprogramm") is the frontend layer with a special syntax, making the use of PGF easier. Ti''k''Z commands are prevalently similar to Metafont, the option mechanism is similar to PsTricks syntax. 

Other packages building on top of Ti''k''Z (e.g., for drawing electrical circuits) can be found here: http://ftp.dante.de/tex-archive/help/Catalogue/bytopic.html#pgftikzsection

In the following some basics of Ti''k''Z are presented.

=== Loading Package, Libraries - tikzpicture environment ===
Using Ti''k''Z in a LaTeX document requires to include
<source lang="latex">
\usepackage{tikz}
</source>
somewhere in the preamble. This will automatically load the pgf package. To load further libraries use
<source lang="latex">
\usetikzlibrary{⟨list of libraries separated by commas⟩}
</source>
Examples for libraries are "<code>arrows</code>", "<code>automata</code>", "<code>backgrounds</code>", "<code>calendar</code>", "<code>chains</code>", "<code>matrix</code>", "<code>mindmap</code>", "<code>patterns</code>", "<code>petri</code>", "<code>shadows</code>", "<code>shapes.geometric</code>", "<code>shapes.misc</code>", "<code>spy</code>", "<code>trees</code>".

Drawing commands have to be enclosed in an tikzpicture environment 
<source lang="latex">
\begin{tikzpicture}[⟨options ⟩] 
  ⟨tikz commands⟩
\end{tikzpicture}
</source>
or alternatively 
<source lang="latex">
\tikz[⟨options⟩]{⟨tikz commands⟩}
</source>
One possible option useful for inlined graphics is
<source lang="latex">
baseline=⟨dimension⟩
</source>
Without that option the lower end of the picture is put on the baseline of the surrounding text. Using this option, you can specify that the picture should be raised or lowered such that the height ⟨dimension⟩ is on the baseline.

Another option to scale the entire picture is
<source lang="latex">
scale=⟨factor⟩
</source>

=== Specifying Coordinates ===
Coordinates are specified in round brackets in an arbitrary TEX dimension either using cartesian coordinates (comma separated), e.g. 1cm in x and 2pt in y direction
<source lang="latex">
(1cm,2pt)
</source>
or using polar coordinates (colon separated), e.g. 1cm in 30 degree direction
<source lang="latex">
(30:1cm)
</source>
Without specifying a unit <code>(1,2)</code>, the standard one is cm <code>(1cm,2cm)</code>. 

Relative coordinates to the previous given point are given by adding one or two plus signs in front of the coordinate. With "<code>++</code>" the last point of the path becomes the current position, with "<code>+</code>" the previous point stays the current path position. Example: 2 standard units to the right of the last point used:
<source lang="latex">
++(2,0)
</source>

=== Syntax for Paths ===
A path is a series of straight and curved line segments. It has to end with a semicolon.
<source lang="latex">
\path[<options>]⟨specification⟩;
</source>
Options for path actions are e.g: "<code>draw</code>", "<code>fill</code>", "<code>pattern</code>", "<code>shade</code>" (filling, in which its color changes smoothly from one to another), "<code>clip</code>" (all subsequent drawings up to the end of the current scope are clipped against the current path and the size of subsequent paths will not be important for the picture size), "<code>use as bounding box</code>". 

The "<code>\path</code>" command with these options can be combined to: "<code>\draw</code>", "<code>\fill</code>", "<code>\filldraw</code>", "<code>\pattern</code>", "<code>\shade</code>", "<code>\shadedraw</code>", "<code>\clip</code>", "<code>\useasboundingbox</code>" <!--representing "<code>\path[draw]</code>", "<code>\path[fill]</code>", "<code>\path[fill,draw]</code>", "<code>\path[pattern]</code>", "<code>\path[shade]</code>", "<code>\path[shade,draw]</code>", "<code>\path[clip]</code>", "<code>\path[use as bounding box]</code>" !-->.

Geometric path options: "<code>rotation=<angle in degree></code>", "<code>xshift=<length></code>", "<code>yshift=<length></code>", "<code>scaling=<factor></code>", "<code>xscale=<factor></code>", "<code>yscale=<factor></code>".

Color options for drawing paths: "<code>color=<color name></code>", "<code>draw=<line color></code>", "<code>opacity=<factor></code>".

Line width options: "<code>line width=<dimension></code>", and abbreviations "<code>ultra thin</code>" for 0.1pt, "<code>very thin</code>" for 0.2pt, "<code>thin</code>" for 0.4pt (the default width), "<code>semithick</code>" for 0.6pt, "<code>thick</code>" for 0.8pt, "<code>very thick</code>" for 1.2pt, "<code>ultra thick</code>" for 1.6pt. 

Line end, line join options: "<code>line cap=<type: round, rect, or butt></code>", "<code>arrows=<start arrow kind>-<end arrow kind></code>", "<code>rounded corners</code>", "<code>rounded corners=<size></code>", "<code>line join=<type: round, bevel, or miter></code>".

Line pattern options: "<code>dash pattern=<dash pattern></code>" (e.g. "<code>dash pattern=on 2pt off 3pt on 4pt off 4pt</code>"), "<code>dash phase=⟨dash phase⟩</code>", "<code>solid</code>", "<code>dashed</code>", "<code>dotted</code>", "<code>dashdotted</code>", "<code>densely dotted</code>", "<code>loosely dotted</code>", "<code>double</code>".

Options for filling paths are e.g. "<code>fill=<fill color></code>", "<code>pattern=<name></code>", "<code>pattern color=<color></code>"

Straight lines are given by coordinates separated by a double minus, 
<source lang="latex">
\draw (1,0) -- (0,0) -- (0,1);
</source>
The first coordinate represents a move-to operation. This is followed by a series of “path extension operations”, like "<code>-- (coordinates)</code>".

A further move-to operation in an existing path starts a new part of the path, which is not connected to the previous part of the path. Here: Move to (0,0) straight line to (2,0), move to (0,1) straight line to (2,1):
<source lang="latex">
\draw (0,0) -- (2,0) (0,1) -- (2,1);
</source>

Connecting two points via straight lines that are only horizontal and vertical, use for first horizontal then vertial
<source lang="latex">
\draw (0,0) -| (1,1);
</source>
or for first vertical then horizontal
<source lang="latex">
\draw (0,0) |- (1,1);
</source>
Curved paths using a Bezier curve can be created using the "<code>..controls() ..()</code>" command, with one or two control points.
<source lang="latex">
\draw (0,0) .. controls (1,1) .. (4,0)
      (5,0) .. controls (6,0) and (6,1) .. (5,2);
</source>

A connected path can be closed using the "<code>--cycle</code>" operation:
<source lang="latex">
\draw (1,0) -- (0,0) -- (0,1) -- cycle;
</source>

User-defined paths can be created using the "<code>to</code>" operation. Without an option it corresponds to a straight line, exactly like the double minus command. Using the "<code>out</code>" and "<code>in</code>" option a curved path can created. E.g. "<code>[out=135,in=45]</code>" causes the path to leave at an angle of 135 degree at the first coordinate and arrive at an angle of 45 degree at the second coordinate.
<source lang="latex">
\draw (0,0) to (3,2);
\draw (0,0) to[out=90,in=180] (3,2);
\draw (0,0) to[bend right] (3,2);
</source>

For rectangles a special syntax exist. Use a move-to operation to one corner and after "<code>rectangle</code>" the coordinates of the diagonal corner. The last one becomes the new current point.
<source lang="latex">
\draw (0,0) rectangle (1,1);
\shade[top color=yellow, bottom color=black] (0,0) rectangle (2,-1);
</source>

Circles and ellipses paths are defined beginning with their center then using the "<code>circle command</code>" either with one length as radius of a circle or with two lengths as semi-axes of an ellipse.
<source lang="latex">
\draw (0,0) circle [radius=1.5];
\draw (0,0) circle (2cm); % old syntax
\draw (0,0) circle [x radius=1.5cm, y radius=10mm];
\draw (0,0) circle (1.2cm and 8mm); % old syntax
\draw (0,0) circle [x radius=1cm, y radius=5mm, rotate=30];
\draw[rotate=30] (0,0) ellipse (20pt and 10pt);  % old syntax
</source>

There are many more predefined commands for special paths, like "<code>arc</code>" for a part of an ellipse, "<code>grid</code>", "<code>parabola</code>", "<code>sin</code>", "<code>cos</code>" (sine or cosine curve in the interval [0,π/2]).
<source lang="latex">
\draw (0,0) arc (0:270:8mm);
\draw (0,0) arc (0:315:1.75cm and 1cm);
\filldraw[fill=green!20!white, draw=green!40!black] (0,0) -- (12mm,0mm) arc (0:30:12mm) -- (0,0);
</source>
The fill color "<code>green!20!white</code>" means 20% green and 80% white mixed together.

<source lang="latex">
\draw[step=0.5, gray, very thin] (-1.4,-1.4) grid (1.4,1.4);
\draw (0,0) parabola (1,1.5) parabola[bend at end] (2,0);
\draw (0,0) sin (1,1) cos (2,0) sin (3,-1) cos (4,0) sin (5,1);
</source>

To add arrow tips there are simple options for the drawing command:
<source lang="latex">
\draw [->] (0,0) -- (30:20pt); 
\draw [<->] (1,0) arc (180:30:10pt); 
\draw [<<->] (2,0) -- ++(0.5,10pt) -- ++(0.5,-10pt) -- ++(0.5,10pt);
</source>

A loop can be realized by "<code>\foreach ⟨variable⟩ in {⟨list of values⟩} ⟨commands⟩</code>".
<source lang="latex">
\foreach \x in {0,...,9} 
  \draw (\x,0) circle (0.4);
</source>

=== Nodes ===

A node is typically a rectangle or circle or another simple shape with some text on it. In the simplest case, a node is just some text that is placed at some coordinate.
Nodes are not part of the path itself, they are added to the picture after the path has been drawn.

Inside a path operation use the following syntax after a given coordinate:
<source lang="latex">
node[<options>](<name>){<text>}
</source>
The "<code>(<name>)</code>" is a name for later reference and it is optional. 
If you only want to name a certain position without writing text there are two possibilities:
<source lang="latex">
node[<options>](<name>){}
coordinate[<options>](<name>)
</source>

Writing text along a given path using the note command is shown as simple example:
<source lang="latex">
\draw[dotted]
    (0,0) node {1st node}
 -- (1,1) node {2nd node}
 -- (0,2) node {3rd node}
 -- cycle;
</source>

Possible options for the node command are e.g. "<code>inner sep=<dimension></code>", "<code>outer sep=<dimension></code>", "<code>minimum size=<dimension></code>", "<code>shape aspect=<aspect ratio></code>", "<code>text=<color></code>", "<code>font=<font commands></code>", "<code>align=<left_right_center></code>".

A node is centered at the current coordinate by default. Often it would be better to have the node to the besides the actual coordinate: Right ("<code>right</code>" or "<code>anchor=west</code>"), left ("<code>left</code>" or "<code>anchor=east</code>"), above ("<code>above</code>" or "<code>anchor=south</code>"), below ("<code>below</code>" or "<code>anchor=north</code>"). Combinations are also possible, like "<code>anchor=north east</code>" or "<code>below left</code>".

<source lang="latex">
\fill[fill=yellow]
    (0,0) node {1st node}
 -- (1,1) node[circle,inner sep=0pt,draw] {2nd node}
 -- (0,2) node[fill=red!20,draw,double,rounded corners] {3rd node};
</source>

To place nodes on a line or a curve use the "<code>pos=<fraction></code>" option, where fraction is a floating point number between 0 representing the previous coordinate and 1 representing the current coordinate.
<source lang="latex">
\draw (0,0) -- (3,1)
  node[pos=0]{0} node[pos=0.5]{1/2} node[pos=0.9]{9/10};
</source>
There exist some abbreviations: "<code>at start</code>" for "<code>pos=0</code>", "<code>very near start</code>" for "<code>pos=0.125</code>", "<code>near start</code>" for "<code>pos=0.25</code>", "<code>midway</code>" for "<code>pos=0.5</code>", "<code>near end</code>" for "<code>pos=0.75</code>", "<code>very near end</code>" for "<code>pos=0.875</code>", "<code>at end</code>" for "<code>pos=1</code>".

The "<code>sloped</code>" option causes the node to be rotated to become a tangent to the curve.

Since nodes are often the only path operation on paths, there are special commands for creating
paths containing only a node, the first with text ouput, the secound without:
<source lang="latex">
\node[<options>](<name>) at (<coordinate>){<text>};
\coordinate[<options>](<name>) at (<coordinate>);
</source>

One can connect nodes using the nodes' labels as coordinates. Having "<code>\path(0,0) node(x) {} (3,1) node(y) {};</code>" defined, the node at (0,0) got the name "<code>(x)</code>" and the one at (3,1) got a label "<code>(y)</code>". 
<source lang="latex">
\path (0,0) node(x) {} 
      (3,1) node(y) {};
\draw (x) -- (y);
</source>
Equivalent to
<source lang="latex">
\coordinate (x) at (0,0); 
\coordinate (y) at (3,1);
\draw (x) -- (y);
</source>

Path construction operations try to be clever, such that the path starts at the border of the node's shape and not from the node's center.
<source lang="latex">
\path (0,0) node(x) {Hello World!}
(3,1) node[circle,draw](y) {$\int_1^2 x \mathrm d x$};
\draw[->,blue] (x) -- (y);
\draw[->,red] (x) -| node[near start,below] {label} (y);
\draw[->,orange] (x) .. controls +(up:1cm) and +(left:1cm) .. node[above,sloped] {label} (y);
</source>

Once the node x has been defined, you can use anchors as defined above relative to (x) as "<code>(x.<anchor>)</code>", like "<code>(x.north)</code>".

=== Examples ===
Example 1
<source lang="latex">
\documentclass{article}
\usepackage{tikz}
\begin{document}
  \begin{tikzpicture}
  \draw[thick,rounded corners=8pt] (0,0) -- (0,2) -- (1,3.25) 
   -- (2,2) -- (2,0) -- (0,2) -- (2,2) -- (0,0) -- (2,0);
  \end{tikzpicture}
\end{document}
</source>

Example 2
<source lang="latex">
\documentclass{article}
\usepackage{tikz}
\begin{document}
 \begin{tikzpicture}[scale=3]
 \draw[step=.5cm, gray, very thin] (-1.2,-1.2) grid (1.2,1.2); 
 \filldraw[fill=green!20,draw=green!50!black] (0,0) -- (3mm,0mm) arc (0:30:3mm) -- cycle; 
 \draw[->] (-1.25,0) -- (1.25,0) coordinate (x axis);
 \draw[->] (0,-1.25) -- (0,1.25) coordinate (y axis);
 \draw (0,0) circle (1cm);
 \draw[very thick,red] (30:1cm) -- node[left,fill=white] {$\sin \alpha$} (30:1cm |- x axis);
 \draw[very thick,blue] (30:1cm |- x axis) -- node[below=2pt,fill=white] {$\cos \alpha$} (0,0);
 \draw (0,0) -- (30:1cm);
 \foreach \x/\xtext in {-1, -0.5/-\frac{1}{2}, 1} 
   \draw (\x cm,1pt) -- (\x cm,-1pt) node[anchor=north,fill=white] {$\xtext$};
 \foreach \y/\ytext in {-1, -0.5/-\frac{1}{2}, 0.5/\frac{1}{2}, 1} 
   \draw (1pt,\y cm) -- (-1pt,\y cm) node[anchor=east,fill=white] {$\ytext$};
 \end{tikzpicture}
\end{document}
</source>

== Chemical Graphics == 
{{under construction}}

[http://www.ctan.org/tex-archive/macros/latex/contrib/chemfig/ chemfig] is a package used to draw 2D chemical structures. It is an alternative to [http://www.2k-software.de/ingo/ochem.html ochem]. Whereas ochem requires Perl to draw chemical structures, chemfig uses the [http://az.ctan.org/pkg/pgf tikz] package to produce its graphics. chemfig is used by adding the following to the preamble:

<source lang="latex">
\usepackage{chemfig}
</source>

=== Basic Usage ===

The primary command used in this package is <tt>\chemfig{}</tt>:

<source lang="latex">
\chemfig{<atom1><bond type>[<angle>,<coeff>,<tikz code>]<atom2>}
</source>

<angle> is the bond angle between two atoms (or nodes). There are three types of angles: absolute, relative, and predefined. Exact angles give a precise angle (generally, 0 to 360, though they can also be negative), and are represented with the syntax <tt>[:<absolute angle>]</tt>. Relative angles require the syntax <tt>[::<relative angle>]</tt> and produce an angle relative to the angle of the preceding bond. Finally, predefined angles are whole numbers from 0 to 7 indicating intervals of 45 degrees. These are produced with the syntax <tt>[<predefined angle>]</tt>. The predefined angles and their corresponding absolute angles are represented in the diagram below.

{|
|<source lang="latex">
\chemfig{(-[:0,1.5,,,draw=none]\scriptstyle\color{red}0)
(-[1]1)(-[:45,1.5,,,draw=none]\scriptstyle\color{red}45)
(-[2]2)(-[:90,1.5,,,draw=none]\scriptstyle\color{red}90)
(-[3]3)(-[:135,1.5,,,draw=none]\scriptstyle\color{red}135)
(-[4]4)(-[:180,1.5,,,draw=none]\scriptstyle\color{red}180)
(-[5]5)(-[:225,1.5,,,draw=none]\scriptstyle\color{red}225)
(-[6]6)(-[:270,1.5,,,draw=none]\scriptstyle\color{red}270)
(-[7]7)(-[:315,1.5,,,draw=none]\scriptstyle\color{red}315)
-0}
</source>
|[[Image:Chemfig_angles.png|250px]]
|}

<bond type> describes the bond attaching <atom1> and <atom2>. There are 9 different bond types:

{|
|<source lang="latex">
\chemfig{A-B}\\
\chemfig{A=B}\\
\chemfig{A~B}\\
\chemfig{A>B}\\
\chemfig{A<B}\\
\chemfig{A>:B}\\
\chemfig{A<:B}\\
\chemfig{A>|B}\\
\chemfig{A<|B}\\ 
</source>
|[[Image:Chemfig_bonds.png|100px]]
|}

<coeff> represents the factor by which the bond's length will be multiplied. 

<tikz code> includes additional options regarding the color or style of the bond. 


A methane molecule, for instance, can be produced with the following code: 

{|
|<source lang="latex">
\chemfig{C(-[:0]H)(-[:90]H)(-[:180]H)(-[:270]H)}
</source>
|[[Image:Methane_chemfig.png|160px]]
|}


Linear molecules (such as methane) are a weak example of this, but molecules are formed in chemfig by nesting. 

=== Skeletal Diagrams === 

Skeleton diagrams can be produced as follows:
{|
|-
|<source lang="latex">
\chemfig{-[:30]-[:-30]-[:30]}
</source>
|[[Image:Skeletondiagram.png|160px]]
|-
|<source lang="latex">
\chemfig{-[:30]=[:-30]-[:30]}
</source>
|[[Image:Skeletondiagram2.png|160px]]
|}


=== Rings ===

Rings follow the syntax <tt><atom>*<n>(code)</tt>, where "n" indicates the number of sides in the ring and "code" represents the specific content of each ring (bonds and atoms).
{|
|-
|<source lang="latex">
\chemfig{A*6(-B-C-D-E-F-)}
</source>
|[[Image:Ring_chemfig.png|120px]]
|-
|<source lang="latex">
\chemfig{A*5(-B-C-D-E-)}
</source>
|[[Image:Ring2_chemfig.png|120px]]
|-
|<source lang="latex">
\chemfig{*6(=-=-=-)}
</source>
|[[Image:Ring3_chemfig.png|120px]]
|-
|<source lang="latex">
\chemfig{**5(------)}
</source>
|[[Image:Ring4_chemfig.png|120px]]
|}


=== Lewis Structures ===

Lewis structures can be created by using the command 

<source lang="latex">
\lewis{<electron angle><electron>,<atom>}
</source>

within <tt>\chemfig{}</tt>.


=== Ions ===

For example, consider an acetate ion: 

{|
|<source lang="latex">
\chemfig{-(-[1]O^{-})=[7]O}
</source>
|[[Image:Acetate-ion2.png|120px]]
|}


Because the chemfig commands enters the math mode, ion charges can be added as superscripts (one caveat: a negative ion requires that the minus sign be enclosed in brackets, as in the example). 

The charge of an ion can be circled by using <tt>\oplus</tt> and <tt>\ominus</tt>:  

{|
|<source lang="latex">
\chemfig{-(-[1]O^{\ominus})=[7]O}
</source>
|[[Image:Acetate-ion.png|120px]]
|}

Alternatively, charges can be placed above ions using \chemabove{}{}: 

{|
|<source lang="latex">
\chemfig{-\chemabove{N}{\scriptstyle\oplus}(=[1]O)-[7]O^{\ominus}} 
 </source>
|[[Image:Ion-example.png|120px]]
|}


=== Resonance Structures and Formal Charges === 

Resonance structures require a few math commands:

<source lang="latex">
% see "Advanced Mathematics" for use of \left and \right
% add to preamble:
%	\usepackage{mathtools}	% \Longleftrightarrow
$\left\{\chemfig{O-N(=[:60]O)-[:300]O}\right\}
\Longleftrightarrow 
\left\{\chemfig{O=N(-[:60]O)-[:300]O}\right\} 
\Longleftrightarrow 
\left\{\chemfig{O-N(-[:60]O)=[:300]O}\right\}$
 </source>
 
 
=== Chemical Reactions ===

Chemical reactions can be created with the following commands: 

{|
|<source lang="latex">
\chemrel[<arg1>][<arg2>]{<arrow code>}
</source>
|<source lang="latex">
\chemsign+	% produces a +
</source>
|}

In <tt>\chemrel{}</tt>, <arg1> and <arg2> represent text placed above and below the arrow, respectively.

There are four types of arrows that can be produced with tt>\chemrel{}</tt>:

<source lang="latex">
A\chemrel{->}B\par 
A\chemrel{<-}B\par 
A\chemrel{<->}B\par 
A\chemrel{<>}B
</source>


=== Naming Chemical Graphics ===

Molecules can be named with the command

<source lang="latex">
\chemname[<dim>]{\chemfig{<code of the molecule>}}{<name>}
</source>

<dim> is inserted between the bottom of the molecule and the top of the name defined by <name>. It is 1.5ex by default. 

<name> will be centered relative to the molecule it describes. 

<source lang="latex">
\chemname{\chemfig{R-C(-[:-30]OH)=[:30]O}}{Carboxylic acid} 
\chemsign{+} 
\chemname{\chemfig{R’OH}}{Alcohol} 
\chemrel{->} 
\chemname{\chemfig{R-C(-[:-30]OR’)=[:30]O}}{Ester} 
\chemsign{+} 
\chemname{\chemfig{H_2O}}{Water} 
</source>

In the reaction above, <tt>\chemname{}</tt> inserts 1.5ex plus the depth of the carboxylic acid molecule in between each molecule and their respective names. This is because the graphic for the first molecule in the reaction (carboxylic acid) extends deeper than the rest of the molecules. A different result is produced by putting the alcohol first:

<source lang="latex">
\chemname{\chemfig{R’OH}}{Alcohol} 
\chemsign{+} 
\chemname{\chemfig{R-C(-[:-30]OH)=[:30]O}}{Carboxylic acid} 
\chemrel{->} 
\chemname{\chemfig{R-C(-[:-30]OR’)=[:30]O}}{Ester} 
\chemsign{+} 
\chemname{\chemfig{H_2O}}{Water} 
</source>

This is fixed by adding <tt>\chemnameinit{<deepest molecule>}</tt> before the first instance of <tt>\chemname{}</tt> in a reaction and by adding <tt>\chemnameinit{}</tt> after the reaction: 

<source lang="latex">
\chemnameinit{\chemfig{R-C(-[:-30]OH)=[:30]O}} 
\chemname{\chemfig{R’OH}}{Alcohol} 
\chemsign{+} 
\chemname{\chemfig{R-C(-[:-30]OH)=[:30]O}}{Carboxylic acid} 
\chemrel{->} 
\chemname{\chemfig{R-C(-[:-30]OR’)=[:30]O}}{Ester} 
\chemsign{+} 
\chemname{\chemfig{H_2O}}{Water} 
\chemnameinit{} 
</source>

Lastly, adding <tt>\\</tt> in <name> will produce a line-break, allowing the name to span multiple lines.




=== Advanced Graphics ===

For advanced commands and examples, refer to the [http://mirror.ctan.org/macros/latex/contrib/chemfig/chemfig_doc_en.pdf chemfig manual], where a more thorough and complete introduction to the package can be found.

== Alternatives ==
In many cases, especially for more advanced diagrams, it may be easier to draw the graphics using external vector graphics software, and then import the file into the document (see [[../Importing Graphics]]). However most software does not support LaTeX fonts or mathematical notation, which can result in ugly and inconsistent graphics. There are several solutions to this problem.

The easiest solution is to use the picture environment and then simply use the "put" command to put a graphics file inside the picture, along with any other desired LaTeX element. For example:

{|
|-
|<source lang="latex">
\setlength{\unitlength}{0.8cm}
\begin{picture}(6,5)
\put(3.5,0.4){$\displaystyle
s:=\frac{a+b+c}{2}$}
\put(1,1){\includegraphics[width=2cm,height=2cm]{picture.eps}}
\end{picture}
</source>
|}

Another solution is to use [http://pav.iki.fi/software/textext/ textext], a plug-in for [http://www.inkscape.org/ Inkscape] which allows one to insert small LaTeX objects into .SVG images. These images can then be saved as .EPS (or .PDF) files which may then be imported into the LaTeX document proper.

Yet another solution is provided by [http://www.math.uni-leipzig.de/~matveyev/lpic/ lpic], which allows TeX annotations to imported graphics.

=== Using xfig to create pictures ===
An option that allows significantly more flexibility while creating graphics that are consistent with LaTeX is to use [http://www.xfig.org xfig]. xfig is a drawing program, which allows exports into various formats from which it's possible to import into LaTeX. While the software is designed for Linux, it can be run using [http://www.macports.org Macports] for Macs and under any X-Window System for Windows. 

There are many ways to use xfig to create graphics for LaTeX documents. One method is to export the drawing as a LaTeX document. This method, however, suffers from various drawbacks: lines can be drawn only at angles that are multiples of 30 and 45 degrees, lines with arrows can only be drawn at angles that are multiples of 45 degrees, several curves are not supported, etc.

Exporting a file as PDF/LaTeX or PS/LaTeX, on the other hand, offers a good deal more flexibility in drawing. Here's how it's done:
<ol>
<li> Create the drawing in xfig. Wherever you need LaTeX text, such as a mathematical formula, enter a LaTeX string in a textbox.
<li> Use the Edit tool to open the properties of each of those textboxes, and change the option on the "Special Flag" field to Special. This tells LaTeX to interpret these textboxes when it opens the figure.
<li> Go to File -> Export and export the file as PDF/LaTeX (both parts) or PS/LaTeX (both parts), depending on whether you are using pdflatex or pslatex to compile your file.
<li> In your LaTeX document, where the picture should be, use the following, where "test" is replaced by the name of the image:

<source lang="latex">
\begin{figure}[htbp]
 \centering
 \input{test.pdf_t}
 \caption{Your figure}
 \label{figure:example}
\end{figure}
</source>

Observe that this is just like including a picture, except that rather than using <tt>\includegraphics</tt>, we use <tt>\input</tt>. If the export was into PS/LaTeX, the file extension to include would be .pstex_t instead of .pdf_t.
<li> Make sure to include packages <tt>graphicx</tt> and <tt>color</tt> in the file, with the <tt>usepackage</tt> command right below the <tt>documentclass</tt> command, like this:
<source lang="latex">
\usepackage{graphicx}
\usepackage{color}
</source>

</ol>
And you're done!

For more details on using xfig with LaTeX, [http://www-epb.lbl.gov/xfig/latex_and_xfig.html this chapter] of the [http://www-epb.lbl.gov/xfig/contents.html xfig User Manual] may prove helpful.
<noinclude>
{{LaTeX/Bottom|Packages|Advanced Topics}}
</noinclude>
