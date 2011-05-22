>{{Haskell minitoc|chapter=Advanced Haskell}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=Advanced Haskell}}

== Theseus and the Zipper ==
=== The Labyrinth ===
"Theseus, we have to do something" said Homer, chief marketing officer of Ancient Geeks Inc.. Theseus put the Minotaur action figure™ back onto the shelf and nodded. "Today's children are no longer interested in the ancient myths, they prefer modern heroes like Spiderman or Sponge Bob." ''Heroes''. Theseus knew well how much he had been a hero in the labyrinth back then on Crete<ref>Ian Stewart. ''The true story of how Theseus found his way out of the labyrinth''. Scientific American, February 1991, page 137.</ref>. But those "modern heroes" did not even try to appear realistic. What made them so successful? Anyway, if the pending sales problems could not be resolved, the shareholders would certainly arrange a passage over the Styx for Ancient Geeks Inc.

"Heureka! Theseus, I have an idea: we implement your story with the Minotaur as a computer game! What do you say?" Homer was right. There had been several books, epic (and chart breaking) songs, a mandatory movie trilogy and uncountable Theseus & the Minotaur™ gimmicks, but a computer game was missing. "Perfect, then. Now, Theseus, your task is to implement the game".

A true hero, Theseus chose Haskell as the language to implement the company's redeeming product in. Of course, exploring the labyrinth of the Minotaur was to become one of the game's highlights. He pondered: "We have a two-dimensional labyrinth whose corridors can point in many directions. Of course, we can abstract from the detailed lengths and angles: for the purpose of finding the way out, we only need to know how the path forks. To keep things easy, we model the labyrinth as a tree. This way, the two branches of a fork cannot join again when walking deeper and the player cannot go round in circles. But I think there is enough opportunity to get lost; and this way, if the player is patient enough, he can explore the entire labyrinth with the left-hand rule."

 data Node a = DeadEnd a
             | Passage a (Node a)
             | Fork    a (Node a) (Node a)

[[Image:Labyrinth-Tree.png|center|frame|An example labyrinth and its representation as tree.]]

Theseus made the nodes of the labyrinth carry an extra parameter of type <code>a</code>. Later on, it may hold game relevant information like the coordinates of the spot a node designates, the ambience around it, a list of game items that lie on the floor, or a list of monsters wandering in that section of the labyrinth. We assume that two helper functions

 get :: Node a -> a
 put :: a -> Node a -> Node a

retrieve and change the value of type <code>a</code> stored in the first argument of every constructor of <code>Node a</code>.
{{Exercises|1=
# Implement <code>get</code> and <code>put</code>. One case for <code>get</code> is <br><code>get (Passage x _) = x</code>.
# To get a concrete example, write down the labyrinth shown in the picture as a value of type <code>Node (Int,Int)</code>. The extra parameter <code>(Int,Int)</code> holds the cartesian coordinates of a node.}}

"Mh, how to represent the player's current position in the labyrinth? The player can explore deeper by choosing left or right branches, like in"

  turnRight :: Node a -> Maybe (Node a)
  turnRight (Fork _ l r) = Just r
  turnRight _            = Nothing

"But replacing the current top of the labyrinth with the corresponding sub-labyrinth this way is not an option, because he cannot go back then." He pondered. "Ah, we can apply ''Ariadne's trick with the thread'' for going back. We simply represent the player's position by the list of branches his thread takes, the labyrinth always remains the same."

 data Branch = KeepStraightOn
             | TurnLeft
             | TurnRight
 type Thread = [Branch]

[[Image:Labyrinth-Thread.png|center|frame|Representation of the player's position by Ariadne's thread.]]

"For example, a thread <code>[TurnRight,KeepStraightOn]</code> means that the player took the right branch at the entrance and then went straight down a <code>Passage</code> to reach its current position. With the thread, the player can now explore the labyrinth by extending or shortening it. For instance, the function <code>turnRight</code> extends the thread by appending the <code>TurnRight</code> to it."

 turnRight :: Thread -> Thread
 turnRight t = t ++ [TurnRight]

"To access the extra data, i.e. the game relevant items and such, we simply follow the thread into the labyrinth."

 retrieve :: Thread -> Node a -> a
 retrieve []                  n             = get n
 retrieve (KeepStraightOn:bs) (Passage _ n) = retrieve bs n
 retrieve (TurnLeft      :bs) (Fork _ l r)  = retrieve bs l
 retrieve (TurnRight     :bs) (Fork _ l r)  = retrieve bs r

{{Exercises|1=Write a function <code>update</code> that applies a function of type <code>a -> a</code> to the extra data at the player's position.}}

Theseus' satisfaction over this solution did not last long. "Unfortunately, if we want to extend the path or go back a step, we have to change the last element of the list. We could store the list in reverse, but even then, we have to follow the thread again and again to access the data in the labyrinth at the player's position. Both actions take time proportional to the length of the thread and for  large labyrinths, this will be too long. Isn't there another way?"

=== Ariadne's Zipper ===
While Theseus was a skillful warrior, he did not train much in the art of programming and could not find a satisfying solution. After intense but fruitless cogitation, he decided to call his former love Ariadne to ask her for advice. After all, it was she who had the idea with the thread.
<br>{{Haskell speaker 2|Ariadne Consulting. What can I do for you?}}
<br>Our hero immediately recognized the voice.
<br>"Hello Ariadne, it's Theseus."
<br>An uneasy silence paused the conversation. Theseus remembered well that he had abandoned her on the island of Naxos and knew that she would not appreciate his call. But Ancient Geeks Inc. was on the road to Hades and he had no choice.
<br>"Uhm, darling, ... how are you?"
<br>Ariadne retorted an icy response, {{Haskell speaker 2|Mr. Theseus, the times of ''darling'' are long over. What do you want?}}
<br>"Well, I uhm ... I need some help with a programming problem. I'm programming a new Theseus & the Minotaur™ computer game."
<br>She jeered, {{Haskell speaker 2|Yet another artifact to glorify your 'heroic being'? And you want me of all people to help you?}}
<br>"Ariadne, please, I beg of you, Ancient Geeks Inc. is on the brink of insolvency. The game is our last hope!"
<br>After a pause, she came to a decision.
<br>{{Haskell speaker 2|Fine, I will help you. But only if you transfer a substantial part of Ancient Geeks Inc. to me. Let's say thirty percent.}}
<br>Theseus turned pale. But what could he do? The situation was desperate enough, so he agreed but only after negotiating Ariadne's share to a tenth.

After Theseus told Ariadne of the labyrinth representation he had in mind, she could immediately give advice,
<br>{{Haskell speaker 2|You need a '''zipper'''.}}
<br>"Huh? What does the problem have to do with my fly?"
<br>{{Haskell speaker 2|Nothing, it's a data structure first published by Gérard Huet<ref>Gérard Huet. ''The Zipper''. Journal of Functional Programming, 7 (5), Sept 1997, pp. 549--554. [http://www.st.cs.uni-sb.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf PDF]</ref>.}}
<br>"Ah."
<br>{{Haskell speaker 2|More precisely, it's a purely functional way to augment tree-like data structures like lists or binary trees with a single '''focus''' or '''finger''' that points to a subtree inside the data structure and allows constant time updates and lookups at the spot it points to<ref>Note the notion of ''zipper'' as coined by Gérard Huet also allows to replace whole subtrees even if there is no extra data associated with them. In the case of our labyrinth, this is irrelevant. We will come back to this in the section [[#Differentiation of data types|Differentiation of data types]].</ref>. In our case, we want a focus on the player's position.}}
<br>"I know for myself that I want fast updates, but how do I code it?"
<br>{{Haskell speaker 2|Don't get impatient, you cannot solve problems by coding, you can only solve them by thinking. The only place where we can get constant time updates in a purely functional data structure is the topmost node<ref>Of course, the second topmost node or any other node at most a constant number of links away from the top will do as well.</ref><ref>Note that changing the whole data structure as opposed to updating the data at a node can be achieved in amortized constant time even if more nodes than just the top node is affected. An example is incrementing a number in binary representation. While incrementing say <code>111..11</code> must touch all digits to yield <code>1000..00</code>, the increment function nevertheless runs in constant amortized time (but not in constant worst case time).</ref>. So, the focus necessarily has to be at the top. Currently, the topmost node in your labyrinth is always the entrance, but your previous idea of replacing the labyrinth by one of its sub-labyrinths ensures that the player's position is at the topmost node.}}
<br>"But then, the problem is how to go back, because all those sub-labyrinths get lost that the player did not choose to branch into."
<br>{{Haskell speaker 2|Well, you can use my thread in order not to lose the sub-labyrinths.}}
<br>Ariadne savored Theseus' puzzlement but quickly continued before he could complain that he already used Ariadne's thread,
<br>{{Haskell speaker 2|The key is to ''glue the lost sub-labyrinths to the thread'' so that they actually don't get lost at all. The intention is that the thread and the current sub-labyrinth complement one another to the whole labyrinth. With 'current' sub-labyrinth, I mean the one that the player stands on top of. The zipper simply consists of the thread and the current sub-labyrinth.}}

 type Zipper a = (Thread a, Node a)

[[Image:Labyrinth-Zipper.png|center|frame|The zipper is a pair of Ariadne's thread and the current sub-labyrinth that the player stands on top. The main thread is colored red and has sub-labyrinths attached to it, such that the whole labyrinth can be reconstructed from the pair.]]

Theseus didn't say anything.
<br>{{Haskell speaker 2|You can also view the thread as a '''context''' in which the current sub-labyrinth resides. Now, let's find out how to define <code>Thread a</code>. By the way, <code>Thread</code> has to take the extra parameter <code>a</code> because it now stores sub-labyrinths. The thread is still a simple list of branches, but the branches are different from before.}}

 data Branch a  = KeepStraightOn a
                | TurnLeft  a (Node a)
                | TurnRight a (Node a)
 type Thread a  = [Branch a]

{{Haskell speaker 2|Most importantly, <code>TurnLeft</code> and <code>TurnRight</code> have a sub-labyrinth glued to them. When the player chooses say to turn right, we extend the thread with a <code>TurnRight</code> and now attach the untaken left branch to it, so that it doesn't get lost.}}
<br>Theseus interrupts, "Wait, how would I implement this behavior as a function <code>turnRight</code>? And what about the first argument of type <code>a</code> for <code>TurnRight</code>? Ah, I see. We not only need to glue the branch that would get lost, but also the extra data of the <code>Fork</code> because it would otherwise get lost as well. So, we can generate a new branch by a preliminary"

 branchRight (Fork x l r) = TurnRight x l

"Now, we have to somehow extend the existing thread with it."
<br>{{Haskell speaker 2|Indeed. The second point about the thread is that it is stored ''backwards''. To extend it, you put a new branch in front of the list. To go back, you delete the topmost element.}}
<br>"Aha, this makes extending and going back take only constant time, not time proportional to the length as in my previous version. So the final version of <code>turnRight</code> is"

 turnRight :: Zipper a -> Maybe (Zipper a)
 turnRight (t, Fork x l r) = Just (TurnRight x l : t, r)
 turnRight _               = Nothing

[[Image:Labyrinth-TurnRight.png|center|frame|Taking the right subtree from the entrance. Of course, the thread is initially empty. Note that the thread runs backwards, i.e. the topmost segment is the most recent.]]

"That was not too difficult. So let's continue with <code>keepStraightOn</code> for going down a passage. This is even easier than choosing a branch as we only need to keep the extra data:"

 keepStraightOn :: Zipper a -> Maybe (Zipper a)
 keepStraightOn (t, Passage x n) = Just (KeepStraightOn x : t, n)
 keepStraightOn _                = Nothing

[[Image:Labyrinth-KeepStraightOn.png|center|frame|Now going down a passage.]]

{{Exercises|1=Write the function <code>turnLeft</code>.}}

Pleased, he continued, "But the interesting part is to go back, of course. Let's see..."

 back :: Zipper a -> Maybe (Zipper a)
 back ([]                   , _) = Nothing
 back (KeepStraightOn x : t , n) = Just (t, Passage x n)
 back (TurnLeft  x r    : t , l) = Just (t, Fork x l r)
 back (TurnRight x l    : t , r) = Just (t, Fork x l r)

"If the thread is empty, we're already at the entrance of the labyrinth and cannot go back. In all other cases, we have to wind up the thread. And thanks to the attachments to the thread, we can actually reconstruct the sub-labyrinth we came from."
<br>Ariadne remarked, {{Haskell speaker 2|Note that a partial test for correctness is to check that each bound variable like <code>x</code>, <code>l</code> and <code>r</code> on the left hand side appears exactly once at the right hands side as well. So, when walking up and down a zipper, we only redistribute data between the thread and the current sub-labyrinth.}}

{{Exercises|1=
<ol><li>Now that we can navigate the zipper, code the functions <code>get</code>, <code>put</code> and <code>update</code> that operate on the extra data at the player's position.
</li><li>Zippers are by no means limited to the concrete example <code>Node a</code>, they can be constructed for all tree-like data types. Go on and construct a zipper for binary trees
<pre>
 data Tree a = Leaf a | Bin (Tree a) (Tree a)
</pre>
Start by thinking about the possible branches <code>Branch a</code> that a thread can take. What do you have to glue to the thread when exploring the tree?
</li><li>Simple lists have a zipper as well.
<pre>
 data List a = Empty | Cons a (List a)
</pre>
What does it look like?
</li><li>Write a complete game based on Theseus' labyrinth.
</li></ol>}}

Heureka! That was the solution Theseus sought and Ancient Geeks Inc. should prevail, even if partially sold to Ariadne Consulting. But one question remained:
<br>"Why is it called zipper?"
<br>{{Haskell speaker 2|Well, I would have called it 'Ariadne's pearl necklace'. But most likely, it's called zipper because the thread is in analogy to the open part and the sub-labyrinth is like the closed part of a zipper. Moving around in the data structure is analogous to zipping or unzipping the zipper.}}
<br>"'Ariadne's pearl necklace'," he articulated disdainfully. "As if your thread was any help back then on Crete."
<br>{{Haskell speaker 2|As if the idea with the thread were yours,}} she replied.
<br>"Bah, I need no thread," he defied the fact that he actually did need the thread to program the game.
<br>Much to his surprise, she agreed, {{Haskell speaker 2|Well, indeed you don't need a thread. Another view is to literally grab the tree at the focus with your finger and lift it up in the air. The focus will be at the top and all other branches of the tree hang down. You only have to assign the resulting tree a suitable algebraic data type, most likely that of the zipper.}}

[[Image:Labyrinth-Finger.png|center|frame|Grab the focus with your finger, lift it in the air and the hanging branches will form new tree with your finger at the top, ready to be structured by an algebraic data type.]]

"Ah." He didn't need Ariadne's thread but he needed Ariadne to tell him? That was too much.
<br>"Thank you, Ariadne, good bye."
<br>She did not hide her smirk as he could not see it anyway through the phone.

{{Exercises|1=Take a list, fix one element in the middle with your finger and lift the list into the air. What type can you give to the resulting tree?
}}


Half a year later, Theseus stopped in front of a shop window, defying the cold rain that tried to creep under his buttoned up anorak. Blinking letters announced
<center>"Spider-Man: lost in the Web"<br>
- find your way through the labyrinth of threads -<br>
the great computer game by Ancient Geeks Inc.
</center>
He cursed the day when he called Ariadne and sold her a part of the company. Was it she who contrived the unfriendly takeover by WineOS Corp., led by Ariadne's husband Dionysus? Theseus watched the raindrops finding their way down the glass window. After the production line was changed, nobody would produce Theseus and the Minotaur™ merchandise anymore. He sighed. His time, the time of heroes, was over. Now came the super-heroes.

== Differentiation of data types ==

The previous section has presented the zipper, a way to augment a tree-like data structure <code>Node a</code> with a finger that can focus on the different subtrees. While we constructed a zipper for a particular data structure <code>Node a</code>, the construction can be easily adapted to different tree data structures by hand.
{{Exercises|1=
Start with a ternary tree
<pre>
 data Tree a = Leaf a | Node (Tree a) (Tree a) (Tree a)
</pre>
and derive the corresponding <code>Thread a</code> and <code>Zipper a</code>.
}}

=== Mechanical Differentiation ===
But there is also an entirely mechanical way to derive the zipper of any (suitably regular) data type. Surprisingly, 'derive' is to be taken literally, for the zipper can be obtained by the '''derivative''' of the data type, a discovery first described by Conor McBride<ref>Conor Mc Bride. ''The Derivative of a Regular Type is its Type of One-Hole Contexts''. Available online. [http://strictlypositive.org/diff.pdf PDF]</ref>. The subsequent section is going to explicate this truly wonderful mathematical gem.

For a systematic construction, we need to calculate with types. The basics of structural calculations with types are outlined in a separate chapter [[../Generic Programming/]] and we will heavily rely on this material.

Let's look at some examples to see what their zippers have in common and how they hint differentiation. The type of binary tree is the fixed point of the recursive equation
<center><math>\mathit{Tree2} = 1 + \mathit{Tree2}\times\mathit{Tree2}</math>.</center>
When walking down the tree, we iteratively choose to enter the left or the right subtree and then glue the not-entered subtree to Ariadne's thread. Thus, the branches of our thread have the type
<center><math>\mathit{Branch2} = \mathit{Tree2} + \mathit{Tree2} \cong 2\times\mathit{Tree2}</math>.</center>
Similarly, the thread for a ternary tree
<center><math>\mathit{Tree3} = 1 + \mathit{Tree3}\times\mathit{Tree3}\times\mathit{Tree3}</math></center>
has branches of type
<center><math>\mathit{Branch3} = 3\times\mathit{Tree3}\times\mathit{Tree3}</math></center>
because at every step, we can choose between three subtrees and have to store the two subtrees we don't enter. Isn't this strikingly similar to the derivatives <math>\frac{d}{dx} x^2 = 2\times x</math> and <math>\frac{d}{dx} x^3 = 3\times x^2</math>?

The key to the mystery is the notion of the '''one-hole context''' of a data structure. Imagine a data structure parameterised over a type <math>X</math>, like the type of trees <math>\mathit{Tree}\,X</math>. If we were to remove one of the items of this type <math>X</math> from the structure and somehow mark the now empty position, we obtain a structure with a marked hole. The result is called "one-hole context" and inserting an item of type <math>X</math> into the hole gives back a completely filled <math>\mathit{Tree}\,X</math>. The hole acts as a distinguished position, a focus. The figures illustrate this.
{|
|-
| [[Image:One-hole-context-Tree.png|center|frame|Removing a value of type <math>X</math> from a <math>\mathit{Tree}\,X</math> leaves a hole at that position.]]
| [[Image:One-hole-context-plug.png|center|frame|A more abstract illustration of plugging <math>X</math> into a one-hole context.]]
|}

Of course, we are interested in the type to give to a one-hole context, i.e. how to represent it in Haskell. The problem is how to efficiently mark the focus. But as we will see, finding a representation for one-hole contexts by induction on the structure of the type we want to take the one-hole context of automatically leads to an efficient data type<ref>This phenomenon already shows up with generic tries.</ref>. So, given a data structure <math>F\, X</math> with a functor <math>F</math> and an argument type <math>X</math>, we want to calculate the type <math>\partial F\, X</math> of one-hole contexts from the structure of <math>F</math>. As our choice of notation <math>\partial F</math> already reveals, the rules for constructing one-hole contexts of sums, products and compositions are exactly Leibniz' rules for differentiation.
{|class="wikitable"
|-
! colspan=2 | One-hole context
! Illustration
|-
| <math>(\partial\mathit{Const_A})\,X</math>
| <math>=\,0</matH>
| There is no <math>X</math> in <math>A = \mathit{Const_A}\,X</math>, so the type of its one-hole contexts must be empty.
|-
| <math>(\partial\mathit{Id})\,X</math>
| <math>=\,1</matH>
| There is only one position for items <math>X</math> in <math>X=\mathit{Id}\,X</math>. Removing one <math>X</math> leaves no <math>X</math> in the result. And as there is only one position we can remove it from, there is exactly one one-hole context for <math>\mathit{Id}\,X</math>. Thus, the type of one-hole contexts is the singleton type.
|-
| <math>\partial(F + G)</math>
| <math>=\partial F + \partial G</math>
| As an element of type <math>F+G</math> is either of type <math>F</math> or of type <math>G</math>, a one-hole context is also either <math>\partial F</math> or <math>\partial G</math>.
|-
| <math>\partial (F \times G)</math>
| <math>=F \times \partial G + \partial F \times G</math>
| [[Image:One-hole-context-product.png]]<br>
The hole in a one-hole context of a pair is either in the first or in the second component.
|-
| <math>\partial (F \circ G)</math>
| <math>=(\partial F \circ G) \times \partial G</math>
| [[Image:One-hole-context-composition.png]]<br>
'''Chain rule'''. The hole in a composition arises by making a hole in the enclosing structure and fitting the enclosed structure in.
|}
Of course, the function <code>plug</code> that fills a hole has the type <math>(\partial F\,X) \times X \to F\,X</math>.

So far, the syntax <math>\partial</math> denotes the differentiation of functors, i.e. of a kind of type functions with one argument. But there is also a handy expression oriented notation '''<math>\partial_X</math>''' slightly more suitable for calculation. The subscript indicates the variable with respect to which we want to differentiate. In general, we have
<center><math>(\partial F)\,X=\partial_X(F\,X)</math></center>
An example is
<center><math>\partial(\mathit{Id}\times\mathit{Id})\,X=\partial_X(X\times X)=1\times X + X\times 1 \cong 2\times X</math></center>
Of course, <math>\partial_X</math> is just point-wise whereas <math>\partial</math> is point-free style.
{{Exercises|1=
# Rewrite some rules in point-wise style. For example, the left hand side of the product rule becomes <math>\partial_X(F\,X \times G\,X) = \dots </math>.
# To get familiar with one-hole contexts, differentiate the product <math>X^n := X\times X\times \dots\times X</math> of exactly <math>n</math> factors formally and convince yourself that the result is indeed the corresponding one-hole context.
# Of course, one-hole contexts are useless if we cannot plug values of type <math>X</math> back into them. Write the <code>plug</code> functions corresponding to the five rules.
# Formulate the '''chain rule''' for '''two variables''' and prove that it yields one-hole contexts. You can do this by viewing a bifunctor <math>F\,X\,Y</math> as an normal functor in the pair <math>(X,Y)</math>. Of course, you may need a handy notation for partial derivatives of bifunctors in point-free style.
}}

=== Zippers via Differentiation ===

The above rules enable us to construct '''zipper'''s for recursive data types <math>\mu F := \mu X.\,F\,X</math> where <math>F</math> is a polynomial functor. A zipper is a focus on a particular subtree, i.e. substructure of type <math>\mu F</math> inside a large tree of the same type. As in the previous chapter, it can be represented by the subtree we want to focus at and the thread, that is the context in which the subtree resides
<center><math>\mathit{Zipper}_F = \mu F\times\mathit{Context}_F</math>.</center>
Now, the context is a series of steps each of which chooses a particular subtree <math>\mu F</math> among those in <math>F\,\mu F</math>. Thus, the unchosen subtrees are collected together by the one-hole context <math>\partial F\,(\mu F)</math>. The hole of this context comes from removing the subtree we've chosen to enter. Putting things together, we have
<center><math>\mathit{Context}_F = \mathit{List}\, (\partial F\,(\mu F))</math>.</center>
or equivalently
<center><math>\mathit{Context}_F = 1 + \partial F\,(\mu F) \times \mathit{Context}_F</math>.</center>

To illustrate how a concrete calculation proceeds, let's systematically construct the zipper for our labyrinth data type

 data Node a = DeadEnd a
             | Passage a (Node a)
             | Fork a (Node a) (Node a)

This recursive type is the fixed point
<center><math>\mathit{Node}\,A = \mu X.\,\mathit{NodeF}_A\,X</math></center>
of the functor
<center><math>\mathit{NodeF}_A\,X = A + A\times X + A\times X\times X</math>.</center>
In other words, we have
<center><math>\mathit{Node}\,A \cong \mathit{NodeF}_A\,(\mathit{Node}\,A) \cong A + A\times \mathit{Node}\,A + A\times \mathit{Node}\,A\times \mathit{Node}\,A</math>.</center>
The derivative reads
<center><math>\partial_X(\mathit{NodeF}_A\,X) \cong A + 2\times A\times X</math></center>
and we get
<center><math>\partial \mathit{NodeF}_A\,(\mathit{Node}\,A) \cong A + 2\times A\times \mathit{Node}\,A</math>.</center>
Thus, the context reads
<center><math>\mathit{Context}_\mathit{NodeF} \cong \mathit{List}\,(\partial \mathit{NodeF}_A\,(\mathit{Node}\,A)) \cong \mathit{List}\,(A + 2\times A\times (\mathit{Node}\,A))</math>.</center>
Comparing with

 data Branch a  = KeepStraightOn a
                | TurnLeft  a (Node a)
                | TurnRight a (Node a)
 type Thread a  = [Branch a]

we see that both are exactly the same as expected!
{{Exercises|1=
# Redo the zipper for a ternary tree, but with differentiation this time.
# Construct the zipper for a list.
# Rhetorical question concerning the previous exercise: what's the difference between a list and a stack?
}}

=== Differentation of Fixed Point ===
There is more to data types than sums and products, we also have a fixed point operator with no direct correspondence in calculus. Consequently, the table is missing a rule of differentiation, namely how to differentiate fixed points <math>\mu F\,X = \mu Y.\,F\,X\,Y</math>:
<center><math>\partial_X(\mu F\,X) = {?}</math>.</center>
As its formulation involves the chain rule in two variables, we delegate it to the exercises. Instead, we will calculate it for our concrete example type <math>\mathit{Node}\,A</math>:
<center><math>\begin{matrix}
\partial_A(\mathit{Node}\,A) &=& \partial_A(A + A\times\mathit{Node}\,A + A\times \mathit{Node}\,A\times\mathit{Node}\,A)\\
&\cong& 1 + \mathit{Node}\,A + \mathit{Node}\,A\times\mathit{Node}\,A\\
&&+ \partial_A(\mathit{Node}\,A)\times(A + 2\times A\times\mathit{Node}\,A)
.\end{matrix}</math></center>
Of course, expanding <math>\partial_A(\mathit{Node}\,A)</math> further is of no use, but we can see this as a fixed point equation and arrive at
<center><math>\partial_A(\mathit{Node}\,A) = \mu X.\,T\,A + S\,A \times X</math></center>
with the abbreviations
<center><math>T\,A = 1 + \mathit{Node}\,A + \mathit{Node}\,A\times\mathit{Node}\,A</math></center>
and
<center><math>S\,A = A + 2\times A\times\mathit{Node}\,A</math>.</center>
The recursive type is like a list with element types <math>S\,A</math>, only that the empty list is replaced by a base case of type <math>T\,A</math>. But given that the list is finite, we can replace the base case with <math>1</math> and pull <math>T\,A</math> out of the list:
<center><math>\partial_A(\mathit{Node}\,A) \cong T\,A \times (\mu X.\,1+S\,A\times X) = T\,A\times\mathit{List}\,(S\,A)</math>.</center>
Comparing with the zipper we derived in the last paragraph, we see that the list type is our context
<center><math>\mathit{List}\,(S\,A) \cong \mathit{Context}_{\mathit{NodeF}}</math></center>
and that
<center><math>A\times T\,A \cong \mathit{Node}\,A</math>.</center>
In the end, we have
<center><math>\mathit{Zipper}_{\mathit{NodeF}} \cong \partial_A(\mathit{Node}\,A) \times A</math>.</center>
Thus, differentiating our concrete example <math>\mathit{Node}\,A</math> with respect to <math>A</math> yields the zipper up to an <math>A</math>!
{{Exercises|1=
# Use the chain rule in two variables to formulate a rule for the differentiation of a fixed point.
# Maybe you know that there are inductive (<math>\mu</math>) and coinductive fixed points (<math>\nu</math>). What's the rule for coinductive fixed points?
}}

=== Differentation with respect to functions of the argument ===
When finding the type of a one-hole context one does d f(x)/d x. It is entirely possible to solve expressions like d f(x)/d g(x).
For example, solving d x^4 / d x^2 gives 2x^2 , a two-hole context of a 4-tuple. The derivation is as follows
let u=x^2
d x^4 / d x^2 = d u^2 /d u = 2u = 2 x^2 .

=== Zippers vs Contexts===
In general however, zippers and one-hole contexts denote different things. The zipper is a focus on arbitrary subtrees whereas a one-hole context can only focus on the argument of a type constructor. Take for example the data type

  data Tree a = Leaf a | Bin (Tree a) (Tree a)

which is the fixed point
<center><math>\mathit{Tree}\,A = \mu X.\,A+X\times X</math>.</center>
The zipper can focus on subtrees whose top is <code>Bin</code> or <code>Leaf</code> but the hole of one-hole context of <math>\mathit{Tree}\,A</math> may only focus a <code>Leaf</code>s because this is where the items of type <math>A</math> reside. The derivative of <math>\mathit{Node}\,A</math> only turned out to be the zipper because every top of a subtree is always decorated with an <math>A</math>.
{{Exercises|1=
# Surprisingly, <math>\partial_A(\mathit{Tree}\,A)\times A</math> and the zipper for <math>\mathit{Tree}\,A</math> again turn out to be the same type. Doing the calculation is not difficult but can you give a reason why this has to be the case?
# Prove that the zipper construction for <math>\mu F</math> can be obtained by introducing an auxiliary variable <math>Y</math>, differentiating <math>\mu X.\, Y\times F\,X</math> with respect to it and re-substituting <math>Y=1</math>. Why does this work?
# Find a type <math>G\,A</math> whose zipper is different from the one-hole context.
}}

===Conclusion===
We close this section by asking how it may happen that rules from calculus appear in a discrete setting. Currently, nobody knows. But at least, there is a discrete notion of '''linear''', namely in the sense of "exactly once". The key feature of the function that plugs an item of type <math>X</math> into the hole of a one-hole context is the fact that the item is used exactly once, i.e. linearly. We may think of the plugging map as having type
<center><math>\partial_X F\,X \to (X \multimap F\,X)</math></center>
where <math>A \multimap B</math> denotes a linear function, one that does not duplicate or ignore its argument like in linear logic. In a sense, the one-hole context is a representation of the function space <math>X \multimap F\,X</math>, which can be thought of being a linear approximation to <math>X\to F\,X</math>.

== Notes ==
<references/>

== See Also ==

{{Wikipedia|Zipper (data structure)}}
* [http://www.haskell.org/haskellwiki/Zipper Zipper] on the haskell.org wiki
* [http://okmij.org/ftp/Computation/Continuations.html#zipper Generic Zipper and its applications]
* [http://okmij.org/ftp/Computation/Continuations.html#zipper-fs Zipper-based file server/OS]
* [http://www.cs.indiana.edu/~adamsmd/papers/scrap_your_zippers/ Scrap Your Zippers: A Generic Zipper for Heterogeneous Types]

{{Haskell navigation|chapter=Advanced Haskell}}
{{Auto category}}
