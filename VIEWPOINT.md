# Logic


## A personal experience: theoretical philosophy and computers

From the east, a bright light from the sun shines on the bright yellow facade
of the university building. It is Monday morning, July 2, 1984, and the *Second
International Conference in Logic Programming* is about to begin in Uppsala ...

What was I expecting? When I started my studies at the university in the
spring of 1982, I had an idea that *philosophy* could be combined with the
science of *data* and *programming* in some way. Ignorant as I was, however,
I had no idea *how* they were to be united. The conference, on the other hand,
clearly declared in the title that logic, which had its historical connection
to philosophy, and programming to the data area were part of a kind of fusion:
*logic programming*. Exciting! After starting with theoretical philosophy,
I found out that I had also applied and been accepted for a systems science
course (systemvetenskap) in Stockholm. Well, maybe it can wait, I thought.


### The 1984 Logic Programming conference

With some surprise I found *no one* from the philosophy department present at
the conference. On the other hand, there were several people from the department
under the professor of computer science, Sten-Åke Tärnlund. Several promising
students came a few years later to go over to Japan with a substantial salary
increase to participate in the new construction of the fifth generation computer
technology project. The Japanese state invested huge sums through its Ministry
of Trade and Industry (MITI) in the construction of knowledge systems, expert
systems, artificial intelligence (AI) from a more logical perspective as a base,
large databases rather than file systems, new parallel supercomputers with a
symbolic basis rather than arithmetic, as well as logic programming. In the
future, the computers would have conversations with us, reason like us, translate
languages, or interpret images. At this time, Japan was seen as a miracle of success.
It was cheap electronics, cheap cars that washed over the West but were no longer seen
as copies but often of better quality than what Europe or the US could produce.
Some researchers in the United States feared that Japan did not stop at cars or
electronic watches, but also took over computer development and thus future
industrial and research development. So it was speculated that "[...] the next
generation of computers think, reason, and speak – in Japanese.".[^1]
Artificial intelligence (AI) researcher Edward A. Feigenbaum and journalist
Pamela McCorduck reacted fiercely, with the American response to the Japanese
challenge, they write in their book *The fifth generation: artificial intelligence
and Japan's computer challenge to the world*: “In the end, we have no choice. We
can decide when we shall participate, not if.”[^2] In hindsight, however, the entire
ten-year Japanese project turned out to be a phenomenal failure. Fifty billion yen
costs hope with meager results.[^3] There are probably many different explanations
for why it was a failure. One reason that has been pointed to is that the main
exponential development of computers and processors better known under the name
'Moore's Law', also constantly drove down the price of the hardware.[^4] Thus
no special hardware or software was necessary at the time or the near future.
The project with the fifth generation aimed, among other things, to develop
expensive special hardware with parallel working processors which in the long
run could not be justified or compete with the main development.[^5]
Whatever the reasons, the hot streaks of optimism cooled over time.
The answer to the Japanese project was the US's competing effort
'Strategic Computing Initiative' backed by the military's department
for technical development DARPA (Defense Advanced Research Projects
Agency). But the SCI venture met the same fate as the Japanese one and shrank
drastically financially towards the end of the 80s.[^6]

[^1]: From the back cover text of the paperback edition of Edward A. Feigenbaum,
& Pamela McCorduck, The fifth generation: artificial intelligence and Japan's
computer challenge to the world, Rev. & upd. ed., Pan Books, London, 1984.

[^2]: Ibid, p. 290.

[^3]: Wikipedia, "Fifth generation computer,"
https://en.wikipedia.org/w/index.php?title=Fifth_generation_computer&oldid=891491126
(retrieved 2019-04-09).

[^4]: Wikipedia, “Moore's law,”
https://en.wikipedia.org/w/index.php?title=Moore%27s_law&oldid=891671193
(retrieved 2019-04-11)

[^5]: Wikipedia, “Lisp Machines,”
https://en.wikipedia.org/windex.php?title=Lisp_Machines&oldid=871717316
(retrieved 2019-04-13)

[^6]: Wikipedia, “AI winter,”
https://en.wikipedia.org/w/index.php?title=AI_winter&oldid=891874138
(retrieved 2019-04-11)


### Philosophy at the edges

Maybe my professor of theoretical philosophy Stig Kanger had already foreseen
the problems, or maybe he wasn't interested. Kanger would not call himself an
analytical philosopher, but he was clearly a very great logician. He was warm,
kind but a little tight-lipped. As a matter of principle, all his work had been
in the formal logical arena. He was very clear about what he liked and
didn't like. I had only been a student for two years in 1984 and cautiously
walked up the stairs to his room to ask how I should approach the paper you'll
have to write before starting with a doctoral thesis.
I knocked, still convinced that there should be some connection between the
technicalities of data and the philosophy, I ventured to suggest that I read
*Logic for Problem Solving* by Robert Kowalski. In his textbook, Kowalski
shows the connection of logic to programming through such things as a
certain formulation of logical propositions in Horn clauses, inferences,
matching between propositions or the interpretation of negation as failure
of proof. Everything possible seemed to be practically programmable in the
Prolog programming language. Kanger retorted that he had never heard of the
author and headed for the bookshelf to quickly pull out a book that I should
read instead. He suggested Rudolf Carnap's *Meaning and Necessity*. Somewhat
disappointed, however, I trusted Kanger's recommendations completely.
In retrospect, I can add that he was also right. I read through Carnap,
who was a phenomenal thinker, but couldn't say I had anything to add.
Later, however, I instead came to write about the medieval philosopher
William Occam and the assertion (assertio), also at the suggestion of Kanger.
Although I got the nice offer to start a PhD with Professor Jaakko Hintikka
at Stanford after Kanger seemed to appreciate my efforts, my interests had
now started to lean more towards history and the history of philosophy.
Partly as an effect of reading texts by Occam, partly by another teacher
that got me interested in history: Thorild Dahlqvist. But after studying more of
the history of philosophy, the history of ideas and learning, as well as the
cultural and social life of antiquity during the latter part of the 1980s,
my interest in practical programming and philosophy had not completely
stopped. I went to UPMAIL seminars at the university where many different
kinds of logic and programming were discussed, sometimes hosted by philosophers
such as the professor of theoretical philosophy in Stockholm Dag Prawitz,
but mostly by computer scientists who organized the seminars.


### Short interlude: Early AI and theorem provers

In the summer of 1956, John McCarthy, together with among others Herbert A. Simon,
Marvin Minsky, Claude Shannon and Nathan Rochester will outline a conference on a
topic which McCarthy first calls 'artificial intelligence'.[AI] It contains several
parts that will be part of the discipline in the future, such as neural networks:
"How can a set of (hypothetical) neurons be arranged so as to form concepts."[^Concepts]
Simon is so convinced of the power of the new machines that he sees how the eternal
philosophical body-consciousness problem is now solved, how machines can now think,
learn or create.[^Simon] Simon had earlier in 1956 succeeded in proving certain theorems
from Alfred A. Whitehead and Bertrand Russell's groundbreaking work in logic
Principia Mathematica with his and Allen Newell's 'Logic Theorist'[^Theorist] program.
Other breakthroughs came later with the program 'General Problem Solver'[^General]
in 1959. One can very well have an understanding of the researchers' view that they are
drawing long lines into the future based on the results they have now shown,
that soon many tough problems could soon be solved. They predicted a bright
future for these machines.

[^AI]: Wikipedia, “Logic Theorist”,
https://en.wikipedia.org/w/index.php?title=Logic_Theorist&oldid=875003976
(retrieved 2019-04-11).
Wikipedia, “Dartmouth workshop”,
https://en.wikipedia.org/w/index.php?title=Dartmouth_workshop&oldid=878151960
(retrieved 2019-04-11).
John McCarthy, et. al. “A proposal for the Dartmouth summer research project
on artificial intelligence”, August 31 1955,
http://www-formal.stanford.edu/jmc/history/dartmouth/dartmouth.html
(retrieved 2019-04-11)

[^Concepts]: John McCarthy, et. al.
“A proposal for the Dartmouth summer research project on artificial
intelligence”, August 31 1955,
http://www-formal.stanford.edu/jmc/history/dartmouth/dartmouth.html
(retrieved 2019-04-11)

[^Simon]: Wikipedia, “Chinese room”,
https://en.wikipedia.org/w/index.php?title=Chinese_room&oldid=891604484
(retrieved 2019-04-11).
CHECK SOURCE Russell, Stuart J.; Norvig, Peter (2003), Artificial Intelligence:
A Modern Approach (2nd ed.), Upper Saddle River, New Jersey: Prentice Hall,
ISBN 0-13-790395-2. PAGE 18.

[^Theorist]: Wikipedia, “Logic Theorist”,
https://en.wikipedia.org/w/index.php?title=Logic_Theorist&oldid=875003976
(retrieved 2019-04-11)

[^General]: Wikipedia, “General Problem Solver”,
https://en.wikipedia.org/w/index.php?title=General_Problem_Solver&oldid=856579631
(retrieved 2019-04-11)

Here we arrive at an exciting intersection in history with the two Swedes I have
already mentioned: Dag Prawitz and Stig Kanger. Kanger wrote about automatic
proofs of theorems via the calculus LC in his treatise *Provability in Logic* in
1957.[^Provability] But Kanger did not go further with clear proofs or other
clarifications. Prawitz, on the other hand, came to work in the same year
on a method for performing automatic proofs. Not only did he describe the method,
but he codified the method into a program with the help of his father Håkan Prawitz,
among others. This means that "the first experiments with general theorem
provers for first-order logic were performed in Stockholm in 1958".[^First]

[^Provability]: Stig Kanger, *Provability in Logic*, Vol. 1 of Studies in Philosophy,
Almqvist & WIksell, Stockholm 1957.

[^First]: Degtyarev, Anatoli & Voronkov, Andrei. (2011).
“Kanger’s Choices in Automated Reasoning”. 10.1007/978-94-010-0630-9_4.
In Holmström-Hintikka, Ghita & Lindström, Sten & Sliwinski, Rysiek.
(2001). Collected Papers of Stig Kanger with Essays on His Life and Work:
Vol. II. 10.1007/978-94-010-0630-9 Page 53.



### Disillusioned

So my thinking today lean towards that Kanger might (might!) later
deliberately avoided any encouragement to even think about philosophy and
computers. Well ... so while I drifted towards historical subjects at
the university, my interests also still was with computers. However
those were large, quite out of reach for a student in philosophy.
After various kinds of attempts to fuse computers and philosophy
during the 1980s in attempts to understand, convince, argue, discuss
with everyone I could even if I had little faith they were interested,
*but all with setbacks*.
I was lost and doubted whether combination really existed or
could exist. Was the combination of philosophy and computers that I imagined just
a wishful dream? I also grew increasingly tired of higher seminars which seemed to
me to be getting nowhere. I was getting tired of university courses.


### From logic to connectionism ...

During the spring of 1992, however, the then docent in theoretical philosophy
Sten Lindström had a small course in AI. He had introduced me to model theory
and Kripke semantics in another modal logic course exactly ten years earlier.
It was possibly the first ever AI course at the institution (not really close,
but although I saw in the possible courses under Kanger there was one in
'automata theory'). Lindström went through common problems such as consciousness,
different ideas about how AI is defined and what it is, but also arguments from
John Searle questioning whether the computer or AI really understands or has thoughts.
But what was particularly interesting to me at the time was that Lindström
was the very last to take up a direction of what was then called *connectionism*.
In his lecture connectionism is contrasted with classical AI. Lindström writes
in the lecture notes: "Classical AI wants to construct computers that can represent their
environment and that can reason about this environment using formal logical rules."
This is what they call good old fashion AI (after the English concept of GOFAI or
“good old fashion AI”). This takes place through the implementation of specified
functions, which become algorithms in the computer. On the other hand, connectionism
rather tries to *construct computers that mimic the brain in neural networks*:
"While classical AI sees intelligence mainly as symbolic thinking, connectionists
emphasize learning and adaptive behavior." Lindström then describes examples of
how an artificial neural network is structured, but limits the networks to a
single layer and a special case of backpropagation in the delta rule.
