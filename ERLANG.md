# Logic

![Article](/assets/images/IMG_2741.png)

## About an article on Erlang

Translation of: Set Lonnert,
"Erlang - an old programming language with a future",
*Datormagazin* no. 3 2001.

This is from the original manuscript and not from the final
published version, so the editor might have changed minor things.
Translation has mainly been done through Google translate.

I worked as a freelance journalist for approximately 10 years
from around 1998 until 2008, with the focus on computers.
Here is an article from 2001 on Erlang, a little known language
at the time for the audience of hobbyists and other computer
interested laymen, as I was in the main also. But even if I wrote
about 80 articles in total, there must also have been mistakes.
In this one article there was. None of the others did not reach
any such responses or corrections.

In a reply Ulf Wiger, Senior System Architect at Ericsson,
writes:

> "Thank you for a nice article about Erlang. Unfortunately the
article ends with the assertion that Ericsson no longer develops
Erlang. This  is completely wrong." *Datormagazin* no. 4 2001.

He was obviously right. I learned the hard way that
__checking your sources__ is __always__ a requirement
before publishing. I wrongly trusted an argument I read
on the internet, having a discussion on Erlang.
Well, sorry bout that. Now over to the article.

![Response](/assets/images/IMG_2740.png)


## *Erlang - an old programming language with a future*

In the wake of things like broadband and integration with telephony, the Internet
requires better software. With faster connections, it follows that the systems
can cope with high availability, have fault tolerance, fast reactions, or be
almost linearly scalable to cope with the pressure. Several of today's systems
cannot handle that future at all.

To remedy this, there is reason to look and learn from areas that already have
extensive experience with systems designed based on high requirements such as
the aviation industry, telephony or banks. One component could be looking at
relevant programming languages. Ericsson has stitched together e.g. an AX switch
and database system (DBMS) using the Erlang language. This completely proprietary
language/system has several features that make it almost perfect for putting
together typical such applications.

### Features

When we make a phone call, we expect the connection to be quick and easy. You can
get annoyed if the phone doesn't work or the automatic exchange connects us incorrectly.
A system should therefore have real-time characteristics, be able to handle several
calls at the same time, or insert new software without having to switch off the exchange,
upgrade and then start the exchange again.

When Ericsson looked at programming languages at its development labs in the mid-80s
to manage its switches, no existing language really fit. They therefore constructed
their own language Erlang, named after a Danish mathematician. It is declarative,
but also functional language. This means that the statements that are written are
not classically imperative like Pascal or C. Rather, it is reminiscent of Prolog,
which Erlang was first implemented in. But unlike common implementations of Prolog,
it is constructed with only functions similar to, for example, Lisp.

As it is declarative, it also has the property that relatively complex programs
can be formulated with little means. Large teams of programmers have an easier time
working together to build complex systems. It is quick to learn. When Erlang was
designed, it was in collaboration with those who would use it. Unused stuff was
removed, simplification was added, like introducing new primitives to reuse
complicated structures. Among the ideas can be noted the module structure, error
handling or code loading without system shutdown.

Among the basic subtleties of the language is the use of simultaneous processes
(concurrent). Processes can be controlled for both parallel and sequential execution.
The processes communicate by exchanging asynchronous messages. This means that
programs have the possibility, among other things, to be distributed over several
machines (nodes), and the processes can communicate with each other in parallel.
All this is basically why complicated applications such as distributed database
systems use ready-made primitives. Erlang also has a built-in time perception
against milliseconds (soft real-time programming). Tasks are easily timed with
timers. Erlang is thus designed from the outset to handle errors that may occur
in a process. We cannot, for example, wait any length of time for a call to be
disconnected. Software bugs can cause a process to hang, becoming unresponsive.
Erlang can still continue running, perhaps killing the process, or restarting it,
letting the part that was running in the process continue elsewhere. None of the
rest of the system is affected by this, since no common address space for the
processes is used.

### Example

A trivial textbook example in programming languages is usually integer factorization.
In figure 1 you see how such a thing looks in Erlang. The first line names the module
with fact. The easiest way is to save it as a file with the same file name as the module.
An initial divi character marks that it is a so-called attribute. The sentence ends
with a period. The next line contains a statement that says that a function "factorial"
with one argument can be accessed from the outside, from other modules.

Figure 1
```erlang
-module(fact) .
-export([factorial/1]).

factorial(0) ->
  1;
factorial(N) ->
  N * factorial(N - 1).
```

The calculation itself is very similar to the mathematical formulation in figure 2.
When testing the module/function, e.g. "fact:factorial(34)." in the running environment
(shell) that exists. Erlang tries to pattern match "factorial(34)" against "factorial(0)"
and fails. But against "factorial(N)" where N is a variable, the match succeeds and N
assumes the value 34. When the match succeeds, Erlang can continue after the -> symbol.
Here it is tasked with recursively working on 34 multiplied by the factorization of
34 minus 1, etc.

Figure 2
```math
n! = \begin {cases}
1 & : n = 0 \\
n ({n - 1})! & : n &gt; 0
\end {cases}
```

A more refined way of calculating factorization is shown in figure 3, with so-called
guards, or expanded in figure 4 with both guards and local temporary variables that
are not accessible from the outside.

Figure 3
```erlang
-module(fact1).
-export([factorial_1/1]).

factorial_1(0) ->
  1;
factorial_1(N) when N > 0 ->
  N * factorial_1(N - 1).
```

Figure 4
```erlang
-module(fact2).
-export([factorial_2/1]).

factorial_2(N) when N == 0 ->
  1;
factorial_2(N) when N > 0 ->
  N1 = N - 1,
  F1 = factorial_2(N1),
  N * F1.
```

### Erlang/OTP

Erlang is now available as open source code to download. An accessible
environment for experiments during e.g. Microsoft Windows is Erlang/OTP
(open telecom platform). This platform means that certain standards,
libraries and other components are assumed. If you start the environment,
it looks like in the picture below.

![Layout](/assets/images/bild5.bmp)

This is the shell you communicate through. Here you can interact with
Erlang directly, for example calculate arithmetic expressions directly.
Compiling and loading a Tetris game is provided with a simple command
`c(tetris).`. Because an internal library is used, some GUI components
are slightly different from the standard components under Windows,
as shown in the figure below.

![Tetris](/assets/images/bild6.bmp)

Since Erlang is distributed and can be run over several nodes (machines) over the
Internet, e.g. it is possible to check the status (such as load balancing) of the
various nodes. In picture 7, a node can be seen here without a special name. The
processes can be shown as in image 8, and in more detail by double-clicking the
process <0.21.0> in image 9. In addition to process management, there are also
debugging tools in the environment.

![Layout](/assets/images/bild7.bmp)

![Layout](/assets/images/bild8.bmp)

![Layout](/assets/images/bild9.bmp)

Erlang is similar to Java in several ways. There is an automatic garbage collector,
so that the program does not have to keep track of memory references. So better
security for memory leaks than e.g. pointer structures under C/C++. It also has an
Erlang Runtime system (ERTS) virtual machine. This means that basically the same
program can be run on different platforms/operating systems without changing anything.
Programs are robust as they cannot crash the entire emulator/virtual machine. The
disadvantage, of course, is that programs become slow when they are first semi-compiled
and then interpreted.


### Mnesia

An application that demonstrates several points of Erlang is the database manager
Mnesia. Mnesia acts as a direct evolution of Erlang. The database works with the
data structures that are built in, with the fault tolerance, the distribution and
even the language itself functions as a database language. All components like
transaction manager, replication, logging, primary and secondary memory storage
backup etc. are managed in Erlang. Since Mnesia is designed for telecoms, telephone
exchanges, it handles complex values, such as adding a new subscriber (when multiple
tables are likely to be updated). But there is also speed such as forwarding calls
(routing), with fast table lookups. Since Mnesia can manage only RAM, there is no
need for slow disk writes or reads. Backup can, for example, be handled by other
processes that write and read from disk. The fault tolerance built through, among
other things, the distribution (possibly to several machines) and the replication
(both to RAM and to disk) means that availability will be very high.

Unfortunately, Ericsson has stopped its development of Erlang, and refrains from
writing applications in it in the future. Many of the group that worked with Erlang
left Ericsson and formed Bluetail in 1999. Bluetail was recently sold last August
to Alteon WebSystems. Perhaps no company is eager for Erlang anymore, but many
others are now starting to discover it. There is still reason to learn highly
useful ideas from the project, or volunteer enthusiasts give it a continued future.
At e.g. The research institute SICS has a project that aims to be able to
automatically verify parts of programs written in Erlang. Researchers also believe
that more insecure languages such as PLEX and C may gain in the short term in terms
of efficiency (speed), while Erlang may gain in more secure properties.


*Set Lonnert*


#### Downloads:

* Official website: documentation, source code, compiled Erlang/OTP for e.g. Microsoft Windows: http://www.erlang.org/
* Bluetail: http://www.bluetail.com/ (today defunct)
* SICS verification project: http://www.sics.se/fdt/Erlang/ (redirects to RISE, also government projects)
* Several useful links: http://dmoz.org/Computers/Programming/Languages/Erlang/ (today defunct)
* Downloadable book about Erlang, and examples: http://www.serc.rmit.edu.au/~maurice/erlbk/ (today defunct)
