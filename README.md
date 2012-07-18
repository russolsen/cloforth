cloforth
========

A little Forth-like language implemented in Clojure

For a couple of hundred lines of Clojure, Cloforth
is actually pretty capable. It can do basic arithmetic,
it has if statements, you can define new procedures
(new 'words' in the jaron of Forth). The most
interesting part is that the language is built
around a compile/execute cycle: All Cloforth
code, even the stuff that you type in to the
REPL is compiled into an vector of functions
before it is executed.


Cloforth is a stack oriented, postfix language.
What this means is that numbers just get
pushed onto the stack, so:

3

Will simply push 3 onto the stack, while

3 5

Will first push 3 then 5 onto the stack. The
typical operators will pop a couple of numbers
off of the stack, operate on them and then
push the result back on, so that:

3 5 +

Will leave you with 8 on the stack.

Cloforth commands are called 'words'. So there
is an 'nl' word which just prints a newline.
So do this:

nl

And you will see a blank line printed. Another
word is the plain old dot: . Yes, it's not much
of a word, but that is the jargon. The . word
justs prints whatever is on the top of the stack
(popping it off in the process) Thus if you
do this:

3 . nl

You will see a three followed by a newline printed.

Other handy words are dup, which pushes a copy of
whatever is on top of the stack back onto the
stack, so that

3 dup . nl

Will print the three and also leave it on top of
the stack.

You can define your own words with :
Colon takes a name and a list of words
enclosed in [ ] and defines a new word.
Thus this:

: plus1 [ 1 + ]

Defines a new word called 'plus1' that adds
one to whatever is on the top of the stack.




