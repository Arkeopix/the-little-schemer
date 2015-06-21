# The little schemer

This is the repository where i'm going to to put all the things I do in order to learn lisp.
Basicaly, it's a huge blob of lisp and a (hopefuly usefull) readme.

## why ?
Lisp, and by extension the whole family, are taking the piss with me. They tempt me with all
their shiny things like S-expressions, functionnal paradigm, maccros, code as data etc...
And that's without the gazzilions of success story about lisp and why it is the best language
in the world, and how it will make me feel the universe in a totaly new fancy way
(seriously, go over [this page](http://learnlispthehardway.org/book/1-0-0-overview/) and lol a
little at the definition of groking). So i decide to learn a Lisp. I then proceed to
download [SBCL](http://www.sbcl.org/), setup my emacs and start looking for a guide or a
tutorial, or idealy one of those nice books with cool names like "learn you some * for great good".

One of the major problem is: for the past three years i've been trying to learn Lisp, and it
always has been a faillure! Maybe i'm just plain stupid or maybe good ressources concerning lisp are hard to comme by ? Who knows ?

Anyway, this time i decided to document my learning process. maybe writing about it will help me focus/understand better.
May it serve some other poor souls wandering before the gates of Lisp land.

## Usefull ressources
for the past tree years (i'm writing this in 2015) I did found some stuff worth looking at. I will
tell how i felt about them. If you do know other cool Lisp stuff, please do not hesitate do make a PR to
improve this readme.

And without furter ado, the list:
- [learn lisp the hard way](http://learnlispthehardway.org/): which looks absolutely awesome, i go through the
first chapter and start thinking this is the book i need ! but wait, where is chapter 2 ? and 3 ? and 4 ? well,
the book is WIP and seems dead (apologies to the owner/maintainer if i'm wrong). 
- [practical common lisp](http://www.gigamonkeys.com/book/) which is absolutely awesome but also absolutely brutal
with the new-commer. I just felt realy overflowed by all this information and gave up quickly
(plus my short attention span did not help)
- [the little schemer](http://kysmykseka.net/koti/wizardry/Programming/Lisp/Scheme/The%20Little%20Schemer%204th%20Ed.pdf)
is by far one of the best ressources i've read about Lisp. It's also a good introduction to recursion.
i'm not done yet, but as far of chapter four, the recursion demonstrated in the book is not terminal.
- [Project Euler](https://projecteuler.net/about)
maths exercises. lots of them.
- [PTHW](http://projectsthehardway.com/)
a bunch of cool project to get you started with some real stuff, instead of computing primes and what not


### The little schemer
you may have noticed the precence of a file
[the-little-schemer.lisp](https://github.com/Arkeopix/the-little-schemer/blob/master/the-little-schemer.lisp) in the repositoty. I'm making the exercices of the book in this file. Here follows the rules i found in the book.

####The First Commandment 

>>When recurring on a list of atoms, lat , ask two questions
>>about it: ( null? lat) and else.
>>When recurring on a number, n , ask two questions about
>>it: (zero ? n) and else.
>>When recurring on a list of S-expressions, l, ask three
>>question about it: ( null? l ) , ( atom ? ( car l) ) , and else.

####The Fourth Commandment
>>Always change at least one argument while recurring.
>>When recurring on a list of atoms, lat, use ( cdr lat ) . When
>>recurring on a number, n , use (sub1 n) . And when recurring on a list of S-expressions, l , use ( car l) and ( cdr l) if
>>neither ( null ? l) nor ( atom ? ( car l)) are true.
>>It must be changed to be closer to termination. The changing argument must be tested in the termination condition:
>>when using cdr , test termination with null ? and
>>when using sub1 , test termination with zero ?.

####The Sixth Commandment
>>Simplify only after the function is correct.

####The Seventh Commandment
>>Recur on the subparts that are of the same nature:
>>• On the sublists of a list.
>>• On the subexpressions of an arithmetic expression.

####The Heighth Commandment
>>Use help functions to abstract from representations.

### Euler
in case anyone is reading this: if you're going though euler project, do not check my solution to the problem
you are solving, you would miss the point of this great project
Project [Euler](https://projecteuler.net/about) is an awesome collection of math problems, which you must resolve
using whatever the fuck you want, for me it's lisp as i'm trying to learn it.
I will probably never finish all of the challenges, but at least enough to get Lisp, i hope

### Practical common lisp
quick notes about things i deem important:

#### S-Expressions
They're composed of atoms and list:
- lists are delimited by parentheses
- atoms are everything else

Atoms may be:
- Numbers -> 244, 3/7, -1, +42 are numbers
- Strings -> "Antipop", "Frizzle\"fry" are strings
- Names -> format, hello-world *db* are names they are represented by objects called symbols

#### Special operators
>>          (if x (format t "yes") (format t "no"))
>>If IF were a function, the evaluator would evaluate the argument expressions from left to right.
>>The symbol x would be evaluated as a variable yielding some value; then (format t "yes")
>>would be evaluated as a function call, yielding NIL after printing "yes" to standard output. Then
>>(format t "no") would be evaluated, printing "no" and also yielding NIL. Only after all three expressions
>>were evaluated would the resulting values be passed to IF, too late for it to control which of the two
>>FORMAT expressions gets evaluated.

this is why there are 25 special operators built in the language

'if' is one of them
'quote' or ' is another one

#### Truth, Falsehood, and Equality
everyting other than nil is true. the cannonical way of reprenting truth is t
nil is the only object that is an atom and a list at the same time, sa we use nil to represent empty lists

we test equality with:
- = for numbers
- char= for characters
- string= for strings
- and so on

other than that, (there are eq, eql, equal and equalp)[http://stackoverflow.com/questions/547436/whats-the-difference-between-eq-eql-equal-and-equalp-in-common-lisp/547451#547451]:
-eq tests for object identity, which means we can use it to check whether two object are the exact same (eq 'a 'a) true, (eq 1 1) unsure
-eql tests behaves in the same way, plus ot compares classes and value so that (eql 1 1)is true and (eql 1 1.0) is false
-equal is the same as eql, onmly more tolerant
-equalp is tolerant++

#### Formatting Lisp Code
comments -> ;;;; header text, ;;; paragraph, ;; "block" specific, ; line specific

### PTHW

####Logfind
So i learned quite a lot about lisp doing this exercise: scope, macros (which i finaly did not use)
