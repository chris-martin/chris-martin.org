--------------------------------------------------------------------------------
title:    In the computer
date:     2020 Sep 9
slug:     in-the-computer
abstract: It's so simple
--------------------------------------------------------------------------------

When people talk about "algebraic reasoning" explanations fall flat because we neglect to first figure out kind of reasoning we're contrasting it against. When we write computer code using an operational model, we do *think about* what we write -- so what manner of reasoning are we using? Can we give it a name? And can we explain why it seems so incompatible with the sort of reasoning that Lambda Man is always going on about?

## Metaphors we code by

I propose we follow the approach of George Lakoff and look to programmers' phrasings for evidence that operational and algebraic programmers apply different cognitive metaphors to the understanding of what programming *is*.

We should try to not lend much weight to prescriptive jargon. Both Java and Haskell have a word 'class' with disparate meanings; but these were choices made by individual language designers, and I do not believe that the obedience of language users in adhering to the architect's terminology (especially when the word in question is etched as a keyword into the language specification itself) is necessarily a signifier of cognitive metaphor at play, as in such cases we often stick to the standard terminology begrudgingly even if it *conflicts* with cognitive metaphor. Given Python's use of the `func` keyword, I have little choice but to refer to the entity it declares as a "function", regardless of whether this word matches my understanding of it. Therefore no one's use of the phrase "a Python function" reveals much of anything about the metaphors by which they understand Python functions; the phrasing was merely preordained.

Instead we should study the *informal* ways we talk about code. This includes a great wealth of terminology that is also standardized, just not formally so. Most of the rules of this sort need to be learned but are never taught -- because most of us who know them have never noticed them -- so you can readily find them in conversations with novices who are grasping the definitions but have not yet learned to talk like "a local". There is perhaps no better source of examples than prepositions. We apply a function *to* an argument; we do not apply a function *upon* an argument. `map :: (a → b) → List a → List b` represents the application of a function *over* a list -- not *throughout* the list, *into* the list, or *alongside* the list.

## Inside and out

I believe that the culture of programming at present may be divided into two approaches, explained by the following competing metaphors for the act of programming:

  1. An operational programmer *goes into* the computer;
  2. An algebraic programmer remains *outside* the computer.

I want to be careful to avoid conflating this with the orientational metaphor *concrete is down, abstract is up*. Our understanding of concrete/abstract being structured from the physical basis of down/up is why we may describe a C programmer as "close to the metal" and an Idris programmer as a "type astronaut". But I am *not* asserting that inside/outside is the basis of our understanding of operational/algebraic reasoning. *Programming inside* and *programming outside* are two different -- and conflicting -- metaphors. And many of us become entrenched in one or the other.

## Who calls the calls

To start piecing together the orientation of each kind of programmer with respect to the computer, I'd like to look at a curious question of agency regarding function calls.

  - When I write the expression "f x", I may describe my act of programming by saying that "I **apply** the function *f* to an argument *x*".
  - When the machine executes the program, I expect that "it will **evaluate** the function *f* at the argument *x*".

But this separation between my action as an author and the machine's action as an automaton is only so distinct in the parlance of an algebraic programmer. If I were a Python programmer:

  - When I write the expression "f(x)", I am "calling the function *f*".
  - When the machine executes my program, I expect that the Python interpreter will "call the function *f*".

So who calls the function: me or Python? The answer is both; I *am* Python, and its actions are my actions, regardless of whether I am present and typing into a REPL or whether I have scripted them out ahead of time in a program that may run in my absence.

## One with the machine

In either variety of programming, we sometimes put ourselves in the shoes of the machine to reason about the anticipated outcome of what we write. But how this imagination works depends on a great deal on whether we are outside or in. When we trace an operational program flow, the text of the program forms a space we can move within, and each variable is a statue that comes alive and begins to talk. Algebraic expression evaluation is a much more sterile and dull affair, and we remain seated in our desk chair. *Evaluating* is rewriting an expression in another form; it does not take us into different headspace from the one in which we wrote the code in the first place.

## Removing the lime from the coconut

When an experienced inside-the-computer author begins in a programming languages that forces us to approach programs from the outside, we can expect the question: If "I have an `IO String`", then "how do I get to the `String`"?

While others have already addressed this question in detail, what I want to draw attention to here is that the misunderstanding originates from trying to apply the *inside* conceptual mapping to a programming model that is strongly *outside*. An `IO String` is a process that produces a `String` result. So if I *were* standing inside a Haskell program, holding such a thing in my hands, it stands to reason that I could run the process and get the string. But we do not *have* such values because we do not *go* inside to *get* anything. We remain at the text editor, writing definitions. One such definition might be for a process which consists of 1. first running some `IO String` process; and then 2. doing some other action with the resulting string.

This is not an unfamiliar task for a JavaScript programmer, who knows that you cannot get the value from a Promise -- all I can do is set up plans for what to do once the Promise is fulfilled. A JavaScript programmer, although inside of the computer, is outside of the event loop. When my callbacks are roused, I do my work, then I fall back sleep to await another gig.

## What you got in that room

The term "global variable" reveals something interesting about our mental picture. Such a variable does not span the globe, nor even a local network. To what scope does a word so grand as "global" refer? Humbly, the scope of a process. Or perhaps an entire machine, if I am a kernel developer. When I code operationally, I reside in a tiny world -- the landmasses on my little blue marble are the memory segments to which I have access.

When I switched from operational to algebraic programming, first I learned that there are no global variables, then that terminology just faded from consciousness. As a Haskell programmer, I'm not in a little globe on the desk; I live on the Earth and I type definitions. Among those definitions may be a datatype that represents the state of a process, true. But this datatype is not my world, and the vast majority definitions I write in service of the program will not be functions of it.

Private "member variables" in a Java class can only be accessed *from within* the class. Perhaps the preposition can be taken to refer somewhat literally to the lexical scope of the class definition -- that is, the code that is written between the opening and closing braces. But do we employ a deeper container metaphor here? Maybe this one is just me, but I see the *instance* as a *room*, and the members as the stuff I have at my disposal when I'm working inside that room.

In Haskell we also have lexical scoping, as well as a notion of modules with definitions that are either exported or not exported, which for many purposes mirrors the public/private field distinction. But I do not have the experience of mentally going inside a module in the same sense as reasoning inside of a Java instance. I believe it is Java's coupling of modules with mutable state that encourages this spacial reasoning.

## Getting your steps in

When you use a step debugger, you actually *step into* the program! This is true regardless of whether you are using the debugging facilities of Python or Haskell. Though the code may be algebraic, when we use a step debugger we are always looking at it from an operational perspective.

## Working on documents

Lately I like to refer to my role as "author" more than "programmer" -- regardless of whether the file extension is ".md" or ".hs". It's because I don't feel like I work inside a computer anymore. I work sitting *at* a computer, I write *about* programs, and -- although much of what I write can be executed by a machine -- I no longer get lost inside it.
