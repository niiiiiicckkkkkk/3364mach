# 3364 MACH

# Project Motivation
In my last semester of college I took a Laboratory Electronics Course. Twice a week we built circuits starting with a voltage divider and frequency filters all the way to an opamp, PID controller, a few logic gates and finally a programmable microcomputer on a FPGA. While working on the last few labs dealing with digital logic, I noticed a lot of questions about the DFF, registers, and memory. This class had students coming from many programs and majors and without the 2 semester long courses on computer architecture, it was difficult to stare at the Verilog/Lab spec and understand how signals and information moved through the system.

# Goals

I want to create a software simulation tool for reading in assembly for our custom microprocessor instruction set, converting it to machine code and simulating the execution: allowing users to step through, rewind and inspect the various signals and updates as they would happen on the FPGA chip.

# Project Status

## Parsing / Loading
- [x] parse assembly files including labels and numeric literals
- [ ] options for hex arguments
- [ ] track error location for better UX on parse failure
- [ ] VSCode extension (syntax highlighting) for assembly files
- [ ] warnings when arguments overflow the memory location


## Binary Representation
- [x] binary type class that supports "safe" conversion between values of different length
- [x] debug type class the supports printing binary information as binary, signed numeric value or unsigned numeric value
- [ ] support hex printing in debug type class


## Simulation

- [x] clocked registers (PC, AC, IR) input and output
- [x] addressable memory (256 addresses - 16 bit wide) with input and output
- [x] fetch / decode / execute for the ~10 instructions supported
- [x] ability to rewind the state of simulation

## Debugging / UX

- [x] command line file path for parsing and loading
- [x] quit, print machine state or step (simulate one rising clock edge)
- [ ] stepping backward
- [ ] breakpoints / running until some condition
- [ ] run the entire program to completion
- [ ] support the OUT instruction through some kind of IO
- [ ] select which machine components to print
- [ ] colored output

# Implementation
for anyone interested  :)

## Why Haskell?

Of course there are many technical reasons I find Haskell compelling. The type system is great, and as software gets larger and more complicated, having an additional (but sound) set of eyes on the entirety of a program's specification through type declarations is an invaluable tool. I've also found functional paradigms promote the greater use of generics and decomposition of repeated patterns into functions. This makes programs modular and (in my opinion) far easier to refactor.

A friend once jokingly said "oh no he's joined the Haskell cult" but I truly believe Haskell is under appreciated. However, I ultimately write Haskell because it's fun. Coding in any language that supports a functional paradigm is so expressive I feel limited in my approach to a problem only by my imagination. The ability to think through 100 and then try 100 more approaches to a problem in order to find a great one excites me and Haskell gives me the tools to do just that.

## Parsing with Applicative Functors

Even though assembly is not recursive and therefore a regex solution would have worked just as well, I found Haskell's canonical parsing ecosystem too interesting to pass up.

In Haskell a parser is modeled as a function that captures the idea of taking textual input and turning it into some output, the rest of the text all wrapped in a "Maybe" to recognize the possibility of failure.

``` type Parser a = String -> Maybe (a, String) ```

What makes this approach different from parser generators in other languages is that there parsers can be combined and threaded together to make more complicated parsers. As an example I'll discuss simplified pseudocode for my assembly instruction parser. 

```
insnP = Insn <$> wsP (optional labelP) <*> opP <*> operandP
	where
		Insn :: Maybe Label -> Opcode -> Operand -> Insn
		labelP :: Parser Label
		opP :: Parser Opcode
		operandP :: Parser Operand
		-- In this case f would specialize to Parser
		<$> :: (a -> b) -> f a -> f b
		<*> :: f (a -> b) -> f a -> f b
```
In this example I've combined 3 different parsers for labels, opcodes and operands into one for instructions using fmap (<$>) and ap (<*>). Loosely speaking, fmap "lifts" a function over a structure, applying the function to the enclosed value. Ap describes some method of combining a function contained in Applicative structure with a value contained in Applicative structure which necessitates some merging together of the enclosing structures andddd I'm definitely not qualified to explain these ideas. But what I can say is that these are two higher order functions that allow me to use functions and change the way a parser behaves. So instead of writing a parser generator rule for instructions, I've written 4 functions each of which can (and is) reused elsewhere in the program but combined through lawful type class instances to parse entire instructions.

## Type Level Safety for Binary Conversion

One of my favorite parts of Haskell is the strong type system which converts whole classes of runtime errors to compile time errors. Turns out, tons of programmers feel the same way and there are a lot of Language Extensions that can be used to take this idea to another level.

My first model of Binary data was a linked list of boolean values

``` type Binary = [Bool] ```

space concerns aside, this wasn't terrible. All operations from the simulator could be done in time linear in length of the Binary data and I liked the fact that my underlying Binary model was capable of working on data of arbitrary length (even infinite) only scaling down as necessary inside the simulator. Where this solution wasn't particularly satisfying is that it's not obvious how list operations should impact Binary data. For example, I chose to list my binary data in reverse (bit 0 at the front). This made left shifts easy (useful for multiplication) but what if someone else wanted to use this Binary module? They take 8 elements from the list. Should this be the first or last byte? What if there weren't even 8 elements to begin with? As I mentioned earlier it feels like Haskell provides the framework for 1001 ways to solve every problem but what I set out to do is add some type level checks that force programmers to specify how their Binary data would be modified and some acknowledgment of how it would happen.

To do this I added made use of the data kinds extension which promotes data constructors to types and types to kinds. This enables a kind of "meta-programming" on the types.

``` data BinVal (sz :: Nat) = forall a. (FiniteBits a) => BinVal a ```

This definition says that BinVals hold a value supporting FiniteBits operations and **the type** holds natural number representing the value's width. Notice there is no term level observation of this sz variable. It is only present on the type level.

To phantom type in action I'll show my declaration for "reduce" which takes BinVal and selects a subset of it as a new value of different width.

```
reduce :: (
	hi - lo + 1 <= dstSz,
	hi <= srcSz,
	0 <= lo) =>
	BinVal srcSz -> Proxy lo -> Proxy hi -> BinConv dstSz -> BinVal dstSz

```
This declaration says that for any valid subset of the bits in the source BinVal **that fits** into the width specified by dstSz ("destination size"), take a source value and a converter for dstSz and create a new BinVal of width dstSz.

This is absolutely not the best way to specify this check, and more experienced Haskellers would definitely have suggestions. However, the results are nonetheless exciting: Anyone who uses the Binary module and wants to convert between values of different width must acknowledge that they are taking a subarray of bits and putting it into a Binary value of at least that size otherwise, the type system will complain!

The types are the spec and the spec is the types :)

## Simulation with State Monad

Programming in many languages is **just** managing state. All variables, the stack, the heap is state and programs dictate manipulation of that state...right??? There's nothing wrong with that methodology but I really like how the functional paradigm takes another approach. We've built computers, the means to manipulate the state and compilers, the means to dictate that management. What if the programs were one level up? Programs as stateless description of data transformation. But I'm writing a simulator...I need state.

```
-- State Monad in crappy Haskell psuedocode
type S = State -> (a, State)
>>= :: State a -> (a -> State b) -> State b
sa >>= a2Sb = \s -> 
					let (a, s') = sa s in
						a2Sb a s'

>> :: State a -> State b -> State b
sa >> sb = \s ->
				let (a, s') = sa s in
					sb s'
```

First things first, we model State as a function that takes some State (any type) to some output (any type) and a possibly modified State (of the same type as the initial state). Now for the fun part

(>>=) is called bind. Its a member of the monad type class but in this instance it takes a state transformation that produces some value "a" and a function from that "a value" to a new state transformation and gives back a state transformation corresponding to that entire process. If we bind many times it creates a huge nested function that threads the state through many transitions...but it's still one function. That's amazing. An entire sequence of "reads" and "writes" described as a single function State to value and new altered state.

I'm not sure if (>>) has a name. It's the sequencing operator. At first glance it looks kind of like flip const. Take State a, throw it out and give back whatever comes in next. But that's not what happens in the function definition. Yes, the "a value" produced by the first State input is thrown out. But the altered state is not. This is known as monadic context and (>>) keeps those "effects" as computation is appended.



mind=blown. Haskell is kind of like convenient, typed lambda calculus. There's no assignment but with these higher order functions state is modeled through functions!

# Sources

because we can't do it ourselves

[CIS5520](https://www.seas.upenn.edu/~cis5520/current/index.html) is the college course I took on Haskell where I was introduced to most everything discussed here. It's taught by [Professor Weirich](https://www.seas.upenn.edu/~sweirich/) and she's amazing.

[Haskell Book](https://haskellbook.com) is a great book I read after taking the undergraduate course. I would describe it as a survey of Haskell because I sense there's a way to go deeper into most every topic presented. However, as a survey it does a great job.

[An Introduction to Type Level Programming](https://rebeccaskinner.net/posts/2021-08-25-introduction-to-type-level-programming.html) is the blog post by Rebecca Skinner that gave rise to the monstrosity that is my Binary module (just kidding I'm very proud). Given only maybe a couple pages of actual text, it does a pretty great job of introducing type level programming and I look forward to reading here book "Effective Haskell" when I get the chance.