Space (🌌) lang
================

Written for [LangJam 0001](https://github.com/langjam/jam0001), in 48 hours,
based on the theme of "first-class comments".

Language overview
-----------------

The language aims to answer the question: what if identifiers could be anything?
A function which name is the complete works of Shakespeare? Spaces in variable
names? This way your variable, function or structure name can say everything
it needs about the thing itself, becoming an integral part of your program,
unlike standard comments that are ignored by the compiler.

First of all, spaces are permitted within identifier names, so this is valid:

```go
let he who is without sin cast the first stone := 5;
```

And defines a variable called `he who is without sin cast the first stone` with
value 5.

You can also use numbers, emojis and some punctuation, though the first
symbol cannot be a digit or a symbol used by an operator.

```go
let Hello world := 42;
let Annie are you OK? := 1;
let There's 99 bottles of beer on the wall := 99;
let 🌌 := 7;
```

In case you want to do something that's not allowed within a standard
identifier (like include a punctuation symbol that can be confused for
a language operator, or start the identifier with a number), you can
simply mark it with backticks:

```go
let `Hello, world!` := 42;
let `Are you left-handed?` := 0;
let `3city` := 1;
```

You can use this notation to replace standard comments. A statement that contains
only an identifier is always a noop:

```go
`Space lang does not have comments, but you can use standalone identifiers
to explain your code. Even though this identifer does not refer to anything
that exists, the interpreter will not treat it as an error, until you try
to interact with it.`;

This works for naked identifiers too;
```

This means identifiers can get pretty long and unwieldy. In order to mitigate
that, you can refer to an identifier with any unambiguous prefix:

```go
let Hello world := 42;

the line below prints 42;
println(Hello); 
```

When defining a function, you can embed parameter names within the function
name with backticks:

```go
func count down from a given `number` to 1 {
    if number < 1 { return; }
    println(number);
    count down from (n - 1);
}

count down from (10);
```

Other than that, Space is a simple imperative language with familiar constructs:

```go
it can print stuff;
print("Hello!");

it can do arithmetic and logic;
let Hello world := 2 + 2 * 2;

it has mutable state;
Hello world := 7;

it has conditional statements;
if 2 > 1 {
	print("Surely");
} else if 0 != 0 {
	print("No way!");
} else {
	print("Huh?");
}

it has while loops;
let i the loop iterator := 0;
while i < 10 {
	println(i);
	i := i + 1;
}
```

You can find more example in the [test_code](test_code/) directory.

**Note:** the Mandelbrot program does not currently work, as floating
point numbers are not supported. All the other programs should work without
issues.


Building and running
--------------------

The interpreter is written in Rust, as a Cargo project, so all you need
to do is:

```
$ cargo run -- filename.🌌
```

Yes, we are using the Milky Way emoji (🌌) as a source file extension.
Surprisingly, it has not caused us any issues. The interpreter actually does 
not check for the extension, so if you do run into any issues, you can use any 
other extension instead.

You can also run the interpreter without any arguments, in which case
the code will be read from standard input.