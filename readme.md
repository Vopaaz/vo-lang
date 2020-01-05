# vo-lang

[![Actions Status](https://github.com/Vopaaz/vo-lang/workflows/CI/badge.svg)](https://github.com/Vopaaz/vo-lang/actions)
[![codecov](https://codecov.io/gh/Vopaaz/vo-lang/branch/master/graph/badge.svg?token=LbvhZpKxgF)](https://codecov.io/gh/Vopaaz/vo-lang)

A toy language interpreter written in Scala,
for learning basic principles of compiler design and practicing the Scala language.

Based on the book *Writing an interpreter in Go*,
with some creative modifications rather than just follow step by step.

## Running

Prerequisite: sbt

Packaging:

```bash
sbt package
```

Running the REPL:

```bash
scala ./target/scala-2.13/vo-lang_2.13-0.0.1.jar
```

Currently the functionality of REPL is limited to a single line statement.

Executing a `.vo` file:

```bash
scala ./target/scala-2.13/vo-lang_2.13-0.0.1.jar VOFILE
```

## Example Scripts

See [demo](src/main/resources/demo/).

## Differences between Vo and Monkey (the language in the book)

### Semicolon

In Monkey, a statement is terminated by a semicolon.
However, I program in Python, JavaScript and Scala and none of them uses semicolon so I do hate that...
So semicolon is not allowed in Vo, rather the line breaks are in charge of splitting statements.

Constrained by the design of parser in the book, Vo lang cannot "peek" too far ahead.
Everything, except for a start of a block statement, `{`, must be in the same line.

For example, this will work in Vo and result in an "1.0":
```txt
if (true) {
    1
} else {
    2
}
```

But this will cause an error:
```txt
if (true)
{
    1
}
else
{
    2
}
```

### Return statements

In Monkey, there are return statements which will terminate the execution of a block statement,
and return a value.

However I found the design of Scala amazing, which does not enable return statement by default
and use the value of the last statement in the block as the value of the block statement.

Given that "use the value of the last statement in the block as the value of the block statement"
is already there in Monkey,
I decide to not implement the return statement in my Vo-Lang.

### Integer and Number

Integer is specifically defined in Monkey, while there is only Number (float/double) in Vo.

### Object Orient Programming

Vo, unlike Monkey, will (hopefully) be an object orient language.
That's also why the Array is not implemented yet in Vo, as the `push(x, arr)` is not elegant.
It will be implemented soon.

## TO-DOs

- [ ] Clean up dirty error messages
- [x] Fix a linefeed alone will cause a "none" in REPL
- [ ] Support Object Orient Programming
- [ ] Implement built-in functions
- [ ] Simple `import` functionality
- [ ] Support `while` loop
- [ ] Support multiline statements in REPL
