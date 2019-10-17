# Vanillalog

[![Build Status](https://travis-ci.com/madgen/vanillalog.svg?branch=master)](https://travis-ci.com/madgen/vanillalog)

A statically-typed Datalog variant that is as vanilla as it gets.

This is a frontend that compiles to [Exalog](https://github.com/madgen/exalog) for evaluation and optimisation.

## Installation

Clone the repository, run `stack build`, run `stack exec -- vanillalog`, let me know if it fails.

If you do `stack install`, you shouldn't need the prefix  `stack exec --`. From there on, I'll omit it.

## Usage

Running `vanillalog` lists all the options, but in summary, one can `run` or
inspect (`pp`) a datalog program. There is a `repl` as well.

The most common usage to evaluate a program and see the results to queries in
the source is:

```
vanillalog run -f <filepath>
```

The REPL also evaluates a Datalog file, but it also provides a loop to write
further queries on it. It's invocation is as follows:

```
vanillalog repl -f <filepath>
```

You can use `:q`, `:quit`, `:e`, `:exit`, or `Ctrl+D` to exit it.

The REPL doesn't yet allow extending existing predicates or defining recursive
queries.

## Syntax

Best way to get a feel is to look at [the examples](examples). Definitely let me know if any of them causes problems!

A program is a list of _clauses_, _facts_, and _queries_.

A clause is of the form `head :- body.` (don't omit the full stop!). Example clause:

```prolog
ancestor(X,Y) :- ancestor(X,Z), parent(Y,Z).
```

A fact is of the form `head.` (again, don't omit the full stop!). Example fact:

```prolog
parent("Dad","Son").
```

A query is of the form `?- body.` (you know the deal). Example query:

```prolog
?- ancestor(Ancestor,"Son").
```

### Head

A clause _head_ is an _atomic formula_.

An atomic formula is of the form `pred([Term]...)`. It consits of a _predicate_ name and zero or more _terms_. Predicate names starts with lowercase letters, thereafter they can contain upper or lowercase alphanumeric characters as well as underscore (`_`) and a tick (`'`).

A term is a _variable_, a _literal_, or a _wildcard_.

Variables start with uppercase letters followed by alphanumeric characters `_` and `'`.

There are three kinds of literals: bools, integers, texts.

Bools: `true` and `false`

Integers: exactly what you'd expect, e.g., `42`, `-24`, `0`.

Texts: quoted arbitrary characters with no means to escape, e.g., `"Horn Clauses are nice!"`

A wildcard starts with an underscore (`_`) and is followed by anything that a variable can be.

### Body

A _body_ is structured like expressions are in most languages. The binary operators are conjunction (`,`) and disjunction (`;`). The unary operator is negation (`!`). There are also parantheses for disambiguation. Here's the BNF syntax:

```
Body ::= Atomic Formula | Body , Body | Body ; Body | ! Body | ( Body )
```

## Semantics

To be continued...
