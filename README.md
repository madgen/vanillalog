# Vanillalog

[![Build Status](https://travis-ci.com/madgen/vanillalog.svg?branch=master)](https://travis-ci.com/madgen/vanillalog)

A Datalog variant that is as vanilla as it gets.

When used as a library, it provides an abstract AST acting as an intermediate target for Datalog variants with interesting operators. For example, see [Temporalog](https://github.com/madgen/temporalog) for using Computation Tree Logic (CTL) operators in Datalog formulae.

This frontend (or intermediate representation) compiles to [Exalog](https://github.com/madgen/exalog) for evaluation and optimisation.

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

## Foreign predicates

There are a number of foreign predicates included for your convenience.

### Arithmetic

|Predicate name|Type|Mode|Description|
|--------------|----|----|-----------|
|`add`| `Int x Int x Int`|`++?`|Puts the sum of first two params. into the third.|
|`subtract`| `Int x Int x Int`|`++?`|Take a guess.|
|`lt`| `Int x Int`|`++`|Holds when first param. is less than the second.|
|`lte`| `Int x Int`|`++`|Take a guess.|
|`gt`| `Int x Int`|`++`|Take a guess.|
|`gte`| `Int x Int`|`++`|Take a guess.|

### Unification

|Predicate name|Type|Mode|Description|
|--------------|----|----|-----------|
|`unify_int`| `Int x Int`|`+?`|Unifies the first parameter with the second.|
|`unify_text`| `Text x Text`|`+?`|Ditto.|
|`unify_bool`| `Bool x Bool`|`+?`|Ditto.|

### IO

|Predicate name|Type|Mode|Description|
|--------------|----|----|-----------|
|`csv1`| `Text x Text`|`+?`|Reads a CSV file, from the path in the first parameter and places each row in the second parameter.|
|`csv2`| `Text x Text x Text`|`+??`|Take a guess.|
|`csv3`| `Text x Text x Text x Text`|`+???`|Take a guess.|
|`csv4`| `Text x Text x Text x Text x Text`|`+????`|Take a guess.|

## Semantics

To be continued...
