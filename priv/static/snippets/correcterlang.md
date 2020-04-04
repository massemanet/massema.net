Write Erlang the Right Way(tm)
---

I hate the expression `best practice`. However, maybe it's diffrent if I'm the
one defining them? So here's the Best Way to write Erlang.

# good things

### procs (with monitors)

Naked `spawn` is not very safe, and links is the old way. Use monitors instead. That way you can synchronize
between potentially crashing processes.

### `gen_server` (but not `gen_fsm`)

The `gen_server` is old, but still the go-to pattern. Use it. Avoid `gen_fsm` (lacks
expressive power) and `gen_statem` (needlessly complicated).

### supervision trees

Supervision trees are old and comes with a fair bit of boilerplate. Still worth
it though. Forces some structure on you, and provides reporting. `one_for_one`
and `simple_one_for_one` are the defaults.

### functions (no nested cases)

Use functions. Make big functions small by splitting them into more
functions. Hardcoded values should be in functions, not nakes in the code or
hidden by macros.

### lambdas (fold)

`lists:foldl` is one of the most imortant library functions. Learn to use it.

### matching (`=` is not assignment)

Use matching as much as possible. Remember that `=` is not assignment, but
binding and matching combined.

### hot loading (rapid dev, debugging)

Hot code loading can be used to upgrade, but if possible it's better to use
rolling restarts (especially in this the age of the container). Even so, hot
code loading enables much more rapid turnaround times while developing and
debugging.

### tracing (redbug)

Tracing is a key enabler while debugging. Especially if you're using
[redbug](https://github.com/massemanet/redbug) or something similar.

### repl (rr, rp)

The REPL (a.k.a. The Shell) is another key enabler for rapid development and
debugging. Learn to use it. Some important commands are

* `rr` - load record definitions
* `rp` - print a term without "helpful" truncation
* `c` - compile and load a module
* `f` - forget variable bindings

Keep in mind though; the code you write in the shell is not compiled, but
interpreted. Hence, not "real" erlang.

# bad things

### preprocessor (esp include)

The preprocessor is, in summary, brown and smells bad. It allows you to write
things that looks like erlang, even though it's not. It does not work on parse
trees, but on lists of tokens. All of this is confusing, and makes for
difficult to read code.

Include files breaks the assumption that modules are independent entities
(since two modules can include the same file, they must always be compiled in
tandem). This introduces an intractable compile time dependency beteen the
modules.

The preprocessor is necessary if you want to present different code to
different versons of the compiler , e.g. if there has been an incompatible
change in the language.

There are two cases where you need to use the preproc; user defined guards and
abstract patterns. See wiz.

### `if`, `and`, and `or`

You should avoid `if`, `and`, and `or`. They are unneccesary and you are better
off sticking to `case`, `andalso`, and `orelse`. USe lambdas intead of `begin`.

## lacks

### typing (use dialyzer)

Set up your build system to always compile and dialyze. You will get the few
extra seconds of compile time (once the cache is built) back very quickly.

### abstract patterns (use preproc)

This is not expressable in real erlang; `f(P(X,Y)) ->...`, where `P` is an
"abstract pattern" that can be used in a match. You have to use the preprocssor
(`P` can be a macro).

### user defined guards (use preproc or case)

This is not expressable in real erlang; `f(X) when G(X) ->...`, where `G` is a
"user defined guard". `G` can be a macro though, if it only uses subset if
BIF's that are allowed in guards. E.g., this `is_integer(hd(S))` used as a
guard only matches lists of integers, which in many programs are the same thing
as strings (all strings are lists of integers, but the opposite is not
generally true).

## antipatterns

### large functions (no nested case)

Overly large functions are bad in all languages, but especially silly in a
functional language (duh). A good rule of thumb is to have no more than one
level of nesting in each function. Invest some time in the sensible naming of
all these small functions, and you'll be good. Ideally the code should read
like a natural language, like `case is_too_large(X) of true -> give_up()` or
somesuch.

### faking mutability

This pattern is pretty common;
```
A = a(),
A1 = b(A),
A2 = c(A1)
```
It's unreadable and very easy to get wrong.
There are three ways to work around this.

* break the problem into smaller functions
* skip the intermediate variables; `c(b(a()))`
* fold over a list of functions. Something like `lists:foldl(fun(F,A) -> F(A)
  end, a(), [fun b/1, fun c/1])`

All four of these patterna are arguably bad. In practice, limiting yourself to
one intermediate variable, like `X` and `NewX`, and breaking into smaller
functions works the best.

### Wildcard receives

A wildcard `receive _` is bad if there are some other code (perhaps a library)
that handles its own receives. Make a ref if you need to and match on it.

### Catching wildcard exceptions

A wildcard `catch _:_` is bad if some other code does exception handling. Stick
something into your exceptions that you can match on.

### Using timeouts to coordinate

In concurrent code, it's tempting to stick in some waits here and there to make
things happen in the right order (tell; the code has `timer:sleep` in it). This
is bad because it only works by accident. It will break when you run it on a
different machine or with a different workload. Do explicit coordintaion
between the processes instead.

## discuss

* maps vs. records
* sync between procs (monitors + `exit(Result)`)
* non local return (try/throw)
