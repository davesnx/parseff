---
title: Credits
description: Research and projects that influenced Parseff's design
---

Parseff builds on ideas from several papers and projects. This page acknowledges the work that shaped its form.

- **[Retrofitting Effect Handlers onto OCaml](https://dl.acm.org/doi/10.1145/3453483.3454039)** (Sivaramakrishnan et al., PLDI '21). The design and implementation of effect handlers in OCaml 5.

- **[A Typed, Algebraic Approach to Parsing](https://www.cl.cam.ac.uk/~jdy22/papers/a-typed-algebraic-approach-to-parsing.pdf)**. Showed that parsing can be framed as an algebraic problem with typed combinators very nicely.

- **[Angstrom](https://github.com/inhabitedtype/angstrom)**. The OCaml parser combinator library whose streaming model motivated a simpler, effects-based alternative.

- **[yieldparser](https://github.com/JavaScriptRegenerated/yieldparser)** A JavaScript parser combinator library that uses generators to write parsers in direct style, demonstrating that parser combinators don't have to be monadic to be composable. An inspiration for the direct-style approach using effects instead of generators.
