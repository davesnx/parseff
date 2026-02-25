# Parseff

Parser combinators with OCaml 5 algebraic effects

## The Core Idea

In yieldparser (JS), a parser is a generator function that `yield`s chunks to its driver, which feeds back results. In Parseff, a parser is a plain OCaml function that `perform`s effects to its handler, which feeds back results. The handler is the "driver": it advances the cursor, handles failure, and manages backtracking via captured continuations.

```
JS generator world:     yield chunk      →   driver matches   →  send result back
OCaml effects world:    perform effect   →   handler matches  →  continue k result
```

The algebraic effects model is strictly more powerful: handlers can capture the continuation `k` and call it *multiple times* (for backtracking) or *not at all* (for failure/cut).

## Installation

```bash
opam install parseff
```

Or to build from source:

```bash
git clone https://github.com/username/parseff.git
cd parseff
opam install . --deps-only
dune build
dune install
```

## Quick Start

```ocaml
open Parseff

(* Parse an IP address *)
let number () =
  let digits = many1 digit () in
  let n = List.fold_left (fun acc d -> (acc * 10) + d) 0 digits in
  if n >= 0 && n <= 255 then n else fail "number out of range"

let ip_address () =
  let a = number () in let _ = consume "." in
  let b = number () in let _ = consume "." in
  let c = number () in let _ = consume "." in
  let d = number () in
  end_of_input ();
  (a, b, c, d)

(* Run the parser *)
match run "192.168.1.1" ip_address with
| Ok ((a, b, c, d), _) -> 
    Printf.printf "Parsed: %d.%d.%d.%d\n" a b c d
| Error { pos; expected } -> 
    Printf.printf "Error at %d: %s\n" pos expected
```

## How Parseff Compares

### Comparison to Other OCaml Parser Libraries

| Feature | Parseff | Angstrom | Menhir | Sedlex+Menhir |
|---------|---------|----------|--------|---------------|
| **Approach** | Effect-based combinators | Monadic combinators | LR(1) parser generator | Lexer + parser generator |
| **Sequencing** | Plain `let x = p ()` | `>>=`, `let*`, `<*>` | Grammar rules | Grammar rules |
| **Backtracking** | Via `<\|>` effect (explicit) | Via `<\|>` combinator | Limited (GLR extension) | No |
| **Error Messages** | Position + expected token | Customizable | Excellent (generated) | Excellent |
| **Left Recursion** | ❌ Not supported | ❌ Not supported | ✅ Supported | ✅ Supported |
| **Performance** | Very good (1.6-4.3x faster than Angstrom) | Very good | Excellent | Excellent |
| **Ambiguous Grammars** | ✅ Allowed | ✅ Allowed | ❌ Rejected at compile time | ❌ Rejected |
| **Type Safety** | Runtime errors possible | Runtime errors possible | Compile-time guarantees | Compile-time guarantees |
| **Ease of Use** | ⭐⭐⭐⭐⭐ Imperative style | ⭐⭐⭐ Monadic style | ⭐⭐⭐ Grammar learning curve | ⭐⭐⭐ Two-stage learning |
| **Dynamic Grammars** | ✅ Yes | ✅ Yes | ❌ No (compile-time) | ❌ No (compile-time) |
| **OCaml Version** | ≥ 5.0 (effects required) | ≥ 4.03 | Any | Any |
| **Dependencies** | `re` (regex) | None | None | `sedlex` |
| **Recursive Parsers** | `let rec` (natural) | Requires `fix` combinator | Natural in grammar | Natural in grammar |
| **Context-Sensitive** | ✅ Possible (via state) | ❌ No | ❌ No | ❌ No |
| **Incremental Parsing** | ❌ No | ✅ Yes | ❌ No | ❌ No |

### Key Takeaways

**Choose Parseff if you want:**
- ✅ Imperative-style parser writing (like JS generators)
- ✅ Maximum flexibility (context-sensitive grammars)
- ✅ To avoid monadic ceremony (`>>=`, `<*>`, etc.)
- ✅ To experiment with algebraic effects in OCaml 5
- ✅ Quick prototyping with simple syntax

**Choose Angstrom if you want:**
- ✅ Battle-tested production library
- ✅ Incremental parsing (streaming)
- ✅ OCaml < 5.0 compatibility
- ✅ Known performance characteristics

**Choose Menhir if you want:**
- ✅ Left-recursive grammars
- ✅ Best-in-class error messages
- ✅ Compile-time grammar validation
- ✅ Maximum performance

**Choose Sedlex+Menhir if you want:**
- ✅ Full Unicode support in lexer
- ✅ Production-grade compiler frontend
- ✅ Separate lexer and parser stages

## Examples

### Route Parser

```ocaml
type route =
  | Home
  | About
  | BlogHome
  | BlogArticle of string

let home () = 
  let _ = consume "/" in
  end_of_input (); Home

let blog_article () =
  let _ = consume "/blog/" in
  let re = Re.compile (Re.Posix.re ".+") in
  let slug = match_re re in
  end_of_input ();
  BlogArticle slug

let route () = 
  (home <|> about <|> blog_home <|> blog_article) ()

(* Usage *)
match run "/blog/hello-world" route with
| Ok (BlogArticle slug, _) -> Printf.printf "Article: %s\n" slug
| _ -> ()
```

### CSS Parser

```ocaml
type declaration = { property : string; value : string }
type rule = { selector : string; declarations : declaration list }

let ws_may () =
  let re = Re.compile (Re.Posix.re "[ \t\n\r]*") in
  match_re re

let identifier () =
  let re = Re.compile (Re.Posix.re "[a-zA-Z_-][a-zA-Z0-9_-]*") in
  match_re re

let declaration () =
  let _ = ws_may () in
  let property = identifier () in
  let _ = ws_may () in let _ = consume ":" in let _ = ws_may () in
  let value = match_re (Re.compile (Re.Posix.re "[^;]+")) in
  let _ = ws_may () in let _ = consume ";" in
  { property; value }

let rule () =
  let selector = identifier () in
  let _ = ws_may () in let _ = consume "{" in let _ = ws_may () in
  let declarations = many declaration () in
  let _ = ws_may () in let _ = consume "}" in
  { selector; declarations }

(* Parse CSS *)
match run "body { color: red; }" rule with
| Ok (r, _) -> (* Process rule *)
| Error _ -> (* Handle error *)
```

See `examples/` directory for more complete examples.

## API Reference

### Core Primitives

- `consume : string -> string` - Match exact string literal
- `char : char -> char` - Match exact character
- `satisfy : (char -> bool) -> string -> char` - Match character satisfying predicate
- `match_re : Re.re -> string` - Match compiled regular expression
- `end_of_input : unit -> unit` - Ensure no input remains

### Combinators

- `(<|>) : (unit -> 'a) -> (unit -> 'a) -> unit -> 'a` - Alternation (try left, backtrack and try right)
- `many : (unit -> 'a) -> unit -> 'a list` - Zero or more repetitions
- `many1 : (unit -> 'a) -> unit -> 'a list` - One or more repetitions
- `optional : (unit -> 'a) -> unit -> 'a option` - Optional parser
- `sep_by : (unit -> 'a) -> (unit -> 'b) -> unit -> 'a list` - Parse separated list
- `count : int -> (unit -> 'a) -> unit -> 'a list` - Parse exactly n times
- `look_ahead : (unit -> 'a) -> 'a` - Parse without consuming input

### Convenience

- `digit : unit -> int` - Parse decimal digit (0-9) as integer
- `letter : unit -> char` - Parse ASCII letter
- `whitespace : unit -> string` - Parse zero or more whitespace
- `alphanum : unit -> char` - Parse alphanumeric character

See `lib/parseff.mli` for complete API documentation.

## Design Philosophy

### Effect Handlers

Parseff uses OCaml 5's algebraic effects to create a clean separation between parser logic and execution:

- **Parser functions** describe *what* to parse using effects
- **Effect handler** implements *how* to parse (cursor management, backtracking, error handling)

This is similar to how async/await separates async logic from the event loop.

### Backtracking

The `<|>` combinator explicitly marks backtracking points. When the left branch fails, the handler:
1. Saves the current cursor position
2. Tries the left parser
3. If it fails, restores the cursor and tries the right parser

No automatic backtracking happens - you control where it occurs.

### No Left Recursion

Direct left recursion will cause infinite loops:

```ocaml
(* ❌ This will loop forever *)
let rec expr () =
  let lhs = expr () in  (* immediate recursive call *)
  consume "+";
  let rhs = term () in
  Add (lhs, rhs)
```

**Solution**: Rewrite grammar or use iteration with `many`.

## Relationship to Academic Work

### Krishnaswami & Yallop (PLDI '19)

The paper "Algebraic Effects and Handlers for Parsing" defines a typed algebraic presentation of CFGs with provable guarantees: parsers are linear-time with single-token lookahead and no backtracking.

**Key difference**: Parseff allows backtracking (via `<|>`), trading guaranteed linear-time performance for ease of use and expressiveness. Parseff grammars can be ambiguous and context-sensitive, whereas K&Y's approach rejects them at construction time.

### Other References

- **Wadler, P.** "Monads for Functional Programming" - Foundation of monadic parser combinators
- **Kiselyov, O. et al.** "Algebraic Effects and Effect Handlers" - Theory of delimited control
- **Warth et al.** "Packrat Parsers Can Support Left Recursion" - Memoization techniques (not yet implemented in Parseff)

## Performance

### Benchmark Results

Testing on a JSON array parser (`[1, 2, 3, ..., 10]`) with 100,000 iterations:

```
Parseff (span):  ~4,940,000 parses/second  (4.3x faster than Angstrom)
Parseff (fair):  ~1,930,000 parses/second  (1.6x faster than Angstrom)
Angstrom:        ~1,150,000 parses/second
Memory:          Parseff 197 MB vs Angstrom 584 MB (3.0x less)
```

The "span" variant uses zero-copy parsing with a custom `float_of_span` that avoids
`float_of_string` for 1-2 digit integers. The "fair" variant uses the same
`float_of_string` conversion as Angstrom for an apples-to-apples comparison.

Starting from a baseline of ~12,000 parses/sec (101x slower than Angstrom), systematic
optimization achieved a **412x improvement** and surpassed Angstrom. See
`OPTIMIZATION_LOG.md` for the full optimization journey.

### Performance Tips

To get the best performance from Parseff:

1. **Always pre-compile regexes at module level**:
   ```ocaml
   (* BAD - compiles regex on every call *)
   let number () =
     let re = Re.compile (Re.Posix.re "[0-9]+") in
     match_re re
   
   (* GOOD - compiles once at module initialization *)
   let number_re = Re.compile (Re.Posix.re "[0-9]+")
   let number () = match_re number_re
   ```

2. **Use `take_while`/`skip_while` instead of regex** for character class patterns:
   ```ocaml
   (* GOOD - fast character scanning, no regex overhead *)
   let digits () = take_while1 (fun c -> c >= '0' && c <= '9') "digit"
   ```

3. **Use fused operations** for hot paths:
   ```ocaml
   (* GOOD - skip ws + match char in one effect dispatch *)
   skip_while_then_char is_whitespace ','
   
   (* BEST - entire separated list in one effect dispatch *)
   let values = sep_by_take is_whitespace ',' is_digit in
   ```

4. **Use `skip_whitespace` instead of `whitespace`** when you don't need the string

5. **Minimize `<|>` usage** - each alternation requires a handler setup for backtracking

See `OPTIMIZATION_LOG.md` for detailed analysis.

### Known Trade-offs

1. **No Streaming**: Unlike Angstrom, Parseff requires the full input string upfront

2. **Backtracking Cost**: Each `<|>` combinator saves/restores cursor position
   - Deep alternation trees can be expensive

3. **OCaml 5+ Required**: Uses algebraic effects (not available before OCaml 5.0)

### When Parseff Works Well

- High-performance parsing with imperative-style syntax
- Parsing configuration files, protocols, or data formats
- Building DSL interpreters with context-sensitive rules
- Any use case where Angstrom would work, with simpler syntax

## Adding Better Error Messages

Parseff makes it easy to add better error messages in userland:

### The `<?>` Operator

```ocaml
let ( <?> ) parser msg () =
  try parser () with _ -> fail msg

(* Usage *)
let dot () = ((fun () -> char '.') <?> "expected '.' separator") ()
```

### Context Labels

```ocaml
let label name parser () =
  try parser () with
  | _ -> fail (Printf.sprintf "in %s: parse failed" name)

(* Usage *)
let number_0_255 () =
  (label "number (0-255)" (fun () ->
     let n = parse_int () in
     if n <= 255 then n else fail "out of range")) ()
```

### Complete Example

```ocaml
let ip_address_with_errors () =
  let a = number_0_255 () in
  let _ = ((fun () -> consume ".") <?> "expected '.' after first octet") () in
  let b = number_0_255 () in
  (* ... *)
  (a, b, c, d)

(* Error output: *)
(* Error at pos 10: in number (0-255): parse failed *)
```

See `examples/better_errors.ml` for a complete working example with expression parsing.

### Error Accumulation

You can also build error accumulation on top:

```ocaml
type error_context = {
  errors : (int * string) list;  (* All failure points *)
  furthest : int;                 (* Furthest point reached *)
}

(* Track all errors by wrapping the runner *)
```

## Known Limitations

1. **Performance**: With fused operations and zero-copy spans, Parseff is 1.6-4.3x faster than Angstrom. Without fused operations, expect comparable or slightly slower performance.

2. **Left Recursion**: Direct left recursion causes infinite loops (rewrite grammar or use iteration)

3. **No Memoization**: Same input position may be parsed multiple times

4. **OCaml 5+ Required**: Uses algebraic effects (not available before OCaml 5.0)

5. **No Incremental Parsing**: Full input must be available upfront

6. **Complex Recursive Grammars**: Deep mutual recursion with alternations may have subtle interaction bugs (see known issues)

## Contributing

Contributions are welcome! Please:

1. Open an issue to discuss proposed changes
2. Write tests for new features
3. Run `dune build @fmt` before submitting
4. Ensure all tests pass with `dune runtest`

## License

MIT License - see LICENSE file for details

## Acknowledgments

Inspired by:
- [yieldparser](https://github.com/JavaScriptRegenerated/yieldparser) - JavaScript generator-based parsers
- [Angstrom](https://github.com/inhabitedtype/angstrom) - Fast OCaml parser combinators
- OCaml 5 effects system - Making delimited control mainstream
