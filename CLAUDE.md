# CLAUDE.md

This repository contains a research project to port Berkeley HardFloat-style Chisel floating-point hardware into Bluespec SystemVerilog, with the long-term goal of proving refinement against a Flocq/Rocq specification.

Claude should act as a careful hardware porting assistant, not as an authority on correctness. Floating-point correctness is subtle. Preserve exact behavior unless explicitly told otherwise.

## Repository layout

Important directories:

* `src/main/scala/`

  * Original HardFloat Chisel/Scala source code.
  * Treat this as the semantic reference for the port.
  * Do not edit this directory unless explicitly asked.

* `src/`

  * Bluespec SystemVerilog source code.
  * New BSV ports should go here, following the existing project organization and naming style.

* `tests/HardFloat-1/test/source/`

  * Existing HardFloat test source files.
  * Use these as references for test behavior, edge cases, and expected module semantics.
  * When adding or adapting tests, first inspect this directory to understand existing conventions.

## Project goals

* Port Chisel/Scala HardFloat modules from `src/main/scala/` into idiomatic Bluespec SystemVerilog under `src/`.
* Preserve bit-exact IEEE-754 behavior.
* Preserve the semantics of HardFloat recoded floating-point formats.
* Reuse or adapt tests from `tests/HardFloat-1/test/source/` where possible.
* Build a path toward differential testing and formal refinement against Flocq/Rocq.
* Keep every change small, reviewable, and tied to a specific source module.

## Important correctness rules

Do not simplify FPU logic unless explicitly asked.

Be especially careful with:

* Bit widths
* Chisel implicit width inference
* Signed versus unsigned arithmetic
* Two’s-complement casts
* Concatenation and slicing order
* Guard, round, and sticky bits
* Normalization and leading-zero count logic
* Subnormal numbers
* Signed zero
* Infinities
* Quiet NaNs and signaling NaNs
* NaN payload propagation
* Exception flags
* Rounding modes
* Overflow and underflow behavior
* Cancellation paths in add/sub
* Recoded exponent conventions

If there is ambiguity, stop and explain the ambiguity before editing code.

Never claim equivalence unless supported by tests, a proof sketch, or a clearly stated assumption.

## Preferred workflow

For each module port:

1. Read the relevant Chisel source from `src/main/scala/`.
2. Search for related tests in `tests/HardFloat-1/test/source/`.
3. Inspect the existing BSV style in `src/`.
4. Explain the source module’s behavior.
5. Identify all input/output widths.
6. Identify implicit Chisel width inference that may matter.
7. Identify signedness assumptions.
8. Identify special floating-point cases.
9. Port the module to Bluespec in `src/`.
10. Add or describe tests.
11. List assumptions and remaining risks.

Make small changes. Prefer one module or helper function per patch.

Do not edit unrelated files.

## Chisel-to-Bluespec translation guidelines

Use these default mappings unless there is a specific reason not to:

* Chisel `Bool` maps to BSV `Bool`.
* Chisel `UInt(w.W)` usually maps to `Bit#(w)`.
* Chisel `SInt(w.W)` maps to `Int#(w)` only when signed arithmetic is required.
* For exact bit manipulation, prefer `Bit#(n)` plus explicit casts.
* Chisel `Cat(a, b)` maps to `{a, b}`.
* Chisel `Mux(c, a, b)` maps to `c ? a : b`.
* Chisel `Fill(n, x)` maps to `replicate(x)` when appropriate.
* Chisel slices like `x(hi, lo)` map to explicit BSV bit slicing.
* Chisel single-bit indexing like `x(i)` maps to `x[i]`.
* Chisel reductions like `.orR` and `.andR` should become explicit reduction helpers if needed.
* Chisel `Wire` and `val` expressions should usually become BSV local bindings or helper functions.
* Chisel `Module` ports should become BSV interfaces or modules depending on the surrounding BSV style.

Always make widths explicit in BSV when translating arithmetic or concatenation-heavy code.

## Bluespec style

Prefer clear, direct BSV over clever abstractions.

Use helper functions for repeated bit-level operations, such as:

* exponent extraction
* significand extraction
* recoded format unpacking
* normalization
* leading-zero count
* sticky-bit computation
* rounding increment decisions
* exception flag construction

Keep names close to the original Chisel names when doing so helps review the port. Rename only when the BSV version would otherwise be confusing.

Prefer pure functions for combinational logic.

Avoid hidden state unless the original module is sequential or pipelined.

## Testing expectations

For every ported module, either add tests or explain exactly what tests should be added.

Before designing new tests, inspect:

* `tests/HardFloat-1/test/source/`
* any existing test infrastructure in the repository
* any existing BSV testbenches

Use both directed and randomized tests when possible.

Directed tests should include:

* positive zero
* negative zero
* smallest positive subnormal
* largest subnormal
* smallest normal
* ordinary positive and negative normals
* largest finite number
* positive infinity
* negative infinity
* quiet NaNs
* signaling NaNs
* NaNs with nontrivial payloads
* rounding boundary cases
* exponent overflow
* exponent underflow
* cancellation in add/sub
* sticky-bit-sensitive cases

When possible, compare BSV output against the original Chisel/Scala implementation or a trusted HardFloat reference.

If the existing HardFloat tests in `tests/HardFloat-1/test/source/` already cover a module, prefer adapting or reusing those tests rather than inventing a new test format.

## Formal verification awareness

This project is intended to support later refinement proofs against Flocq/Rocq.

When writing or modifying BSV, try to preserve structure that will be convenient for proof:

* Keep unpacking, normalization, rounding, and packing logically separated.
* Use explicit helper functions with clear contracts.
* Avoid mixing many semantic phases into one large expression.
* Document invariants near the code.
* State assumptions about recoded formats and bit widths.

For each meaningful helper function, consider adding a comment describing its mathematical intent.

## Review checklist

Before finishing a port or patch, check:

* Are all bit widths correct?
* Are all slices off-by-one checked?
* Are signed operations intentionally signed?
* Are unsigned operations intentionally unsigned?
* Is every concatenation ordered correctly?
* Are all special IEEE-754 cases handled?
* Are exception flags preserved?
* Are rounding modes handled exactly?
* Are NaN cases handled exactly?
* Are subnormals handled exactly?
* Does the BSV structure match the Chisel structure closely enough to review?
* Are assumptions documented?

## How to respond during coding

Before editing a nontrivial module, first provide:

* a short summary of the source module
* relevant tests found under `tests/HardFloat-1/test/source/`
* the planned BSV structure
* the main correctness risks

After editing, provide:

* files changed
* summary of implementation
* tests added or recommended
* known assumptions
* remaining risks

Do not provide overly confident claims of correctness.

## Commands and build

Before running commands, inspect the repo to find the actual build and test setup.

Do not assume the exact command names.

Prefer existing scripts, Makefiles, sbt configs, BSV build files, or CI commands.

If no test command exists, say so and suggest a minimal test strategy.

Useful first commands are usually:

```sh
find src/main/scala -type f
find src -type f
find tests/HardFloat-1/test/source -type f
find . -maxdepth 3 -type f \( -name "Makefile" -o -name "*.mk" -o -name "build.sbt" -o -name "*.bsv" -o -name "*.scala" \)
```

Do not run destructive commands.

## Boundaries

Do not rewrite large parts of the project without being asked.

Do not introduce a new framework or dependency without explaining why it is needed.

Do not change public interfaces unless explicitly requested.

Do not delete old Chisel reference code.

Do not remove comments that explain tricky IEEE-754 behavior.

Do not optimize for performance until correctness is established.
