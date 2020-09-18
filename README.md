# agda-unused

`agda-unused` checks for unused code in an Agda project, including:

- variables
- definitions
- data & record types
- `import` statements
- `open` statements
- pattern synonyms

`agda-unused` can be run globally on a full project, or locally on a file and
its dependencies. Running `agda-unused` globally requires an `.agda-roots` file
in the project root directory, which specifies the "roots" of the project. The
"roots" of a project are identifiers that are considered used even if not used
within the project itself. You might think of the `.agda-roots` file as a
specification of the public interface of your project.

## Example

File `~/Test.agda`:

```
module Test where

open import Agda.Builtin.Bool
  using (Bool; false; true)
open import Agda.Builtin.Unit

_∧_
  : Bool
  → Bool
  → Bool
false ∧ x
  = false
_ ∧ y
  = y
```

Command:

```
$ agda-unused --local Test.agda
```

Output:

```
/home/user/Test.agda:4,23-27
  unused imported item ‘true’
/home/user/Test.agda:5,1-30
  unused import ‘Agda.Builtin.Unit’
/home/user/Test.agda:11,9-10
  unused variable ‘x’
```

## Roots

Running `agda-unused` globally requires an `.agda-roots` file in the project
root directory. The `.agda-roots` format consists of module names, each followed
by a list of identifiers. Identifiers may be qualified; this allows us to refer
to identifiers defined within inner modules. If no identifiers are given for a
module, then all publicly accessible identifiers in that module are considered
roots, and will not be marked unused.

Example:

```
Main
- main

Pythagorean.Core
- isTriple
- allTriples

Pythagorean.Theorems

Pythagorean.Utils
- Print.prettyNat
```

Spacing and indentation are irrelevant; the only requirement is that module
names, identifiers, and the list symbol (`-`) appear in an appropriate order,
separated by any number of spaces of any kind.

We also supply an `agda-roots` executable which extracts a list of root files,
separated by single spaces, from an `.agda-roots` file; run `agda-roots --help`
for more information.

## Usage

```
Usage: agda-unused [-r|--root ROOT] [-l|--local FILE]
  Check for unused code in project with root directory ROOT

Available options:
  -h,--help                Show this help text
  -r,--root ROOT           Path of project root directory
  -l,--local FILE          Path of file to check locally
  -j,--json                Format output as JSON 
```

The project root directory is determined as follows:

- If the `--root` option is given, its value is the project root.
- If the `--local` option is given, the nearest ancestor with an `.agda-roots`
  file is the project root, if any.
- If the current directory has an `.agda-roots` file, it is the project root.
- Otherwise, the nearest ancestor of the current directory with an `.agda-roots`
  file is the project root, if any.
- Otherwise, we take the current directory as the project root.

## Approach

We make a single pass through each relevant Agda file:

- When a new item (variable, definition, etc.) appears, we mark it unused.
- When an existing item appears, we mark it used.

This means, for example, if we have three definitions (say `f`, `g`, `h`), each
depending on the previous one, where `h` is not a root, then `f` and `g` are
considered used, while `h` is considered unused. If we remove `h` and run
`agda-unsed` again, it will now report that `g` is unused. This behavior is
different from Haskell's built-in tool, which would report that all three
identifiers are unused on the first run.

For a global check, we check each file appearing in `.agda-roots` and its
dependencies. For a local check, we check just the given file and its
dependencies, ignoring all publicly accessible definitions.

## JSON

If the `--json` switch is given, the output is a JSON object with two fields:

- `type`: One of `"none"`, `"unused"`, `"error"`.
- `message`: A string, the same as the usual output of `agda-unused`.

The `"none"` type indicates that there is no unused code.

## Limitations

### Unsupported language features

We currently do not support code containing
[unquoting declarations](https://agda.readthedocs.io/en/v2.6.1/language/reflection.html#id3) or
[external libraries](https://agda.readthedocs.io/en/v2.6.1/tools/package-system.html)
(other than Agda's built-in libraries). `agda-unused` will produce an error if
your code uses these language features.

### Mutual recursion

For ordinary recursion, we consider an identifier unused if it only appears in
its own definition, but we have no such special handling for mutual recursion.
For example, consider the following code:

```
f
  : ℕ
  → ℕ

g
  : ℕ
  → ℕ

f zero
  = zero
f (suc n)
  = g n

g zero
  = zero
g (suc n)
  = f n
```

After checking this code, `g` and `h` are both considered used.

Fixing this issue will require a graph of dependencies for each block of
mutually recursive code.

