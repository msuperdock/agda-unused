# agda-unused

`agda-unused` checks for unused code in an Agda project, including:

- variables
- definitions
- postulates
- data & record types
- `import` statements
- `open` statements
- pattern synonyms

Supported Agda versions: `>= 2.6.1 && < 2.6.2`

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
$ agda-unused Test.agda
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

## Usage

```
Usage: agda-unused FILE [-r|--root ROOT] [-l|--local] [-j|--json]
  Check for unused code in project with root directory ROOT

Available options:
  -h,--help                Show this help text
  FILE                     Path of file to check
  -r,--root ROOT           Path of project root directory
  -l,--local               Ignore publicly accessible items
  -j,--json                Format output as JSON
```

If `--root` is not given, we use the current directory as the project root.

## Approach

We make a single pass through the given Agda module and its dependencies:

- When a new item (variable, definition, etc.) appears, we mark it unused.
- When an existing item appears, we mark it used.

This means, for example, if we have three definitions (say `f`, `g`, `h`), each
depending on the previous one, where `h` is not a root, then `f` and `g` are
considered used, while `h` is considered unused. If we remove `h` and run
`agda-unsed` again, it will now report that `g` is unused. This behavior is
different from Haskell's built-in tool, which would report that all three
identifiers are unused on the first run.

## JSON

If the `--json` switch is given, the output is a JSON object with two fields:

- `type`: One of `"none"`, `"unused"`, `"error"`.
- `message`: A string, the same as the usual output of `agda-unused`.

The `"none"` type indicates that there is no unused code.

## Limitations

We currently do not support the following Agda features:

- [record module instance applications](https://agda.readthedocs.io/en/v2.6.1.1/language/module-system.html#parameterised-modules)
- [unquoting declarations](https://agda.readthedocs.io/en/v2.6.1.1/language/reflection.html#id3)
- [external libraries](https://agda.readthedocs.io/en/v2.6.1.1/tools/package-system.html)
(other than Agda's built-in libraries)

`agda-unused` will produce an error if your code uses these language features.

