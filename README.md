# agda-unused

`agda-unused` checks for unused code in an Agda project, including:

- variables
- definitions
- postulates
- data & record types
- `import` statements
- `open` statements
- pattern synonyms

`agda-unused` takes a filepath representing an Agda code file and checks for
unused code in that file and its dependencies. By default, `agda-unused` does
not check public items that could be imported elsewhere. But with the `--global`
flag, `agda-unused` treats the given file as a description of the public
interface of the project, and additionally checks for unused files and unused
public items in dependencies. (See below for more on the `--global` flag.)

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
Usage: agda-unused FILE [-r|--root ROOT] [-g|--global] [-j|--json]
  Check for unused code in FILE

Available options:
  -h,--help                Show this help text
  FILE                     Path of file to check
  -r,--root ROOT           Path of project root directory
  -g,--global              Check project globally
  -j,--json                Format output as JSON
```

The project root directory is determined as follows:

- If the `--root` option is given, its value is the project root.
- Otherwise, the nearest ancestor directory of `FILE` containing an `.agda-lib`
 file is the project root, if any.
- Otherwise, the parent directory of `FILE` is the project root.

## Global

If the `--global` flag is given, all declarations in the given file must be
imports. The set of imported items is treated as the public interface of the
project; these items will not be marked unused. The public items in dependencies
of the given module may be marked unused, unlike the default behavior. We also
check for unused files.

To perform a global check on an Agda project, first create a file that imports
exactly the intended public interface of your project. For example:

File `All.agda`:

```
module All where

import A
  using (f)
import B
  hiding (g)
import C
```

Command:

```
$ agda-unused All.agda --global
```

## JSON

If the `--json` flag is given, the output is a JSON object with two fields:

- `type`: One of `"none"`, `"unused"`, `"error"`.
- `message`: A string, the same as the usual output of `agda-unused`.

The `"none"` type indicates that there is no unused code.

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

## Limitations

We do not distinguish between overloaded constructors; if a constructor is used,
we mark all constructors in scope with the same name as used. This is because
`agda-unused` works with Agda's concrete syntax; to give more accurate results,
we would need to use Agda's type-checker, which comes with a performance cost.

Additionally, we currently do not support the following Agda features:

- [record module instance applications](https://agda.readthedocs.io/en/v2.6.1.3/language/module-system.html#parameterised-modules)
- [unquoting declarations](https://agda.readthedocs.io/en/v2.6.1.3/language/reflection.html#id3)

`agda-unused` will produce an error if your code uses these language features.

