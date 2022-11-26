# Changelog

## 0.1.0 (Oct 2020)

Initial release.

## 0.2.0 (May 2021)

- Support code depending on external libraries.
- Support Agda's library-related options (`--include-path`, etc.).
- Remove support for `.agda-roots` file; use an ordinary Agda file instead.
- Replace the `--local` option with a `--global` flag; default to a local check.
- Remove the `--root` option; infer the project root directory automatically.
- The `agda-unused` command now takes a filename as a positional argument.
- Check variables in standalone data & record definitions.
- Check renaming directives simultaneously (fixes bug).
- Check record types with fields referencing other fields (fixes bug).
- Check import statements with `as _` (fixes bug).

## 0.3.0 (Nov 2022)

- Support Agda 2.6.2; remove support for earlier versions.

