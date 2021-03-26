# Changelog

## 0.1.0 (Oct. 2020)

Initial release.

## 0.2.0

- Support all Agda library-related options (`--include-path`, etc.).
- Remove support for `.agda-roots` file; use an ordinary Agda file instead.
- Replace the `--local` option with a `--global` flag; default to a local check.
- The `agda-unused` command now takes a filename as a positional argument.
- Use `.agda-lib` file to infer the root directory if not given.
- Check variables in standalone data & record definitions.
- Check renaming directives simultaneously (fixes bug).

