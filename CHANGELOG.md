# Changelog

## 0.1.0 (Oct. 2020)

Initial release.

## 1.0.0 (Feb. 2021)

- Remove support for `.agda-roots` file. A global check now takes a filename
  representing an Agda module and ignores all public items in that module.
- Replace the `--local` option with a `--global` flag; default to a local check.
- The `agda-unused` command now takes a filename as a positional argument.
- Use `.agda-lib` file to infer the root directory if not given.
- Check variables in standalone data & record definitions.
- Check renaming directives simultaneously (fixes bug).

