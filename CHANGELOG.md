# Changelog

## 0.1.0 (Oct. 2020)

Initial release.

## 1.0.0 (Feb. 2021)

- Remove `.agda-roots` file; a global check now takes a filename representing an
  Agda module and ignores all items publicly accessible from that module.
- The `agda-unused` command now takes a filename as a positional argument.
- The `-l` or `--local` is now a flag, rather than an option taking an argument.
- Check variables in standalone data & record definitions.
- Check renaming directives simultaneously (fixes bug).

