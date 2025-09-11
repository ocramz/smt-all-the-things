# smt-all-the-things

A Haskell Stack project using the SBV (SMT-Based Verification) library for SMT solver examples and utilities.

## Building and Running

### Using Cabal
```bash
cabal build
cabal run smt-all-the-things-exe
cabal test
```

### Using Stack
```bash
stack build
stack run
stack test
```

## Project Structure

- `src/Lib.hs` - Main library module with SBV imports
- `app/Main.hs` - Executable entry point
- `test/Spec.hs` - Test suite (basic structure)
- `smt-all-the-things.cabal` - Cabal package configuration with sbv dependency
- `stack.yaml` - Stack configuration