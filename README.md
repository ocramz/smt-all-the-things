# smt-all-the-things

Solving puzzles with SMT solvers.

Contains some alternative implementations that use `logict`

## Dependencies

* stack
* z3

## Building and Running


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
