# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Advent of Code solutions repository written in Haskell, managed with Nix flakes and Cabal. Solutions are organized by year (Y2021, Y2022, Y2023, Y2024, Y2025) with each day as a separate module.

## Build and Development Commands

### Building the project
```bash
nix build          # Build the entire project
cabal build        # Alternative using cabal directly
```

### Running solutions
The `Main.hs` file controls which solution runs. Edit it to point to the desired year/day/part:
```haskell
import Y2024.Day3 (partII)
main = partII
```

Then run:
```bash
cabal run          # Run via cabal
nix run            # Run via nix
```

### Development environment
```bash
nix develop        # Enter development shell with all dependencies
cabal repl         # Start GHCi REPL with project loaded
```

### Formatting
```bash
nix fmt            # Format Haskell code with fourmolu, Nix with nixpkgs-fmt, and .cabal with cabal-fmt
```

## Architecture

### Module Organization
- **Source**: `src/Y{YEAR}/Day{N}.hs` - Each day's solution in a separate module under year namespace
- **Data**: `data/{YEAR}/day{N}.txt` - Input data files, with optional `day{N}-test.txt` for examples
- **Entry point**: `src/Main.hs` - Import and call the desired solution's `partI` or `partII`

### Solution Module Pattern
Each day module follows this structure:
```haskell
module Y2023.Day1 where

partI :: IO ()
partI = do
    input <- readFile "data/2023/day1.txt"
    -- solve and print result
    print result

partII :: IO ()
partII = do
    input <- readFile "data/2023/day1.txt"
    -- solve and print result
    print result
```

Both functions read from the same data file and print their results directly.

### Adding New Solutions

1. **Create module file**: `src/Y{YEAR}/Day{N}.hs`
2. **Add to cabal**: Update `aoc.cabal` `other-modules` section with `Y{YEAR}.Day{N}`
3. **Add data file**: Place input in `data/{YEAR}/day{N}.txt`
4. **Update Main.hs**: Import and call your solution
5. **Run**: Use `cabal run` or `nix run`

### Cabal Configuration

The project uses an extensive set of default language extensions (see `aoc.cabal` lines 31-70) including:
- `ImportQualifiedPost`
- `OverloadedStrings`
- `LambdaCase`
- `ViewPatterns`
- `TypeApplications`
- And many more...

All modules automatically have these extensions enabled.

### Dependencies

Key libraries available:
- **Parsing**: `attoparsec`, `megaparsec`
- **Data structures**: `containers`, `vector`, `array`, `nonempty-containers`
- **Utilities**: `split`, `lens`, `mtl`, `transformers`
- **Math/Graphs**: `linear`, `fgl`, `data-interval`

### Nix Flake Structure

- Uses `haskell-flake` for Haskell project management
- Formatting with `fourmolu` (configured for `ImportQualifiedPost`)
- Pre-commit hooks enabled via `treefmt`
- CI runs `nix build` on the main branch

## Working with Years

The project tracks multiple years simultaneously. Recent work focuses on Y2024 and Y2025. When adding solutions:
- Y2024 solutions are being deleted/moved (see git status)
- Y2025 is the current active year with empty placeholder files for Days 1-25
- Historical years (Y2021-Y2023) have partial solutions implemented
