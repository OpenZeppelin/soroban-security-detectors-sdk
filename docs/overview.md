# Soroban Security Detectors SDK

This repository provides a framework and out-of-the-box detectors for analyzing
Soroban (Stellar smart contracts in Rust) code for common security issues.

## Crate Overview

- `sdk` — core SDK providing AST, symbol table, and macros for building detectors.
- `detectors` — a collection of prebuilt detectors.
- `detectors-runner` — a CLI tool for running detectors on your code.

## Documentation

For detailed developer onboarding, see the project docs:

- [Project Overview](./overview.md)
- [Getting Started](./getting_started.md)
- [Contributing](./contributing.md)

Each crate also has its own documentation under its `docs/` directory.