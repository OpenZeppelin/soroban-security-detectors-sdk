# Detectors

The `detectors` crate provides a collection of prebuilt security detectors for Soroban
smart contracts. Each detector identifies a specific class of issues in contract code.

## Quick Start

You can execute these detectors via the SDK or with the standalone runner:

```bash
cargo run --bin detectors-runner -- \
  --detectors-path detectors/src \
  --contracts-path path/to/contracts
```

## Documentation

- [Overview](docs/overview.md)
- [Writing new detectors](docs/writing_detectors.md)