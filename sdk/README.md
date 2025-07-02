# Soroban Security Detectors SDK

This crate provides a code model (AST, symbol table, and storage) for Soroban/Rust
code, together with macros and traits to define custom security detectors over that
model.

## Quick start

Add this to your `Cargo.toml`:

```toml
soroban-security-detectors-sdk = "0.0.1"
```

Then in your code:
```rust
use soroban_security_detectors_sdk::{build_codebase, Detector};
// ...
```

For more detailed documentation, see the SDK docs:

- [Overview](docs/overview.md)
- [Usage examples](docs/usage.md)
- [API reference](docs/api_reference.md)