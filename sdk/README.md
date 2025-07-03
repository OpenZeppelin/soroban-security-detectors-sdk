[![Build Status](https://github.com/OpenZeppelin/soroban-security-detectors-sdk/actions/workflows/build.yml/badge.svg)](https://github.com/OpenZeppelin/soroban-security-detectors-sdk/actions/workflows/build.yml)
[![Crates.io](https://img.shields.io/crates/v/soroban-security-detectors-sdk.svg)](https://crates.io/crates/soroban-security-detectors-sdk)
[![docs.rs](https://docs.rs/soroban-security-detectors-sdk/badge.svg)](https://docs.rs/soroban-security-detectors-sdk)
[![License: AGPL-3.0](https://img.shields.io/badge/license-AGPL--3.0-blue.svg)](https://github.com/OpenZeppelin/soroban-security-detectors-sdk/blob/main/LICENSE)

# Soroban Security Detectors SDK

> **soroban-security-detectors-sdk** is the foundational toolkit for writing custom security detectors for Stellar Soroban smart contracts in Rust.

## ✨ Why Use the SDK?

- **Rich AST Model**: Traverse and analyze the Soroban/Rust Abstract Syntax Tree and symbol table with ease.
- **Declarative Macros**: Annotate detector types with `#[detector]` to wire up metadata, templates, and reporting logic.
- **Extensible Utilities**: Leverage built-in symbol resolution, code queries, and fix suggestion helpers.
- **Seamless Integration**: Plug your detectors into the `soroban-scanner` CLI or embed within CI/CD pipelines.

## 🚀 Quick Start

### Add to Your Project

```toml
[dependencies]
soroban-security-detectors-sdk = "0.0.1"
```

### Basic Usage Example

```rust
use soroban_security_detectors_sdk::{build_codebase, DetectorResult};

// Build a codebase from source files or directories
let codebase = build_codebase(vec!["path/to/contract.rs"]).expect("failed to build codebase");

// Your detector logic: traverse codebase.ast(), symbol_table(), etc.
let findings: Vec<DetectorResult> = vec![];
```

## 🛠 Writing a Custom Detector

```rust
use soroban_security_detectors_sdk::{Codebase, DetectorResult, detector, Detector};

#[detector(
    type_name = "ExampleDetector",
    description = "Detects placeholder example issues",
)]
pub struct ExampleDetector;

impl Detector for ExampleDetector {
    fn check(&self, codebase: &Codebase) -> Option<Vec<DetectorResult>> {
        let mut findings = Vec::new();
        // Inspect AST or symbol_table and push findings
        if !findings.is_empty() {
            Some(findings)
        } else {
            None
        }
    }
}
```

## 📚 Documentation

- **Overview**: [sdk/docs/overview.md](docs/overview.md)
- **Usage Guide**: [sdk/docs/usage.md](docs/usage.md)
- **API Reference**: [sdk/docs/api_reference.md](docs/api_reference.md)

## ✅ Testing & Linting

```bash
# Run tests for the SDK crate
cargo test -p soroban-security-detectors-sdk

# Check formatting and lint rules
cargo fmt -- --check
cargo clippy -p soroban-security-detectors-sdk -- -D warnings
```

## 🤝 Contributing

Contributions are welcome! Please see [CONTRIBUTING](../docs/contributing.md) for guidelines.

## 📄 License

Distributed under the [AGPL-3.0 License](../LICENSE).