[![Build Status](https://github.com/OpenZeppelin/soroban-security-detectors-sdk/actions/workflows/build.yml/badge.svg)](https://github.com/OpenZeppelin/soroban-security-detectors-sdk/actions/workflows/build.yml)
[![Crates.io](https://img.shields.io/crates/v/soroban-security-detectors.svg)](https://crates.io/crates/soroban-security-detectors)
[![docs.rs](https://docs.rs/soroban-security-detectors/badge.svg)](https://docs.rs/soroban-security-detectors)
[![License: AGPL-3.0](https://img.shields.io/badge/license-AGPL--3.0-blue.svg)](../LICENSE)

# Soroban Security Detectors

> Ready-to-use security detectors for Stellar Soroban smart contracts in Rust.

## 🔍 Built-in Detectors

| ID                                          | Description                                                                      |
| ------------------------------------------- | -------------------------------------------------------------------------------- |
| `contract-can-panic`                        | Detects `panic!`, `unwrap`, and `expect` calls inside contract functions.         |
| `contract-without-functions`                | Flags contracts defining no functions, indicating incomplete or stub contracts.  |
| `file-without-no-std`                       | Ensures each source file includes the `#![no_std]` attribute for no-std targets.  |
| `extend-ttl-with-max-ttl`                   | Checks that contracts extend `TTL` with a `max_ttl` value.                        |
| `temporary-storage-value-used-as-condition` | Prevents using temporary storage values directly as boolean conditions.           |

## 📦 Adding to Your Project

Add the crate to your `Cargo.toml`:

```toml
[dependencies]
soroban-security-detectors = "0.0.1"
```

## 🔧 Programmatic Usage

Use the SDK to build a codebase and invoke detectors in your code:

```rust
use soroban_security_detectors_sdk::build_codebase;
use soroban_security_detectors::all_detectors;

let code_files = vec!["path/to/contract.rs".to_string()];
let codebase = build_codebase(code_files).expect("failed to build codebase");

for detector in all_detectors() {
    if let Some(findings) = detector.check(&codebase) {
        for finding in findings {
            println!("{:?}", finding);
        }
    }
}
```

## 🛠 CLI Runner

Alternatively, run detectors via the `soroban-scanner` CLI:

```bash
cargo run -p soroban-scanner -- \
  scan path/to/contracts \
  --detectors-path detectors/src
```

## 📚 Documentation

- [Overview](docs/overview.md)
- [Writing Detectors](docs/writing_detectors.md)

## 🤝 Contributing

Contributions are welcome! Please see [CONTRIBUTING](../docs/contributing.md).

## 📄 License

Distributed under the [AGPL-3.0 License](../LICENSE).