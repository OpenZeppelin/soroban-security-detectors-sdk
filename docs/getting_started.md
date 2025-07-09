# Getting Started

> **Soroban Security Detectors SDK** is an open-source framework for detecting security vulnerabilities and enforcing best practices in Soroban (Stellar smart contracts written in Rust) projects.

## 📂 Repository Structure

| Crate             | Description                                                                      |
|-------------------|----------------------------------------------------------------------------------|
| `sdk`             | Core SDK with AST, symbol table, utilities, and macros for building detectors.   |
| `detectors`       | Collection of prebuilt detectors implemented using the SDK.                      |
| `soroban-scanner` | Command-line interface (`soroban-scanner scan ...`) for running detectors on Soroban projects. |

## ➡️ Next Steps

- Review [Contributing](./contributing.md) for guidelines on contributing.
- Explore detailed API docs in each crate’s `docs/` subdirectory.

## Prerequisites

- Rust toolchain (nightly)
- `cargo`

## Building

```bash
git clone https://github.com/OpenZeppelin/soroban-security-detectors-sdk.git
cd soroban-security-detectors-sdk
cargo build --workspace
```

## Testing

```bash
cargo test --workspace
```

See each crate's docs for more specific usage instructions.