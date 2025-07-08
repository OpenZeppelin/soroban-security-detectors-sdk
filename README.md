[![Build Status](https://github.com/OpenZeppelin/soroban-security-detectors-sdk/actions/workflows/build.yml/badge.svg)](https://github.com/OpenZeppelin/soroban-security-detectors-sdk/actions/workflows/build.yml)

# Soroban Security Detectors SDK

> **Soroban Security Detectors SDK** is an open-source framework for detecting security vulnerabilities and enforcing best practices in Soroban (Stellar smart contracts written in Rust) projects.

## 🚀 Features

- **Extensible SDK**: Build custom detectors with AST traversal, symbol resolution, and helper macros.
- **Out-of-the-box Detectors**: Prebuilt checks for common pitfalls like authorization issues, unchecked transfers, and more.
- **CLI Scanner**: `soroban-scanner` command-line tool for running detectors against your codebase.
- **CI/CD Ready**: Easily integrate into GitHub Actions, GitLab CI, or other pipelines.
- **OpenZeppelin Inspector Compatible**: Fully ready to be used as a custom scanner with OpenZeppelin Inspector.

## 📦 Crate Structure

| Crate              | Description                                                             |
| ------------------ | ----------------------------------------------------------------------- |
| `sdk`              | Core SDK providing AST, symbol table, utilities, and macros.            |
| `detectors`        | Collection of prebuilt detectors implemented using the SDK.             |
| `soroban-scanner`  | Command-line interface for executing detectors on Soroban codebases.    |

## ⚡ Quick Start

### Prerequisites

- Rust (nightly toolchain)
- Cargo

### Build the Workspace

```bash
git clone https://github.com/OpenZeppelin/soroban-security-detectors-sdk.git
cd soroban-security-detectors-sdk
cargo build --workspace
```

### Scan Your Contracts

```bash
# Run the installed CLI (requires `soroban-scanner` in your PATH)
soroban-scanner scan path/to/your/contracts

# Or via cargo (from the workspace root)
cargo run -p soroban-scanner -- scan path/to/your/contracts
```

_Filter by specific detectors (optional)_: 

```bash
# Run only the 'auth_missing' and 'unchecked_ft_transfer' detectors
soroban-scanner scan path/to/your/contracts --detectors auth_missing unchecked_ft_transfer
```

_Advanced options_:

```bash
# Specify the project root for relative paths in reports
soroban-scanner scan path/to/your/contracts --project-root path/to

# Load an external detector library (shared object)
soroban-scanner scan path/to/your/contracts --load path/to/libmy_detector.so
```

## 📝 Writing Custom Detectors

Leverage the [SDK Getting Started Documentation](docs/getting_started.md) to author your own detectors:

- AST helpers for traversing Rust code.
- Symbol table utilities for resolving types and references.
- Macros to simplify detector implementation.

## 📚 Documentation

Find detailed docs for developing and contributing:

- [Getting Started](docs/getting_started.md)
- [Contributing](docs/contributing.md)

### OpenZeppelin Inspector Integration

The `soroban-scanner` is fully compatible with OpenZeppelin Inspector's custom scanner interface. To use it with Inspector:

```bash
# Install soroban-scanner as a custom scanner in Inspector
inspector scanner install /path/to/soroban-scanner

# Or install from a URL
inspector scanner install https://github.com/OpenZeppelin/soroban-security-detectors-sdk/releases/latest/download/soroban-scanner-<os>-latest-v<version>.zip

# Run Inspector with soroban-scanner
inspector scan --scanner soroban-scanner path/to/your/soroban/project
```

The scanner provides metadata and findings in the format expected by Inspector, allowing seamless integration.

### API Documentation

Each crate also includes its own `rustdoc` generated documentation under its corresponding `doc/` directory.

It can be viewed directly by opening one of the following files in a web browser:

- `doc/soroban_security_detectors_sdk/index.html` - SDK documentation
- `doc/soroban_scanner/index.html` - Scanner documentation
- `doc/soroban_security_detectors/index.html` - Detectors documentation

## 🤝 Contributing

Contributions are welcome! Please follow the [contributing guidelines](docs/contributing.md).

## 📄 License

This project is licensed under the AGPL-3.0 License. See [LICENSE](LICENSE) for details.
