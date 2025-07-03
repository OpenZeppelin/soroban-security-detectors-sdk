[![Build Status](https://github.com/OpenZeppelin/soroban-security-detectors-sdk/actions/workflows/build.yml/badge.svg)](https://github.com/OpenZeppelin/soroban-security-detectors-sdk/actions/workflows/build.yml)
[![Crates.io](https://img.shields.io/crates/v/soroban-scanner.svg)](https://crates.io/crates/soroban-scanner)
[![docs.rs](https://docs.rs/soroban-scanner/badge.svg)](https://docs.rs/soroban-scanner)
[![License: AGPL-3.0](https://img.shields.io/badge/license-AGPL--3.0-blue.svg)](../LICENSE)

# Soroban Scanner

> Command-line interface to run security detectors on Soroban (Stellar smart contracts) Rust code.
>
> Secure your Soroban contracts with OpenZeppelin’s scanner—discover vulnerabilities, enforce best practices, and integrate seamlessly into CI/CD pipelines.

## ⚡ Quick Start

**Install via crates.io**:

```bash
cargo install soroban-scanner
```

**Or run from the workspace**:

```bash
cargo run -p soroban-scanner -- scan path/to/contracts
```

_Filter by specific detectors_:

```bash
soroban-scanner scan path/to/contracts --detectors auth_missing unchecked_ft_transfer
```

_Advanced usage_:

```bash
soroban-scanner scan path/to/contracts \
  --detectors-path detectors/src \
  --project-root path/to \
  --load path/to/libmy_detector.so
```

## 📖 Usage

```bash
soroban-scanner scan <PATH> [OPTIONS]
soroban-scanner metadata
```

- `scan`: Analyze contract sources and report security findings in JSON.
- `metadata`: Output scanner metadata (available detectors, schema, etc.) in JSON.

For full CLI options, see [Usage Guide](docs/usage.md).

## 📚 Documentation

- [Overview](docs/overview.md)
- [Usage Guide](docs/usage.md)

## 🤝 Contributing

Contributions welcome! See [CONTRIBUTING](docs/contributing.md) for guidelines.

## 📄 License

Distributed under the [AGPL-3.0 License](../LICENSE).