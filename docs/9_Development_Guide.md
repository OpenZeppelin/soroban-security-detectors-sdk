# Development Guide

This document provides comprehensive information for contributors to the Soroban Security Detectors SDK project. It
covers project structure, development environment setup, testing utilities, build processes, and release workflows. This
guide is intended for developers who want to contribute to the SDK core, implement custom detectors, or enhance the CLI
scanner.

For information about using the SDK to create security detectors, see [Security Detectors](7_Security_Detectors.md). For
details about the CLI scanner usage, see [CLI Scanner](8_CLI_Scanner.md).

## Project Structure

The Soroban Security Detectors SDK follows a multi-crate workspace architecture that separates concerns between core SDK
functionality, detector implementations, and command-line tools.

### Workspace Architecture

```mermaid
graph TB
    subgraph "Workspace Root"
        WS["Cargo.toml<br/>workspace = true"]
    end
    
    subgraph "sdk/"
        SDK["soroban-security-detectors-sdk<br/>Core AST & Analysis"]
        SDKC["Cargo.toml<br/>dependencies"]
    end
    
    subgraph "detectors/"
        DET["soroban-security-detectors<br/>Built-in Security Detectors"]
        DETC["Cargo.toml<br/>depends on sdk"]
    end
    
    subgraph "soroban-scanner/"
        CLI["soroban-scanner<br/>CLI Application"]
        CLIC["Cargo.toml<br/>depends on sdk + detectors"]
    end
    
    WS --> SDK
    WS --> DET
    WS --> CLI
    
    SDK --> DET
    SDK --> CLI
    DET --> CLI
    
    style "sdk/" fill:#e1f5fe
    style "detectors/" fill:#f3e5f5
    style "soroban-scanner/" fill:#e8f5e8
```

The workspace configuration defines shared version numbers, licensing, and dependency management across all crates. The
`resolver = "2"` setting enables Cargo's new dependency resolver for better version resolution.

### Crate Dependencies

| Crate                            | Purpose                                             | Key Dependencies                                               |
|----------------------------------|-----------------------------------------------------|----------------------------------------------------------------|
| `soroban-security-detectors-sdk` | Core AST parsing, symbol table, codebase management | `syn`, `quote`, `proc-macro2`, `serde`                         |
| `soroban-security-detectors`     | Built-in security detector implementations          | `soroban-security-detectors-sdk`                               |
| `soroban-scanner`                | CLI interface and detector execution engine         | `soroban-security-detectors-sdk`, `soroban-security-detectors` |

## Development Environment Setup

### Prerequisites

The project requires a standard Rust development environment with specific toolchain components for cross-platform
builds and quality checks.

```mermaid
graph TD
    A["Rust Toolchain"] --> B["stable toolchain"]
    A --> C["clippy component"]
    A --> D["rustfmt component"]
    
    B --> E["Multi-target Support"]
    E --> F["x86_64-unknown-linux-gnu"]
    E --> G["x86_64-apple-darwin"]
    E --> H["x86_64-pc-windows-gnu"]
    
    C --> I["Code Quality"]
    D --> I
    
    I --> J["CI/CD Pipeline"]
    J --> K["Automated Testing"]
    J --> L["Release Builds"]
```

### Local Development Setup

1. **Clone Repository**: Initialize with submodules for complete dependency resolution
2. **Install Rust**: Use the stable toolchain with required components
3. **Build Project**: Execute `cargo build` to compile all workspace members
4. **Run Tests**: Execute `cargo test` to verify installation

The workspace uses specific lint configurations to handle macro-generated code, particularly for the `unused_imports`
lint which is disabled due to macro expansion behavior.

## Testing Framework

The SDK provides comprehensive testing utilities for detector development and codebase analysis validation.

### Test Utilities Module

```mermaid
graph TB
    subgraph "Test Utilities (sdk/src/utils/test.rs)"
        A["create_mock_location()"]
        B["create_mock_file()"]
        C["create_mock_function()"]
        D["create_mock_contract()"]
    end
    
    subgraph "Mock Objects"
        E["Location<br/>source, offsets, lines"]
        F["File<br/>id, name, path, attributes"]
        G["Function<br/>id, name, visibility, parameters"]
        H["Contract<br/>id, name, methods, functions"]
    end
    
    A --> E
    B --> F
    C --> G
    D --> H
    
    subgraph "Test Integration"
        I["Detector Unit Tests"]
        J["AST Node Tests"]
        K["Codebase Analysis Tests"]
    end
    
    E --> I
    F --> J
    G --> K
    H --> K
```

The test utilities provide factory functions for creating mock AST nodes and codebase elements. These utilities are
essential for writing unit tests for detectors without requiring full file parsing.

### Mock Object Creation

The testing framework offers several factory functions for creating test fixtures:

| Function                 | Purpose                      | Key Fields                                           |
|--------------------------|------------------------------|------------------------------------------------------|
| `create_mock_location()` | Creates location metadata    | `source`, `offset_start`, `offset_end`, `start_line` |
| `create_mock_file()`     | Creates file AST nodes       | `id`, `name`, `path`, `attributes`                   |
| `create_mock_function()` | Creates function definitions | `id`, `name`, `visibility`, `parameters`             |
| `create_mock_contract()` | Creates contract structures  | `id`, `name`, `methods`, `functions`                 |

These functions support parameterized creation for customized test scenarios, such as
`create_mock_function_with_parameters()` for testing function analysis.

### Example Test Pattern

```rust
#[test]
fn test_detector_logic() {
    let file = create_mock_file();
    let function = create_mock_function(1);
    // Test detector implementation
}
```

## Build Process

The build system supports multiple target platforms with automated cross-compilation for distribution.

### Build Targets and Matrix

```mermaid
graph TB
    subgraph "Build Matrix"
        A["ubuntu-latest<br/>x86_64-unknown-linux-gnu"]
        B["macos-latest<br/>x86_64-apple-darwin"]
        C["windows-latest<br/>x86_64-pc-windows-gnu"]
    end
    
    subgraph "Build Process"
        D["cargo build --release"]
        E["Package Executable"]
        F["Upload Artifacts"]
    end
    
    A --> D
    B --> D
    C --> D
    
    D --> E
    E --> F
    
    subgraph "Artifacts"
        G["soroban-scanner-ubuntu-latest-vX.Y.Z.zip"]
        H["soroban-scanner-macos-latest-vX.Y.Z.zip"]
        I["soroban-scanner-windows-latest-vX.Y.Z.zip"]
    end
    
    F --> G
    F --> H
    F --> I
```

The build process uses GitHub Actions to create platform-specific binaries. Each target produces a compressed archive
containing the `soroban-scanner` executable.

### Local Build Commands

| Command                 | Purpose                 |
|-------------------------|-------------------------|
| `cargo build`           | Build debug version     |
| `cargo build --release` | Build optimized version |
| `cargo test`            | Run all tests           |
| `cargo clippy`          | Run linter              |
| `cargo fmt`             | Format code             |

## Release Workflow

The project uses an automated release process with semantic versioning and cross-platform binary distribution.

### Release Process Flow

```mermaid
flowchart TD
    A["Workflow Dispatch<br/>version_type: patch/minor/major"] --> B["Bump Version"]
    B --> C["Parse Current Version<br/>from Cargo.toml"]
    C --> D["Calculate New Version<br/>MAJ.MIN.PAT"]
    D --> E["Update Cargo.toml<br/>sed -i replacement"]
    E --> F["Commit & Tag<br/>git commit + git tag"]
    F --> G["Build Matrix<br/>ubuntu/macos/windows"]
    G --> H["Package Binaries<br/>zip archives"]
    H --> I["Generate Changelog<br/>loopwerk/tag-changelog"]
    I --> J["Create GitHub Release<br/>upload artifacts"]
    
    style A fill:#e8f5e8
    style J fill:#e8f5e8
```

The release workflow supports three types of version bumps and automatically handles version string manipulation, git
operations, and artifact publication.

### Version Management

The version bumping logic uses shell scripting to parse and increment semantic version numbers:

```bash
# Current version extraction
CUR=$(grep -m1 '^version' Cargo.toml | sed -E 's/version = "([^"]+)"/\1/')

# Version component calculation
if [ "$version_type" = "patch" ]; then
    PAT=$((PAT+1))
elif [ "$version_type" = "minor" ]; then
    MIN=$((MIN+1)); PAT=0
elif [ "$version_type" = "major" ]; then
    MAJ=$((MAJ+1)); MIN=0; PAT=0
fi
```

## Code Organization Patterns

### Detector Implementation Pattern

The SDK provides macros and traits for implementing security detectors with consistent structure and behavior.

```mermaid
graph TB
    subgraph "Detector Definition"
        A["detector! macro"]
        B["#[type_name = DetectorName]"]
        C["fn check(codebase) -> Option<Vec<DetectorResult>>"]
    end
    
    subgraph "Generated Code"
        D["struct DetectorName"]
        E["impl Detector<T> for DetectorName"]
        F["fn check() implementation"]
    end
    
    subgraph "Required Implementation"
        G["impl DetectorReportTemplate"]
        H["id(), uid(), description()"]
        I["severity(), tags()"]
        J["title_*(), body_*(), closing()"]
    end
    
    A --> D
    B --> E
    C --> F
    
    D --> G
    E --> H
    F --> I
    
    subgraph "Usage"
        K["SorobanDetector<T>"]
        L["CombinedDetector trait"]
    end
    
    G --> K
    H --> L
    I --> L
```

The `detector!` macro automatically generates the detector struct and implements the `Detector` trait, while developers
must manually implement `DetectorReportTemplate` for complete functionality.

### DetectorResult Structure

```mermaid
graph TB
    subgraph "DetectorResult Fields"
        A["file_path: String<br/>Source file location"]
        B["offset_start: u32<br/>Issue start position"]
        C["offset_end: u32<br/>Issue end position"]
        D["extra: Option<HashMap<String, String>><br/>Template substitutions"]
    end
    
    subgraph "Usage Context"
        E["Template Variables<br/>$NAME, $PARENT_NAME"]
        F["Report Generation<br/>YAML template processing"]
        G["Location Mapping<br/>Line number calculation"]
    end
    
    D --> E
    A --> F
    B --> G
    C --> G
```

The `DetectorResult` structure provides precise location information and supports template variable substitution for
flexible report generation.

### Testing Integration

The testing framework integrates with the detector architecture to provide comprehensive validation capabilities:

```mermaid
graph TD
    subgraph "Test Structure"
        A["Test Utilities<br/>create_mock_*()"]
        B["Mock AST Nodes<br/>File, Function, Contract"]
        C["Detector Implementation<br/>check() method"]
        D["Result Validation<br/>DetectorResult assertions"]
    end
    
    A --> B
    B --> C
    C --> D
    
    subgraph "Test Categories"
        E["Unit Tests<br/>Individual detector logic"]
        F["Integration Tests<br/>Full codebase analysis"]
        G["Mock Object Tests<br/>Utility function validation"]
    end
    
    D --> E
    D --> F
    A --> G
```

