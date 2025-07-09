# Overview

## Purpose and Scope

This document provides a comprehensive introduction to the Soroban Security Detectors SDK, a static analysis framework
designed specifically for detecting security vulnerabilities in Soroban smart contracts. The framework enables
developers to identify potential security issues in Stellar smart contracts written in Rust through sophisticated AST
parsing, symbol resolution, and configurable security detectors.

This overview covers the system's architecture, core components, and integration capabilities. For detailed information
about the AST system implementation, see [AST System](3_AST_System.md). For guidance on creating custom detectors,
see [Security Detectors](7_Security_Detectors.md). For CLI usage instructions, see [CLI Scanner](8_CLI_Scanner.md).

## Three-Crate Architecture

The Soroban Security Detectors SDK follows a modular three-crate architecture that separates concerns and enables
extensibility:

| Crate             | Purpose                          | Key Components                                     |
|-------------------|----------------------------------|----------------------------------------------------|
| `sdk`             | Core analysis infrastructure     | AST parsing, symbol tables, analysis utilities     |
| `detectors`       | Security vulnerability detection | Prebuilt detectors, detector metadata              |
| `soroban-scanner` | Command-line interface           | CLI tool, report generation, external integrations |

### Crate Dependencies

```mermaid
graph TD
    scanner["soroban-scanner<br/>(CLI Interface)"]
    detectors["detectors<br/>(Security Checks)"]
    sdk["sdk<br/>(Core Infrastructure)"]
    
    scanner --> detectors
    scanner --> sdk
    detectors --> sdk
    
    subgraph "External Dependencies"
        syn["syn<br/>(Rust AST Parsing)"]
        quote["quote<br/>(Code Generation)"]
        proc_macro2["proc-macro2<br/>(Macro Support)"]
    end
    
    sdk --> syn
    sdk --> quote
    sdk --> proc_macro2
```

**Crate Dependency Architecture**

## System Workflow

The framework processes Soroban smart contracts through a systematic pipeline from source code to security reports:

```mermaid
flowchart LR
    input[("Soroban<br/>Source Code")]
    
    subgraph sdk_processing["SDK Processing"]
        parse["syn::parse_file<br/>(AST Construction)"]
        symbols["Symbol Table<br/>Building"]
        seal["Codebase<br/>Sealing"]
    end
    
    subgraph detector_execution["Detector Execution"]
        load_detectors["Load Detectors<br/>from detectors crate"]
        run_checks["Execute Security<br/>Checks"]
        collect_results["Collect<br/>DetectorResult"]
    end
    
    subgraph scanner_output["Scanner Output"]
        generate_reports["Generate<br/>Reports"]
        format_output["Format Output<br/>(YAML/JSON)"]
    end
    
    subgraph integrations["External Integrations"]
        console["Console<br/>Output"]
        inspector["OpenZeppelin<br/>Inspector"]
        cicd["CI/CD<br/>Pipelines"]
    end
    
    input --> parse
    parse --> symbols
    symbols --> seal
    
    seal --> load_detectors
    load_detectors --> run_checks
    run_checks --> collect_results
    
    collect_results --> generate_reports
    generate_reports --> format_output
    
    format_output --> console
    format_output --> inspector
    format_output --> cicd
```

**End-to-End Security Analysis Workflow**

## Core Components

### SDK Infrastructure

The `sdk` crate provides the foundational components for static analysis:

```mermaid
graph TB
    subgraph "AST System"
        ast_nodes["AST Node Types<br/>(Expression, Statement,<br/>Function, Definition)"]
        nodes_storage["NodesStorage<br/>(Node Management)"]
        ast_builder["AST Builder<br/>(syn Integration)"]
    end
    
    subgraph "Symbol Resolution"
        symbol_table["SymbolTable<br/>(Scope Management)"]
        fixpoint_resolver["fixpoint_resolver<br/>(Import Resolution)"]
        type_inference["Type Inference<br/>System"]
    end
    
    subgraph "Analysis APIs"
        codebase["SealedCodebase<br/>(Analysis Interface)"]
        functions_api["functions()<br/>(Function Queries)"]
        contracts_api["contracts()<br/>(Contract Detection)"]
        type_api["get_expression_type()<br/>(Type Analysis)"]
        symbol_api["lookup_symbol_origin()<br/>(Symbol Tracing)"]
    end
    
    ast_builder --> ast_nodes
    ast_nodes --> nodes_storage
    nodes_storage --> symbol_table
    symbol_table --> fixpoint_resolver
    fixpoint_resolver --> type_inference
    type_inference --> codebase
    
    codebase --> functions_api
    codebase --> contracts_api
    codebase --> type_api
    codebase --> symbol_api
```

**SDK Core Component Architecture**

### Detector Framework

The `detectors` crate implements security vulnerability detection using the SDK infrastructure:

```mermaid
graph TB
    subgraph "Detector Implementation"
        detector_macro["detector!<br/>(Macro Definition)"]
        detector_trait["Detector<br/>(Trait Interface)"]
        detector_result["DetectorResult<br/>(Finding Structure)"]
    end
    
    subgraph "Built-in Detectors"
        extend_ttl["ExtendTtlWithMaxTtl<br/>(Storage TTL Issues)"]
        can_panic["ContractCanPanic<br/>(Panic Detection)"]
        temp_storage["TemporaryStorageCondition<br/>(Storage Patterns)"]
    end
    
    subgraph "Analysis Capabilities"
        function_inline["Function Inlining<br/>(inline_function)"]
        symbol_origin["Symbol Origin<br/>(lookup_symbol_origin)"]
        type_checking["Type Checking<br/>(get_expression_type)"]
        ast_traversal["AST Traversal<br/>(get_children_cmp_cast)"]
    end
    
    detector_macro --> extend_ttl
    detector_macro --> can_panic
    detector_macro --> temp_storage
    
    detector_trait --> detector_result
    
    extend_ttl --> symbol_origin
    can_panic --> function_inline
    temp_storage --> type_checking
    
    function_inline --> ast_traversal
    symbol_origin --> ast_traversal
    type_checking --> ast_traversal
```

**Detector Framework Components**

### Scanner Interface

The `soroban-scanner` crate provides command-line access and external integrations:

```mermaid
graph TB
    subgraph "CLI Interface"
        scanner_main["soroban-scanner<br/>(Main Binary)"]
        scan_command["scan<br/>(Command Handler)"]
        detector_engine["DetectorEngine<br/>(Execution Controller)"]
    end
    
    subgraph "Configuration"
        detector_filter["--detectors<br/>(Detector Selection)"]
        project_root["--project-root<br/>(Path Configuration)"]
        load_external["--load<br/>(External Detector Loading)"]
    end
    
    subgraph "Output Formats"
        yaml_output["YAML Reports<br/>(Structured Output)"]
        json_output["JSON Reports<br/>(Machine Readable)"]
        console_output["Console Output<br/>(Human Readable)"]
    end
    
    subgraph "External Integrations"
        inspector_compat["OpenZeppelin Inspector<br/>(inspector scanner install)"]
        cicd_integration["CI/CD Integration<br/>(GitHub Actions, GitLab CI)"]
    end
    
    scanner_main --> scan_command
    scan_command --> detector_engine
    
    detector_filter --> detector_engine
    project_root --> detector_engine
    load_external --> detector_engine
    
    detector_engine --> yaml_output
    detector_engine --> json_output
    detector_engine --> console_output
    
    yaml_output --> inspector_compat
    json_output --> cicd_integration
    console_output --> inspector_compat
```

**Scanner CLI and Integration Architecture**

## Integration Capabilities

### OpenZeppelin Inspector Compatibility

The framework provides full compatibility with OpenZeppelin Inspector's custom scanner interface, enabling seamless
integration into existing security workflows. The `soroban-scanner` binary can be installed directly as an Inspector
custom scanner and provides metadata and findings in the expected format.

### CI/CD Pipeline Integration

The scanner is designed for automated security analysis in continuous integration environments. It provides structured
output formats and configurable exit codes for pipeline integration, supporting both GitHub Actions and GitLab CI
workflows.

### Extensibility Framework

The modular architecture allows developers to create custom detectors by leveraging the SDK's AST parsing, symbol
resolution, and analysis utilities. The `detector!` macro simplifies detector implementation while providing access to
sophisticated analysis capabilities.