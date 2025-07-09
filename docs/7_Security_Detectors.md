# Security Detectors

This document explains the security detector framework and built-in detectors that analyze Soroban smart contracts for
vulnerability patterns. Security detectors implement specific analysis logic that operates on the parsed codebase to
identify potential security issues.

For information about how codebases are constructed and sealed for analysis,
see [Codebase Management](6_Codebase_Management.md). For details
about the AST system that detectors analyze, see [AST System](3_AST_System.md).

## Detector Framework Architecture

The security detector framework provides a trait-based architecture for implementing security analysis rules. Each
detector implements the `Detector` trait and operates on a `SealedCodebase` to identify vulnerability patterns.

### Core Framework Components

```mermaid
graph TB
    subgraph "Detector Framework"
        DT["Detector<T>"]
        DRT["DetectorReportTemplate"]
        CD["CombinedDetector<T>"]
        DR["DetectorResult"]
        SC["SealedCodebase"]
    end
    
    subgraph "Detector Macro System"
        DM["detector! macro"]
        IMPL["Generated Implementation"]
    end
    
    subgraph "Built-in Detectors"
        ETD["ExtendTtlWithMaxTtl"]
        CCP["ContractCanPanic"]
        TSC["TemporaryStorageValueUsedAsCondition"]
    end
    
    subgraph "Metadata System"
        YML["YAML Templates"]
        META["Detector Metadata"]
    end
    
    DT --> CD
    DRT --> CD
    CD --> ETD
    CD --> CCP
    CD --> TSC
    
    DM --> IMPL
    IMPL --> ETD
    IMPL --> CCP
    IMPL --> TSC
    
    SC --> DT
    DR --> DT
    
    YML --> META
    META --> DRT
```

**Detector Framework Architecture**

The framework consists of several key traits and types:

- `Detector<T>` - Core trait with `check` method that analyzes a codebase
- `DetectorReportTemplate` - Provides metadata and reporting templates
- `CombinedDetector<T>` - Union trait requiring both `Detector` and `DetectorReportTemplate`
- `DetectorResult` - Contains file path, offsets, and extra metadata for findings
- `SealedCodebase` - Type alias for `Codebase<SealedState>` that detectors analyze

### Detector Trait Interface

```mermaid
graph LR
    subgraph "Detector Implementation"
        CHECK["check(&self, codebase: &T)"]
        RESULT["Option<Vec<DetectorResult>>"]
    end
    
    subgraph "DetectorResult"
        FP["file_path: String"]
        OS["offset_start: u32"]
        OE["offset_end: u32"]
        EX["extra: Option<HashMap<String, String>>"]
    end
    
    subgraph "Analysis Capabilities"
        CONTRACTS["codebase.contracts()"]
        FUNCTIONS["codebase.functions()"]
        CHILDREN["codebase.get_children_cmp_cast()"]
        TYPES["codebase.get_expression_type()"]
        SYMBOLS["codebase.lookup_symbol_origin()"]
    end
    
    CHECK --> RESULT
    RESULT --> FP
    RESULT --> OS
    RESULT --> OE
    RESULT --> EX
    
    CHECK --> CONTRACTS
    CHECK --> FUNCTIONS
    CHECK --> CHILDREN
    CHECK --> TYPES
    CHECK --> SYMBOLS
```

**Detector Interface and Analysis Flow**

The `check` method receives a `SealedCodebase` and returns an optional vector of `DetectorResult` objects. Each result
contains precise file offsets and optional metadata for template substitution.

## Built-in Detectors

The codebase includes three built-in detectors that demonstrate different analysis patterns and address specific Soroban
security concerns.

### ExtendTtlWithMaxTtl Detector

This detector identifies improper usage of `max_ttl` values in `extend_ttl` calls, which may not have the intended
effect.

```mermaid
graph TB
    subgraph "Analysis Pattern"
        CONTRACTS["codebase.contracts()"]
        METHODS["contract.methods & functions"]
        METHODCALLS["get_children_cmp_cast<MethodCall>"]
        EXTENDTTL["method_name == 'extend_ttl'"]
        PARAM["parameters[2] (extend_to_param)"]
    end
    
    subgraph "Detection Logic"
        IDENTIFIER["Expression::Identifier"]
        METHODCALL["Expression::MethodCall"]
        CHECKMAXTTL["check_method_call()"]
        CHECKFUNC["check_function_call()"]
        MAXTTLCHECK["method_name == 'max_ttl'"]
    end
    
    subgraph "Symbol Tracing"
        SYMBOLORIGIN["lookup_symbol_origin()"]
        LETSTMT["Statement::Let"]
        ASSIGN["Expression::Assign"]
        RECURSIVE["Recursive checking"]
    end
    
    CONTRACTS --> METHODS
    METHODS --> METHODCALLS
    METHODCALLS --> EXTENDTTL
    EXTENDTTL --> PARAM
    
    PARAM --> IDENTIFIER
    PARAM --> METHODCALL
    IDENTIFIER --> SYMBOLORIGIN
    METHODCALL --> CHECKMAXTTL
    CHECKMAXTTL --> MAXTTLCHECK
    
    SYMBOLORIGIN --> LETSTMT
    SYMBOLORIGIN --> ASSIGN
    LETSTMT --> RECURSIVE
    ASSIGN --> RECURSIVE
```

**ExtendTtlWithMaxTtl Analysis Pattern**

The detector uses sophisticated symbol tracing to follow variable assignments and method calls to determine if the third
parameter of `extend_ttl` calls originates from a `max_ttl()` call.

### ContractCanPanic Detector

This detector identifies contract functions that may panic through `panic!`, `unwrap`, `expect`, or `assert` calls,
including indirect panics through function inlining.

```mermaid
graph TB
    subgraph "Function Analysis"
        CONTRACTS["codebase.contracts()"]
        FUNCTIONS["contract.methods & functions"]
        ENVPARAM["has Env parameter"]
        INLINE["codebase.inline_function()"]
    end
    
    subgraph "Panic Detection"
        CANPANIC["can_panic(function)"]
        STACK["NodeKind stack traversal"]
        MACROS["panic, assert, unreachable"]
        METHODS["unwrap, expect"]
    end
    
    subgraph "Statement Analysis"
        STMTMACRO["Statement::Macro"]
        EXPRMACRO["Expression::Macro"]
        MEMBERACCESS["Expression::MemberAccess"]
        METHODCALL["Expression::MethodCall"]
        CHILDREN["expr.children()"]
    end
    
    CONTRACTS --> FUNCTIONS
    FUNCTIONS --> ENVPARAM
    ENVPARAM --> INLINE
    INLINE --> CANPANIC
    
    CANPANIC --> STACK
    STACK --> STMTMACRO
    STACK --> EXPRMACRO
    STACK --> MEMBERACCESS
    STACK --> METHODCALL
    STACK --> CHILDREN
    
    STMTMACRO --> MACROS
    EXPRMACRO --> MACROS
    MEMBERACCESS --> METHODS
    METHODCALL --> METHODS
```

**ContractCanPanic Analysis Pattern**

The detector uses function inlining to analyze complete execution paths and detect both direct and indirect panic
conditions. It performs stack-based traversal of all statements and expressions.

### TemporaryStorageValueUsedAsCondition Detector

This detector identifies unsafe usage of temporary storage values in conditional expressions, which may lead to
unexpected behavior.

```mermaid
graph TB
    subgraph "Condition Analysis"
        CONTRACTS["codebase.contracts()"]
        FUNCTIONS["contract.methods & functions"]
        IFCONDS["get_children_cmp_cast<If>"]
        CONDITION["if_cond.condition"]
    end
    
    subgraph "Expression Types"
        METHODCALL["Expression::MethodCall"]
        IDENTIFIER["Expression::Identifier"]
        FUNCTIONCALL["Expression::FunctionCall"]
    end
    
    subgraph "Storage Detection"
        HASMETHOD["method_name == 'has'"]
        TEMPTYPE["base type == 'soroban_sdk::storage::Temporary'"]
        SYMBOLCHECK["check_identifier()"]
        FUNCCHECK["check_function_call()"]
    end
    
    subgraph "Symbol Tracing"
        ORIGIN["lookup_symbol_origin()"]
        LETSTMT["Statement::Let"]
        ASSIGN["Expression::Assign"]
        RECURSIVE["Recursive analysis"]
    end
    
    CONTRACTS --> FUNCTIONS
    FUNCTIONS --> IFCONDS
    IFCONDS --> CONDITION
    
    CONDITION --> METHODCALL
    CONDITION --> IDENTIFIER
    CONDITION --> FUNCTIONCALL
    
    METHODCALL --> HASMETHOD
    HASMETHOD --> TEMPTYPE
    IDENTIFIER --> SYMBOLCHECK
    FUNCTIONCALL --> FUNCCHECK
    
    SYMBOLCHECK --> ORIGIN
    ORIGIN --> LETSTMT
    ORIGIN --> ASSIGN
    LETSTMT --> RECURSIVE
    ASSIGN --> RECURSIVE
```

**TemporaryStorageValueUsedAsCondition Analysis Pattern**

The detector analyzes if conditions to identify direct usage of temporary storage `has()` calls or variables that
originate from such calls through symbol tracing.

## Detector Metadata and Templates

Each detector has associated YAML metadata that defines reporting templates and severity levels. This metadata is used
to generate user-friendly security reports.

### Metadata Structure

| Field         | Purpose                    | Example                               |
|---------------|----------------------------|---------------------------------------|
| `id`          | Unique detector identifier | `extend-ttl-with-max-ttl`             |
| `uid`         | Short identifier           | `pQwXyZ`                              |
| `description` | Detector purpose           | Analysis description                  |
| `severity`    | Issue severity level       | `medium`, `high`, `low`               |
| `tags`        | Classification tags        | `audit`, `reportable`, `completeness` |
| `template`    | Report formatting          | Title, body, closing templates        |

### Template Variables

The metadata templates support variable substitution for contextual reporting:

- `$file_name` - Source file name
- `$CONTRACT_NAME` - Contract name from detector results
- `$FUNCTION_NAME` - Function name from detector results
- `$instance_line` - Line number of the finding
- `$instance_line_link` - Link to the specific line
- `$total_files` - Total number of files with issues

## Creating Custom Detectors

The `detector!` macro simplifies creating new detectors by automatically generating the struct and trait
implementations.

### Detector Macro Usage

```rust
soroban_security_detectors_sdk::detector! {
    #[type_name = MyDetector]
    fn my_detector<SealedCodebase>(codebase: &SealedCodebase) -> Option<Vec<DetectorResult>> {
        // Implementation logic
    }
}
```

The macro generates:

- A struct with the specified `type_name`
- Implementation of `Detector<SealedCodebase>`
- Integration with the detector framework

### Analysis Patterns

Common analysis patterns used by detectors:

1. **Contract Iteration**: `codebase.contracts()` to iterate over all contracts
2. **Function Analysis**: Access methods and functions through `contract.methods` and `contract.functions`
3. **AST Traversal**: Use `get_children_cmp_cast` to find specific node types
4. **Type Analysis**: Use `get_expression_type` to determine expression types
5. **Symbol Tracing**: Use `lookup_symbol_origin` to trace variable definitions
6. **Function Inlining**: Use `inline_function` for deep analysis of execution paths

## Detector Execution and Results

Detectors are executed through the scanner CLI and integrated into the broader security analysis pipeline.

### Execution Flow

```mermaid
graph TB
    subgraph "Scanner Integration"
        CLI["soroban-scanner CLI"]
        ENGINE["Detector Engine"]
        CODEBASE["SealedCodebase"]
    end
    
    subgraph "Detector Registry"
        BUILTIN["Built-in Detectors"]
        CUSTOM["Custom Detectors"]
        REGISTER["detector_register()"]
    end
    
    subgraph "Execution Process"
        ITERATE["For each detector"]
        CHECK["detector.check(codebase)"]
        RESULTS["Vec<DetectorResult>"]
        AGGREGATE["Aggregate results"]
    end
    
    subgraph "Report Generation"
        TEMPLATE["Apply templates"]
        FORMAT["Format output"]
        OUTPUT["Security report"]
    end
    
    CLI --> ENGINE
    ENGINE --> CODEBASE
    ENGINE --> REGISTER
    
    REGISTER --> BUILTIN
    REGISTER --> CUSTOM
    
    ENGINE --> ITERATE
    ITERATE --> CHECK
    CHECK --> RESULTS
    RESULTS --> AGGREGATE
    
    AGGREGATE --> TEMPLATE
    TEMPLATE --> FORMAT
    FORMAT --> OUTPUT
```

**Detector Execution and Reporting Flow**

The scanner CLI orchestrates detector execution through a registry system, applies templates to results, and generates
formatted security reports.

### Testing Framework

The codebase includes comprehensive testing utilities for detector validation:

- `build_codebase` function for creating test codebases
- Synthetic source code examples in test cases
- Assertion helpers for verifying detector results
- Multi-file test scenarios for complex analysis

