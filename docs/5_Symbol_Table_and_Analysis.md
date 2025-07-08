# Symbol Table and Analysis

This document covers the symbol table and semantic analysis system that provides rich code understanding capabilities
for security detectors. The system manages scope hierarchies, resolves symbols, performs type inference, and tracks
symbol origins across Rust/Soroban codebases.

For information about the basic AST node structure, see [AST Node Types](4_AST_Node_Types.md). For details about how
detectors use these analysis capabilities, see [Security Detectors](7_Security_Detectors.md).

## Architecture Overview

The symbol table system forms the semantic analysis layer of the SDK, built on top of the AST infrastructure to provide
deep code understanding.

### Symbol Table System Architecture

```mermaid
graph TB
    subgraph "Raw AST Layer"
        AST["AST Nodes"]
        NS["NodesStorage"]
    end
    
    subgraph "Symbol Table Core"
        ST["SymbolTable"]
        SC["Scope"]
        SR["ScopeRef"]
        DR["DefinitionRef"]
    end
    
    subgraph "Resolution Engine"
        FPR["fixpoint_resolver"]
        IR["Import Resolution"]
        SR_PROC["Symbol Resolution"]
    end
    
    subgraph "Analysis APIs"
        TI["Type Inference"]
        SO["Symbol Origin Tracking"]
        SL["Symbol Lookup"]
        ET["Expression Type Analysis"]
    end
    
    subgraph "Detector Interface"
        GET["get_expression_type"]
        LSO["lookup_symbol_origin"]
        GST["get_symbol_type"]
        GFN["get_function_by_name"]
    end
    
    AST --> ST
    NS --> ST
    ST --> SC
    SC --> SR
    SC --> DR
    
    ST --> FPR
    FPR --> IR
    FPR --> SR_PROC
    
    ST --> TI
    ST --> SO
    ST --> SL
    ST --> ET
    
    TI --> GET
    SO --> LSO
    SL --> GST
    ST --> GFN
    
    GET --> DetectorAPIs["Detector Analysis"]
    LSO --> DetectorAPIs
    GST --> DetectorAPIs
    GFN --> DetectorAPIs
```

**Sources**: [sdk/src/symbol_table.rs:1-881]()

### Scope Hierarchy and Definition Management

```mermaid
graph TB
    subgraph "Scope Management"
        RootScope["Root Scope (Crate)"]
        ModScope["Module Scopes"]
        FnScope["Function Scopes"]
        BlockScope["Block Scopes"]
    end
    
    subgraph "Definition Storage"
        DefMap["definitions: HashMap<String, Definition>"]
        VarMap["variables: HashMap<String, NodeType>"]
        MethodMap["methods: HashMap<String, Vec<Function>>"]
        ImportMap["imports: Vec<Use>"]
    end
    
    subgraph "Symbol Resolution"
        LocalLookup["Local Symbol Lookup"]
        ImportLookup["Import Resolution"]
        ParentLookup["Parent Scope Traversal"]
        QualifiedLookup["Qualified Name Resolution"]
    end
    
    RootScope --> ModScope
    ModScope --> FnScope
    FnScope --> BlockScope
    
    ModScope --> DefMap
    FnScope --> VarMap
    ModScope --> MethodMap
    ModScope --> ImportMap
    
    LocalLookup --> DefMap
    ImportLookup --> ImportMap
    ParentLookup --> LocalLookup
    QualifiedLookup --> ImportLookup
```

## Core Components

### SymbolTable Class

The `SymbolTable` is the main coordinator for all symbol resolution and analysis operations:

| Component    | Type                                 | Purpose                            |
|--------------|--------------------------------------|------------------------------------|
| `scopes`     | `HashMap<u32, ScopeRef>`             | Maps scope IDs to scope references |
| `mod_scopes` | `HashMap<String, ScopeRef>`          | Maps module names to their scopes  |
| `defs`       | `HashMap<(u32, String), Definition>` | Stores qualified definitions       |

### Scope Structure

Each `Scope` maintains its own symbol namespace and hierarchy:

| Field         | Type                               | Description                |
|---------------|------------------------------------|----------------------------|
| `id`          | `u32`                              | Unique scope identifier    |
| `name`        | `String`                           | Qualified scope name       |
| `parent`      | `Option<ScopeRef>`                 | Parent scope reference     |
| `children`    | `Vec<ScopeRef>`                    | Child scopes               |
| `definitions` | `HashMap<String, Definition>`      | Local definitions          |
| `variables`   | `HashMap<String, (u32, NodeType)>` | Local variables with types |
| `methods`     | `HashMap<String, Vec<Function>>`   | Type methods               |
| `imports`     | `Vec<Use>`                         | Import declarations        |

### Definition References

The system uses `DefinitionRef` to handle both resolved and unresolved symbol references:

```rust
pub(crate) enum DefinitionRef {
    Ref(String, Definition),     // Resolved reference
    QualifiedName(String),       // Unresolved qualified name
}
```

## Symbol Resolution Process

### Fixpoint Resolution Algorithm

The `fixpoint_resolver` implements an iterative algorithm to resolve all imports and symbol references:

```mermaid
flowchart TD
    Start["Start Resolution"]
    
    subgraph "Iterative Resolution Loop"
        CheckImports["Check Unresolved Imports"]
        ParsePath["Parse Import Path"]
        ResolveSegments["Resolve Path Segments"]
        UpdateTarget["Update Import Target"]
        CheckProgress["Check Progress Made"]
    end
    
    BuildSymTables["Build Symbol Tables"]
    Complete["Resolution Complete"]
    
    Start --> CheckImports
    CheckImports --> ParsePath
    ParsePath --> ResolveSegments
    ResolveSegments --> UpdateTarget
    UpdateTarget --> CheckProgress
    CheckProgress -->|"Progress Made"| CheckImports
    CheckProgress -->|"No Progress"| BuildSymTables
    BuildSymTables --> Complete
```

### Symbol Lookup Hierarchy

The system resolves symbols through a hierarchical lookup process:

```mermaid
flowchart TD
    LookupStart["Symbol Lookup Request"]
    
    subgraph "Local Scope Search"
        CheckLocal["Check Local Definitions"]
        CheckVars["Check Local Variables"]
        CheckImports["Check Import Bindings"]
    end
    
    subgraph "Parent Traversal"
        GetParent["Get Parent Scope"]
        RecursiveLookup["Recursive Lookup"]
    end
    
    subgraph "Resolution Types"
        DefRef["DefinitionRef::Ref"]
        QualName["DefinitionRef::QualifiedName"]
        NotFound["Symbol Not Found"]
    end
    
    LookupStart --> CheckLocal
    CheckLocal -->|"Found"| DefRef
    CheckLocal -->|"Not Found"| CheckVars
    CheckVars -->|"Found"| DefRef
    CheckVars -->|"Not Found"| CheckImports
    CheckImports -->|"Found"| DefRef
    CheckImports -->|"Not Found"| GetParent
    GetParent --> RecursiveLookup
    RecursiveLookup --> DefRef
    RecursiveLookup --> QualName
    RecursiveLookup --> NotFound
```

## Type Inference System

### Expression Type Analysis

The system provides sophisticated type inference for expressions through `infer_expr_type`:

| Expression Type | Type Resolution Strategy           |
|-----------------|------------------------------------|
| `Identifier`    | Variable lookup or definition type |
| `Literal`       | Built-in primitive types           |
| `Binary`        | Operand type matching              |
| `MethodCall`    | Method signature analysis          |
| `FunctionCall`  | Function return type               |
| `MemberAccess`  | Struct field types                 |

### Type Inference Flow

```mermaid
flowchart TD
    ExprInput["Expression Input"]
    
    subgraph "Type Analysis"
        IdentAnalysis["Identifier Analysis"]
        LiteralAnalysis["Literal Type Mapping"]
        BinaryAnalysis["Binary Operation Types"]
        CallAnalysis["Function/Method Call Types"]
        MemberAnalysis["Member Access Types"]
    end
    
    subgraph "Symbol Resolution"
        VarLookup["Variable Type Lookup"]
        DefLookup["Definition Type Lookup"]
        ReturnType["Function Return Type"]
        FieldType["Struct Field Type"]
    end
    
    subgraph "Type Results"
        ConcreteType["Concrete Type"]
        GenericType["Generic Type"]
        EmptyType["NodeType::Empty"]
    end
    
    ExprInput --> IdentAnalysis
    ExprInput --> LiteralAnalysis
    ExprInput --> BinaryAnalysis
    ExprInput --> CallAnalysis
    ExprInput --> MemberAnalysis
    
    IdentAnalysis --> VarLookup
    IdentAnalysis --> DefLookup
    CallAnalysis --> ReturnType
    MemberAnalysis --> FieldType
    
    VarLookup --> ConcreteType
    DefLookup --> ConcreteType
    ReturnType --> ConcreteType
    FieldType --> ConcreteType
    LiteralAnalysis --> ConcreteType
    
    BinaryAnalysis --> ConcreteType
    BinaryAnalysis --> GenericType
    
    ConcreteType --> EmptyType
    GenericType --> EmptyType
```

## Analysis Capabilities for Detectors

### Symbol Origin Tracking

The `lookup_symbol_origin` function provides detailed provenance information for symbols:

```mermaid
flowchart TD
    OriginRequest["Symbol Origin Request"]
    
    subgraph "Variable Analysis"
        CheckVar["Check Variable Definition"]
        FindStmt["Find Declaration Statement"]
        TraceParam["Trace Function Parameter"]
        FindCallSites["Find Call Sites"]
    end
    
    subgraph "Definition Analysis"
        CheckDef["Check Definition"]
        GetDefType["Get Definition Type"]
    end
    
    subgraph "Origin Types"
        FnParam["Function Parameter"]
        LetStmt["Let Statement"]
        Definition["Definition"]
        Assignment["Assignment"]
    end
    
    OriginRequest --> CheckVar
    CheckVar --> FindStmt
    FindStmt --> TraceParam
    TraceParam --> FindCallSites
    
    OriginRequest --> CheckDef
    CheckDef --> GetDefType
    
    FindStmt --> LetStmt
    TraceParam --> FnParam
    GetDefType --> Definition
    FindCallSites --> Assignment
```

### Detector API Integration

The symbol table provides high-level APIs used by detectors:

| API Function           | Purpose                      | Usage Example                              |
|------------------------|------------------------------|--------------------------------------------|
| `get_expression_type`  | Get type of any expression   | Type checking in `extend_ttl_with_max_ttl` |
| `lookup_symbol_origin` | Track symbol provenance      | Variable flow analysis                     |
| `get_symbol_type`      | Get variable type in scope   | Type validation                            |
| `get_function_by_name` | Resolve function definitions | Function inlining                          |

## Integration with Detector Framework

### Type-Based Analysis in Detectors

The `ExtendTtlWithMaxTtl` detector demonstrates sophisticated type analysis:

```mermaid
flowchart TD
    DetectorStart["Detector Analysis Start"]
    
    subgraph "Method Call Detection"
        FindExtendTtl["Find extend_ttl Calls"]
        CheckBaseType["Check Base Type"]
        ValidateStorage["Validate Storage Type"]
    end
    
    subgraph "Parameter Analysis"
        GetParameter["Get max_ttl Parameter"]
        AnalyzeExpr["Analyze Expression Type"]
        TraceOrigin["Trace Symbol Origin"]
    end
    
    subgraph "Type Validation"
        CheckMethod["Check max_ttl Method"]
        ValidateReturn["Validate Return Type"]
        ConfirmStorage["Confirm Storage Instance"]
    end
    
    DetectorStart --> FindExtendTtl
    FindExtendTtl --> CheckBaseType
    CheckBaseType --> ValidateStorage
    ValidateStorage --> GetParameter
    GetParameter --> AnalyzeExpr
    AnalyzeExpr --> TraceOrigin
    TraceOrigin --> CheckMethod
    CheckMethod --> ValidateReturn
    ValidateReturn --> ConfirmStorage
```

### Symbol Analysis in Practice

The detector framework leverages symbol table capabilities for complex analysis patterns:

```rust
// Type checking for method calls
codebase.get_expression_type(mc.base.id()).name().starts_with("soroban_sdk::storage")

// Symbol origin tracking
codebase.lookup_symbol_origin(scope_container.id(), id.name.as_str())

// Function resolution
codebase.get_function_by_name(scope_id, function_call.function_name.as_str())
```

