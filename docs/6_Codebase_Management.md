# Codebase Management

This document covers the lifecycle management of codebases in the Soroban Security Detectors SDK, focusing on how source
files are parsed, organized, and made available for analysis through a type-safe state management system.

The codebase management system handles the transition from raw source files to a fully analyzable code model through
distinct construction and sealing phases. For information about AST node types and structures,
see [AST Node Types](4_AST_Node_Types.md). For symbol resolution and type inference capabilities,
see [Symbol Table and Analysis](5_Symbol_Table_and_Analysis.md).

## State Management Overview

The codebase uses a type-state pattern to enforce correct usage through the compilation lifecycle. The `Codebase<S>`
struct is parameterized by state, ensuring that construction operations are only available during the open phase and
analysis operations are only available after sealing.

### Codebase State Diagram

```mermaid
stateDiagram-v2
    [*] --> OpenState
    
    state OpenState {
        [*] --> FileAdding
        FileAdding --> parse_and_add_file: "Add source files"
        parse_and_add_file --> FileAdding
        FileAdding --> ASTBuilding: "All files added"
        ASTBuilding --> SymbolResolution: "Parse complete"
        SymbolResolution --> ReadyToSeal: "Symbols resolved"
    }
    
    OpenState --> SealedState: "build_api()"
    
    state SealedState {
        [*] --> AnalysisReady
        AnalysisReady --> ContractAccess: "contracts()"
        AnalysisReady --> FunctionAccess: "functions()"
        AnalysisReady --> TypeInference: "get_expression_type()"
        AnalysisReady --> SymbolLookup: "lookup_symbol_origin()"
        AnalysisReady --> DetectorExecution: "Detector.check()"
    }
    
    SealedState --> [*]
```

## Construction Process

The codebase construction follows a multi-phase process that transforms raw source files into a fully analyzed code
model.

### Construction Pipeline

```mermaid
flowchart TD
    A["Source Files Map<br/>HashMap<String, String>"] --> B["build_codebase()"]
    B --> C["Codebase<OpenState>"]
    C --> D["parse_and_add_file()"]
    D --> E["syn::parse_file()"]
    E --> F["AST Construction"]
    F --> G["NodesStorage"]
    G --> H["SymbolTable Building"]
    H --> I["fixpoint_resolver()"]
    I --> J["build_api()"]
    J --> K["Codebase<SealedState>"]
    
    subgraph "OpenState Operations"
        D
        E
        F
        G
        H
        I
    end
    
    subgraph "SealedState Ready"
        K
        L["contracts()"]
        M["functions()"]
        N["get_expression_type()"]
        O["lookup_symbol_origin()"]
    end
    
    K --> L
    K --> M
    K --> N
    K --> O
```

### File Processing

The system processes files through several steps:

| Phase               | Function                | Purpose                                   |
|---------------------|-------------------------|-------------------------------------------|
| Parsing             | `parse_and_add_file()`  | Converts source text to `syn::File` AST   |
| Storage             | `add_node()`            | Stores AST nodes in `NodesStorage`        |
| Symbol Resolution   | `build_symbol_tables()` | Builds symbol table and scope information |
| Fixpoint Resolution | `fixpoint_resolver()`   | Resolves imports and external references  |
| Sealing             | `build_api()`           | Transitions to immutable analysis state   |

## Sealing Process

The sealing process transitions the codebase from a mutable construction state to an immutable analysis state, enabling
type-safe access to analysis APIs.

### Sealing Implementation

```mermaid
sequenceDiagram
    participant CB as "Codebase<OpenState>"
    participant PC as "ParserCtx"
    participant ST as "SymbolTable"
    participant NS as "NodesStorage"
    participant SC as "Codebase<SealedState>"
    
    CB->>PC: "new(node_id_seed, loader, scope)"
    PC->>NS: "parse() - Add nodes"
    PC->>ST: "build_symbol_tables()"
    CB->>ST: "fixpoint_resolver()"
    CB->>NS: "seal()"
    CB->>SC: "Create sealed codebase"
    SC->>SC: "Build contract cache"
    SC->>SC: "Add function routes"
```

The sealing process performs several critical operations:

1. **Parser Context Creation**: Creates a `ParserCtx` with a synthetic root scope
2. **AST Parsing**: Processes all files and builds the node tree
3. **Symbol Resolution**: Resolves all imports and external references through `fixpoint_resolver`
4. **Storage Sealing**: Finalizes parent-child relationships in `NodesStorage`
5. **Contract Cache**: Builds cache of contract structures for efficient access
6. **Route Establishment**: Links contract methods and functions to their parent structs

## Storage Management

The `NodesStorage` system manages AST nodes and their relationships through a graph-based approach.

### Node Storage Architecture

```mermaid
graph TD
    NS["NodesStorage"] --> NR["node_routes: Vec<NodeRoute>"]
    NS --> ND["nodes: Vec<NodeKind>"]
    NS --> FC["file_content_map: HashMap<u32, String>"]
    
    NR --> NR1["NodeRoute { id: u32, parent: Option<u32>, children: Vec<u32> }"]
    
    ND --> NK1["NodeKind::File"]
    ND --> NK2["NodeKind::Definition"]
    ND --> NK3["NodeKind::Expression"]
    ND --> NK4["NodeKind::Statement"]
    
    subgraph "Navigation Methods"
        FM["find_node(id)"]
        FP["find_parent_node(id)"]
        FF["find_node_file(id)"]
        SC["get_node_source_code(id)"]
    end
    
    NS --> FM
    NS --> FP
    NS --> FF
    NS --> SC
```

### Storage Operations

The storage system provides several key operations:

- **Node Lookup**: `find_node(id)` retrieves any node by its unique identifier
- **Parent Traversal**: `find_parent_node(id)` walks up the hierarchy
- **File Resolution**: `find_node_file(id)` finds the source file containing a node
- **Source Code Access**: `get_node_source_code(id)` extracts original source text

## API Access Patterns

Once sealed, the codebase provides rich APIs for analysis and traversal.

### Sealed Codebase APIs

```mermaid
classDiagram
    class Codebase_SealedState {
        +files() Iterator~File~
        +contracts() Iterator~Contract~
        +functions() Iterator~Function~
        +get_node_source_code(id) Option~String~
        +find_node_file(id) Option~File~
        +get_children_cmp(id, comparator) Vec~NodeKind~
        +get_children_cmp_cast(id, comparator) Vec~T~
        +get_expression_type(id) NodeType
        +get_symbol_type(scope_id, symbol) Option~NodeType~
        +lookup_symbol_origin(scope_id, symbol) Option~NodeKind~
        +inline_function(func) Function
        +compare_types(a, b) bool
    }
    
    class Contract {
        +id u32
        +name String
        +methods RefCell~Vec~Function~~
        +functions RefCell~Vec~Function~~
        +type_aliases RefCell~Vec~TypeAlias~~
        +constants RefCell~Vec~Constant~~
    }
    
    class Function {
        +id u32
        +name String
        +parameters Vec~Parameter~
        +returns RefCell~NodeType~
        +body Option~Block~
    }
    
    Codebase_SealedState --> Contract
    Codebase_SealedState --> Function
    Contract --> Function
```

### Analysis Capabilities

The sealed codebase provides sophisticated analysis capabilities:

| Method                                   | Purpose                            | Return Type          |
|------------------------------------------|------------------------------------|----------------------|
| `contracts()`                            | Enumerate all contract structures  | `Iterator<Contract>` |
| `functions()`                            | Enumerate all function definitions | `Iterator<Function>` |
| `get_expression_type(id)`                | Infer expression types             | `NodeType`           |
| `lookup_symbol_origin(scope_id, symbol)` | Find symbol definitions            | `Option<NodeKind>`   |
| `inline_function(func)`                  | Inline function calls for analysis | `Function`           |
| `get_children_cmp(id, comparator)`       | Find child nodes matching criteria | `Vec<NodeKind>`      |

### Contract Construction

The system automatically constructs `Contract` objects from `struct` definitions marked with contract attributes,
gathering associated implementation blocks:

```mermaid
graph LR
    SD["Struct Definition<br/>#[contract]"] --> CF["construct_contract_from_struct()"]
    IB["Implementation Blocks<br/>impl StructName"] --> CF
    CF --> CC["Contract Cache<br/>RefCell<HashMap<u32, Contract>>"]
    CF --> CM["Contract Methods<br/>RefCell<Vec<Function>>"]
    CF --> CFN["Contract Functions<br/>RefCell<Vec<Function>>"]
    CF --> CTA["Type Aliases<br/>RefCell<Vec<TypeAlias>>"]
    CF --> CON["Constants<br/>RefCell<Vec<Constant>>"]
```

