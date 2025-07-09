# AST Node Types

This document details the Abstract Syntax Tree (AST) node types that represent parsed Rust code in the Soroban Security
Detectors SDK. These nodes form the foundation for static analysis by providing a structured representation of source
code that security detectors can traverse and analyze.

The AST node types are organized into several categories representing different language constructs: expressions,
statements, definitions, functions, types, and literals. For information about how these nodes are constructed and
managed, see the AST System overview. For details about symbol resolution and type inference that operates on these
nodes, see [Symbol Table and Analysis](5_Symbol_Table_and_Analysis.md).

## AST Node Hierarchy Overview

The AST system is built around a central `NodeKind` enum that unifies all node types under a common interface. Each node
implements the `Node` trait, providing unique identifiers, location information, and child relationships.

```mermaid
graph TB
    subgraph "Core AST Infrastructure"
        NodeKind["NodeKind"]
        Node["Node trait"]
        Location["Location"]
    end
    
    subgraph "Primary Node Categories"
        Expression["Expression"]
        Statement["Statement"] 
        Definition["Definition"]
        Function["Function"]
        Type["Type"]
        Literal["Literal"]
        Misc["Misc"]
        Pattern["Pattern"]
        Directive["Directive"]
    end
    
    subgraph "Expression Variants"
        FunctionCall["FunctionCall"]
        MethodCall["MethodCall"]
        Binary["Binary"]
        If["If"]
        Match["Match"]
        Identifier["Identifier"]
        ExprOther["...34 other variants"]
    end
    
    subgraph "Definition Variants"
        DefFunction["Function"]
        DefStruct["Struct"]
        DefEnum["Enum"]
        DefTrait["Trait"]
        DefImpl["Implementation"]
        DefConst["Const"]
        DefOther["...10 other variants"]
    end
    
    NodeKind --> Expression
    NodeKind --> Statement
    NodeKind --> Definition
    NodeKind --> Function
    NodeKind --> Type
    NodeKind --> Literal
    NodeKind --> Misc
    NodeKind --> Pattern
    NodeKind --> Directive
    
    Expression --> FunctionCall
    Expression --> MethodCall
    Expression --> Binary
    Expression --> If
    Expression --> Match
    Expression --> Identifier
    Expression --> ExprOther
    
    Definition --> DefFunction
    Definition --> DefStruct
    Definition --> DefEnum
    Definition --> DefTrait
    Definition --> DefImpl
    Definition --> DefConst
    Definition --> DefOther
    
    Node --> NodeKind
    Location --> Node
```

## Expression Nodes

The `Expression` enum represents all Rust expression forms and is the most comprehensive node category with 34 variants.
Each expression node tracks whether it represents a return value through the `is_ret` field.

```mermaid
graph LR
    subgraph "Control Flow Expressions"
        If["If"]
        Match["Match"]
        ForLoop["ForLoop"]
        While["While"]
        Loop["Loop"]
        Break["Break"]
        Continue["Continue"]
        Return["Return"]
    end
    
    subgraph "Call Expressions"
        FunctionCall["FunctionCall"]
        MethodCall["MethodCall"]
    end
    
    subgraph "Access Expressions"
        Identifier["Identifier"]
        MemberAccess["MemberAccess"]
        IndexAccess["IndexAccess"]
    end
    
    subgraph "Operation Expressions"
        Binary["Binary"]
        Unary["Unary"]
        Assign["Assign"]
        Cast["Cast"]
    end
    
    subgraph "Literal Expressions"
        Literal["Literal"]
        Array["Array"]
        Tuple["Tuple"]
        EStruct["EStruct"]
    end
    
    subgraph "Block Expressions"
        EBlock["EBlock"]
        Unsafe["Unsafe"]
        TryBlock["TryBlock"]
        Closure["Closure"]
    end
```

### Key Expression Types

| Expression Type | Purpose                | Key Fields                                |
|-----------------|------------------------|-------------------------------------------|
| `FunctionCall`  | Function invocation    | `function_name`, `parameters`             |
| `MethodCall`    | Method invocation      | `method_name`, `base`, `parameters`       |
| `Binary`        | Binary operations      | `left`, `right`, `operator`               |
| `If`            | Conditional expression | `condition`, `then_branch`, `else_branch` |
| `Match`         | Pattern matching       | `expression`, `arms`                      |
| `Identifier`    | Variable reference     | `name`                                    |
| `MemberAccess`  | Field access           | `base`, `member_name`                     |

## Definition Nodes

The `Definition` enum represents top-level items in Rust code including functions, structs, enums, traits, and
implementations. These nodes form the primary structure of modules and contracts.

```mermaid
graph TB
    subgraph "Type Definitions"
        Struct["Struct"]
        Contract["Contract"]
        Enum["Enum"]
        Union["Union"]
        TypeAlias["TypeAlias"]
    end
    
    subgraph "Behavioral Definitions"
        Function["Function"]
        Implementation["Implementation"]
        Trait["Trait"]
        TraitAlias["TraitAlias"]
    end
    
    subgraph "Value Definitions"
        Const["Const"]
        Static["Static"]
    end
    
    subgraph "Module Definitions"
        Module["Module"]
        ExternCrate["ExternCrate"]
        Macro["Macro"]
    end
    
    subgraph "Special Definitions"
        AssocType["AssocType"]
        Plane["Plane"]
    end
    
    Definition --> Struct
    Definition --> Contract
    Definition --> Enum
    Definition --> Union
    Definition --> TypeAlias
    Definition --> Function
    Definition --> Implementation
    Definition --> Trait
    Definition --> TraitAlias
    Definition --> Const
    Definition --> Static
    Definition --> Module
    Definition --> ExternCrate
    Definition --> Macro
    Definition --> AssocType
    Definition --> Plane
```

### Definition Node Details

Each definition node provides `name()` and `visibility()` methods for consistent access to common properties across all
definition types.

| Definition Type  | Purpose             | Key Fields                              |
|------------------|---------------------|-----------------------------------------|
| `Function`       | Function definition | `name`, `parameters`, `body`, `returns` |
| `Struct`         | Struct definition   | `name`, `fields`, `is_contract`         |
| `Implementation` | Impl block          | `for_type`, `functions`, `constants`    |
| `Trait`          | Trait definition    | `name`, `items`, `supertraits`          |
| `Module`         | Module definition   | `name`, `definitions`, `imports`        |
| `Const`          | Constant definition | `name`, `type_`, `value`                |

## Statement and Block Nodes

The `Statement` enum represents executable code units and structural blocks within functions and other code bodies.

```mermaid
graph LR
    subgraph "Statement Types"
        Definition["Definition"]
        Expression["Expression"]
        Block["Block"]
        Let["Let"]
        Macro["Macro"]
    end
    
    subgraph "Block Contents"
        Statements["statements: Vec<Statement>"]
    end
    
    subgraph "Let Statement"
        LetPattern["pattern: Pattern"]
        LetValue["initial_value: Option<Expression>"]
        LetAlt["initial_value_alternative: Option<Expression>"]
    end
    
    Statement --> Definition
    Statement --> Expression
    Statement --> Block
    Statement --> Let
    Statement --> Macro
    
    Block --> Statements
    Let --> LetPattern
    Let --> LetValue
    Let --> LetAlt
```

The `Block` node contains a vector of statements and implements the `Node` trait to provide child access for AST
traversal. The `Let` node represents variable declarations with optional initialization values.

## Type System Nodes

The `Type` enum represents Rust's type system within the AST, including user-defined types, type aliases, and struct
types.

```mermaid
graph TB
    subgraph "Type Variants"
        Typename["Typename"]
        Alias["TypeAlias"]
        Struct["TStruct"]
    end
    
    subgraph "Type Conversion"
        ToTypeNode["to_type_node()"]
        SynParse["syn::parse_str<syn::Type>()"]
        NodeType["NodeType"]
    end
    
    Type --> Typename
    Type --> Alias
    Type --> Struct
    
    Typename --> ToTypeNode
    Alias --> ToTypeNode
    Struct --> ToTypeNode
    
    ToTypeNode --> SynParse
    SynParse --> NodeType
```

### Type Node Structure

| Type Node   | Purpose               | Key Fields                 |
|-------------|-----------------------|----------------------------|
| `Typename`  | Simple type name      | `name`                     |
| `TypeAlias` | Type alias definition | `name`, `visibility`, `ty` |
| `TStruct`   | Struct type reference | `name`, `visibility`, `ty` |

The `Type` enum provides a `to_type_node()` method that converts AST type representations into `NodeType` instances for
type inference and analysis.

## Literal Nodes

The `Literal` enum represents all literal value types in Rust source code, providing typed representations of constant
values.

```mermaid
graph LR
    subgraph "String Literals"
        LString["LString"]
        LBString["LBString"]
        LCString["LCString"]
        LChar["LChar"]
    end
    
    subgraph "Numeric Literals"
        LInt["LInt"]
        LFloat["LFloat"]
        LByte["LByte"]
    end
    
    subgraph "Boolean Literals"
        LBool["LBool"]
    end
    
    Literal --> LString
    Literal --> LBString
    Literal --> LCString
    Literal --> LChar
    Literal --> LInt
    Literal --> LFloat
    Literal --> LByte
    Literal --> LBool
```

### Literal Value Types

| Literal Type | Rust Type         | Value Field     |
|--------------|-------------------|-----------------|
| `LString`    | String literal    | `value: String` |
| `LChar`      | Character literal | `value: char`   |
| `LInt`       | Integer literal   | `value: i128`   |
| `LFloat`     | Float literal     | `value: String` |
| `LBool`      | Boolean literal   | `value: bool`   |
| `LByte`      | Byte literal      | `value: u8`     |

All literal nodes implement the `Node` trait with empty `children()` methods since they represent leaf values in the
AST.

## Function and Parameter Nodes

Function nodes represent Rust function definitions with complete signature information, body statements, and parameter
details.

```mermaid
graph TB
    subgraph "Function Structure"
        Function["Function"]
        FnParameter["FnParameter"]
        Returns["returns: Rc<RefCell<NodeType>>"]
        Body["body: Option<Rc<Block>>"]
    end
    
    subgraph "Function Properties"
        Name["name: String"]
        Visibility["visibility: Visibility"]
        Generics["generics: Vec<String>"]
        Parameters["parameters: Vec<Rc<FnParameter>>"]
        Attributes["attributes: Vec<String>"]
    end
    
    subgraph "Parameter Properties"
        ParamName["name: String"]
        ParamType["type_name: String"]
        IsSelf["is_self: bool"]
        IsMut["is_mut: bool"]
    end
    
    Function --> Name
    Function --> Visibility
    Function --> Generics
    Function --> Parameters
    Function --> Attributes
    Function --> Returns
    Function --> Body
    
    FnParameter --> ParamName
    FnParameter --> ParamType
    FnParameter --> IsSelf
    FnParameter --> IsMut
```

### Function Analysis Methods

The `Function` node provides several analysis methods:

| Method         | Purpose                       | Return Type                             |
|----------------|-------------------------------|-----------------------------------------|
| `parameters()` | Iterate function parameters   | `impl Iterator<Item = Rc<FnParameter>>` |
| `generics()`   | Iterate generic parameters    | `impl Iterator<Item = String>`          |
| `body()`       | Get function body             | `Option<Rc<Block>>`                     |
| `is_public()`  | Check public visibility       | `bool`                                  |
| `is_method()`  | Check if function is a method | `bool`                                  |

## Contract and Struct Nodes

Contract and struct nodes represent Soroban smart contracts and Rust struct definitions, with special handling for
contract-specific attributes and associated items.

```mermaid
graph TB
    subgraph "Struct Node"
        Struct["Struct"]
        StructName["name: String"]
        StructFields["fields: Vec<(String, Type)>"]
        IsContract["is_contract: bool"]
        StructVis["visibility: Visibility"]
    end
    
    subgraph "Contract Node"
        Contract["Contract"]
        ContractName["name: String"]
        ContractFields["fields: Vec<(String, Type)>"]
        Methods["methods: RefCell<Vec<RcFunction>>"]
        Functions["functions: RefCell<Vec<RcFunction>>"]
        TypeAliases["type_aliases: RefCell<Vec<Rc<TypeAlias>>>"]
        Constants["constants: RefCell<Vec<Rc<Const>>>"]
        Macros["macros: RefCell<Vec<Rc<Macro>>>"]
        PlaneDefs["plane_defs: RefCell<Vec<Rc<Plane>>>"]
    end
    
    subgraph "Contract Detection"
        ContractAttr["#[contract]"]
        ContractTypeAttr["#[contracttype]"]
        IsStructContract["is_struct_contract()"]
        IsStructContractType["is_struct_contract_type()"]
    end
    
    Struct --> StructName
    Struct --> StructFields
    Struct --> IsContract
    Struct --> StructVis
    
    Contract --> ContractName
    Contract --> ContractFields
    Contract --> Methods
    Contract --> Functions
    Contract --> TypeAliases
    Contract --> Constants
    Contract --> Macros
    Contract --> PlaneDefs
    
    ContractAttr --> IsStructContract
    ContractTypeAttr --> IsStructContractType
```

### Contract vs Struct Distinction

The `Struct` node includes an `is_contract` field that distinguishes between regular Rust structs and Soroban contract
structs. Contract detection is performed using attribute analysis:

- `is_struct_contract()` checks for `#[contract]` attribute
- `is_struct_contract_type()` checks for `#[contracttype]` attribute

The `Contract` node provides organized access to contract components through methods like `functions()`, `methods()`,
and their corresponding count methods.

## Node Implementation Details

All AST nodes implement the `Node` trait, which provides a consistent interface for traversal and analysis. The trait
includes:

```mermaid
graph LR
    subgraph "Node Trait Methods"
        NodeID["id() -> u64"]
        NodeLocation["location() -> Location"]
        NodeChildren["children() -> Vec<NodeKind>"]
    end
    
    subgraph "Common Node Fields"
        ID["id: u64"]
        Location["location: Location"]
    end
    
    subgraph "Child Relationships"
        ChildExpression["NodeKind::Expression"]
        ChildStatement["NodeKind::Statement"]
        ChildDefinition["NodeKind::Definition"]
        ChildType["NodeKind::Type"]
        ChildPattern["NodeKind::Pattern"]
        ChildLiteral["NodeKind::Literal"]
        ChildMisc["NodeKind::Misc"]
    end
    
    NodeID --> ID
    NodeLocation --> Location
    NodeChildren --> ChildExpression
    NodeChildren --> ChildStatement
    NodeChildren --> ChildDefinition
    NodeChildren --> ChildType
    NodeChildren --> ChildPattern
    NodeChildren --> ChildLiteral
    NodeChildren --> ChildMisc
```

The `children()` method returns a vector of `NodeKind` variants, enabling recursive traversal of the AST. Each node type
implements specific child relationship logic based on its structure - for example, `Binary` expressions return their
left and right operands, while `Function` nodes return their parameters and body statements.

