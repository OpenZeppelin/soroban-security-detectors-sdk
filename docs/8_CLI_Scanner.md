# CLI Scanner

This document covers the `soroban-scanner` command-line interface, which serves as the primary entry point for executing
security detectors against Soroban smart contract codebases. The CLI Scanner orchestrates the detection process, manages
configuration, and formats results for various output targets.

For information about the underlying detector framework and implementation details,
see [Security Detectors](7_Security_Detectors.md). For details about the AST system that powers the analysis,
see [AST System](3_AST_System.md).

## Purpose and Architecture

The CLI Scanner (`soroban-scanner`) is a standalone executable that provides a command-line interface for running
security analysis on Soroban smart contracts. It acts as the orchestration layer that coordinates between the core SDK
components and the detector implementations.

### Core Components Architecture

```mermaid
graph TB
    subgraph "CLI_Layer"
        CLI["soroban-scanner CLI"]
        Args["Command Arguments Parser"]
        Config["Configuration Manager"]
    end
    
    subgraph "Engine_Layer"
        Engine["Detector Engine"]
        Loader["Detector Loader"]
        Filter["Detector Filter"]
    end
    
    subgraph "Analysis_Layer"
        Codebase["Codebase Builder"]
        Detectors["Security Detectors"]
        Results["DetectorResult Collection"]
    end
    
    subgraph "Output_Layer"
        Reporter["Report Generator"]
        Formatter["Output Formatter"]
        Targets["Output Targets"]
    end
    
    CLI --> Args
    Args --> Config
    Config --> Engine
    
    Engine --> Loader
    Engine --> Filter
    Loader --> Detectors
    Filter --> Detectors
    
    Engine --> Codebase
    Codebase --> Detectors
    Detectors --> Results
    
    Results --> Reporter
    Reporter --> Formatter
    Formatter --> Targets
    
    subgraph "External_Integration"
        OZI["OpenZeppelin Inspector"]
        CICD["CI/CD Pipelines"]
        Console["Console Output"]
    end
    
    Targets --> OZI
    Targets --> CICD
    Targets --> Console
```

## Command-Line Interface

The CLI Scanner provides a structured command interface with the primary `scan` command and various configuration
options.

### Basic Usage Pattern

```mermaid
flowchart TD
    Start["soroban-scanner invocation"]
    
    subgraph "Command_Parsing"
        Cmd["scan command"]
        Path["target path parsing"]
        Options["option parsing"]
    end
    
    subgraph "Configuration_Resolution"
        DetectorList["detector filtering"]
        ProjectRoot["project root resolution"]
        ExternalLibs["external library loading"]
    end
    
    subgraph "Execution_Flow"
        CodebaseLoad["codebase loading"]
        DetectorRun["detector execution"]
        ResultCollection["result aggregation"]
    end
    
    subgraph "Output_Generation"
        ReportGen["report generation"]
        Format["format selection"]
        Destination["output destination"]
    end
    
    Start --> Cmd
    Cmd --> Path
    Path --> Options
    Options --> DetectorList
    DetectorList --> ProjectRoot
    ProjectRoot --> ExternalLibs
    ExternalLibs --> CodebaseLoad
    CodebaseLoad --> DetectorRun
    DetectorRun --> ResultCollection
    ResultCollection --> ReportGen
    ReportGen --> Format
    Format --> Destination
```

Sources: [README.md:40-63]()

### Command Structure

| Command          | Purpose                             | Example                                          |
|------------------|-------------------------------------|--------------------------------------------------|
| `scan`           | Execute security analysis           | `soroban-scanner scan path/to/contracts`         |
| `--detectors`    | Filter specific detectors           | `--detectors auth_missing unchecked_ft_transfer` |
| `--project-root` | Set project root for relative paths | `--project-root path/to`                         |
| `--load`         | Load external detector library      | `--load path/to/libmy_detector.so`               |

## Detector Engine Integration

The CLI Scanner integrates with the detector framework through a structured execution engine that manages detector
lifecycle and result aggregation.

### Detector Execution Flow

```mermaid
sequenceDiagram
    participant CLI as "CLI Interface"
    participant Engine as "Detector Engine"
    participant Loader as "Detector Loader"
    participant Codebase as "Codebase Builder"
    participant Detectors as "Security Detectors"
    participant Reporter as "Report Generator"
    
    CLI->>Engine: "initiate scan"
    Engine->>Loader: "load available detectors"
    Loader-->>Engine: "detector registry"
    
    Engine->>Codebase: "build codebase from path"
    Codebase-->>Engine: "SealedCodebase"
    
    Engine->>Detectors: "execute detector.check()"
    Detectors-->>Engine: "DetectorResult"
    
    Engine->>Reporter: "generate report from results"
    Reporter-->>CLI: "formatted output"
    
    CLI->>CLI: "output to destination"
```

The detector engine manages the execution lifecycle, ensuring proper initialization of the codebase and coordinated
execution of all enabled detectors.

## Output and Reporting

The CLI Scanner generates structured reports that can be consumed by various downstream systems and tools.

### Report Generation Pipeline

```mermaid
graph LR
    subgraph "Input_Data"
        Results["DetectorResult[]"]
        Metadata["Detector Metadata"]
        Config["Scanner Configuration"]
    end
    
    subgraph "Report_Processing"
        Aggregator["Result Aggregator"]
        Formatter["Output Formatter"]
        Template["Report Template"]
    end
    
    subgraph "Output_Formats"
        Console["Console Output"]
        YAML["YAML Reports"]
        JSON["JSON Reports"]
        Inspector["Inspector Format"]
    end
    
    Results --> Aggregator
    Metadata --> Aggregator
    Config --> Aggregator
    
    Aggregator --> Formatter
    Formatter --> Template
    
    Template --> Console
    Template --> YAML
    Template --> JSON
    Template --> Inspector
```

## OpenZeppelin Inspector Integration

The CLI Scanner is fully compatible with OpenZeppelin Inspector's custom scanner interface, enabling seamless
integration into Inspector workflows.

### Inspector Integration Architecture

```mermaid
graph TB
    subgraph "Inspector_Environment"
        Inspector["OpenZeppelin Inspector"]
        ScannerRegistry["Scanner Registry"]
        InspectorCLI["inspector scan command"]
    end
    
    subgraph "Installation_Methods"
        LocalInstall["Local Path Installation"]
        URLInstall["URL Installation"]
        ReleaseInstall["Release Download"]
    end
    
    subgraph "Scanner_Interface"
        SorobanScanner["soroban-scanner"]
        MetadataProvider["Metadata Provider"]
        ResultsFormatter["Results Formatter"]
    end
    
    Inspector --> ScannerRegistry
    ScannerRegistry --> LocalInstall
    ScannerRegistry --> URLInstall
    ScannerRegistry --> ReleaseInstall
    
    LocalInstall --> SorobanScanner
    URLInstall --> SorobanScanner
    ReleaseInstall --> SorobanScanner
    
    InspectorCLI --> SorobanScanner
    SorobanScanner --> MetadataProvider
    SorobanScanner --> ResultsFormatter
    
    MetadataProvider --> Inspector
    ResultsFormatter --> Inspector
```

### Integration Commands

The integration supports multiple installation methods:

```bash
# Local installation
inspector scanner install /path/to/soroban-scanner

# URL-based installation
inspector scanner install https://github.com/OpenZeppelin/soroban-security-detectors-sdk/releases/latest/download/soroban-scanner-<os>-latest-v<version>.zip

# Execution through Inspector
inspector scan --scanner soroban-scanner path/to/your/soroban/project
```

## Configuration and Customization

The CLI Scanner supports various configuration options for customizing analysis behavior and output formatting.

### Configuration Options

| Option             | Purpose                                | Default           | Usage                             |
|--------------------|----------------------------------------|-------------------|-----------------------------------|
| Target Path        | Specifies contracts to analyze         | Required          | `scan path/to/contracts`          |
| Detector Filter    | Limits execution to specific detectors | All detectors     | `--detectors detector1 detector2` |
| Project Root       | Sets base path for relative references | Current directory | `--project-root path/to`          |
| External Libraries | Loads additional detector libraries    | None              | `--load path/to/lib.so`           |

### Workspace Integration

The CLI Scanner integrates with Cargo workspace structures for development and testing:

```bash
# Development execution
cargo run -p soroban-scanner -- scan path/to/contracts

# Workspace build
cargo build --workspace
```

## CI/CD Integration

The CLI Scanner is designed for integration into automated pipelines and continuous integration workflows.

### CI/CD Integration Points

```mermaid
graph TB
    subgraph "CI_Pipeline"
        Trigger["Pipeline Trigger"]
        Checkout["Code Checkout"]
        Build["Workspace Build"]
        Scan["Security Scan"]
        Report["Report Generation"]
        Decision["Pass/Fail Decision"]
    end
    
    subgraph "Scanner_Integration"
        SorobanScanner["soroban-scanner"]
        DetectorExecution["Detector Execution"]
        ResultsOutput["Results Output"]
    end
    
    subgraph "Pipeline_Outcomes"
        Pass["Pipeline Success"]
        Fail["Pipeline Failure"]
        Artifacts["Report Artifacts"]
    end
    
    Trigger --> Checkout
    Checkout --> Build
    Build --> Scan
    
    Scan --> SorobanScanner
    SorobanScanner --> DetectorExecution
    DetectorExecution --> ResultsOutput
    
    ResultsOutput --> Report
    Report --> Decision
    
    Decision --> Pass
    Decision --> Fail
    Decision --> Artifacts
```

The scanner's exit codes and structured output format make it suitable for automated decision-making in CI/CD pipelines,
allowing builds to fail when security issues are detected.