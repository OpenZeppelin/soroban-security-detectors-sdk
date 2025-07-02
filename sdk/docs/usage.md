# SDK Usage

## Building a Codebase

```rust
use soroban_security_detectors_sdk::build_codebase;

let codebase = build_codebase("path/to/contract");
```

## Writing a Detector

1. Define a function annotated with the `#[detector(type_name = "MyDetector")]` macro.
2. Implement the `check` signature as specified by the `Detector` trait.
3. Return a vector of `DetectorResult` instances to report issues.

See the API reference for full details on macros and traits.