# SDK API Reference

## `Detector` Trait

Trait that defines the entry point for a security detector. Implementors must provide a `check` method:

```rust
pub trait Detector {
    fn check(&self, codebase: &Codebase) -> Option<Vec<DetectorResult>>;
}
```

## `detector!` and `detectors!` Macros

Macros for registering detectors with a type name and wiring up report templates.

## Other Utilities

- Symbol table
- AST node types
- Codebase builder