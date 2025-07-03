# Writing Detectors

To add a new detector to the `detectors` crate:

1. Create a Rust source file under `detectors/src/`.
2. Define a function annotated with `#[detector(type_name = "YourDetector")]`.
3. Match the `Detector` trait signature and return `DetectorResult` entries.
4. Add unit tests under `detectors/tests/` to cover your detector logic.

Refer to existing detectors for examples and to the SDK API docs for trait details.