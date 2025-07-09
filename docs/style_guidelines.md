# Style Guidelines

## Rust

- Follow the Rust API guidelines.
- Use `rustfmt` (`cargo fmt`) to format code.
- Lint with `clippy --all-targets -- -D warnings`.
- Enable `#![warn(clippy::pedantic)]` in code.
- Write documentation comments (`///`) for public items.

## Commit Messages

- Use imperative, present tense.
- Reference issue numbers when applicable.

## Branching

- Use feature branches named `feature/...` or `bugfix/...`.
