# https://lib.rs/crates/cargo-llvm-cov
# https://doc.rust-lang.org/rustc/instrument-coverage.html
cargo install cargo-llvm-cov || exit 1
cargo llvm-cov --open
