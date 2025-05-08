use clap::{Parser, Subcommand};

#[derive(Subcommand, Debug)]
pub(crate) enum Commands {
    Scan {
        code: Vec<std::path::PathBuf>,
        #[arg(long = "detectors", required = false, value_parser, num_args = 1..)]
        detectors: Option<Vec<String>>,
        #[arg(long = "project-root", required = false, value_parser)]
        project_root: Option<std::path::PathBuf>,
        #[arg(long = "load", required = false, value_parser)]
        load_lib: Option<std::path::PathBuf>,
    },
    Metadata,
}

#[derive(Parser, Debug)]
#[command(name = "soroban-scanner", about = "Soroban Scanner")]
pub(crate) struct Cli {
    #[command(subcommand)]
    pub(crate) command: Commands,
}
