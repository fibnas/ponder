//! `ponder` — a terminal mind-map tool.
//!
//! This library exposes the core types used by the `ponder` TUI binary.
//! The TUI itself lives in `src/main.rs`.

// Re-export types if you want (optional)
// pub use crate::mindmap::{MindMap, Node};

/// Simple version string for external tools.
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
