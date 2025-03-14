//! My base for libraries.
#![feature(box_patterns)]
#![warn(
    clippy::create_dir,
    clippy::infinite_loop,
    clippy::let_underscore_must_use,
    clippy::missing_panics_doc,
    clippy::return_self_not_must_use,
    clippy::same_name_method,
    clippy::should_panic_without_expect,
    clippy::use_debug,
    missing_copy_implementations,
    rust_2018_idioms
)]
#![warn(clippy::missing_docs_in_private_items, missing_docs)]
#![deny(unsafe_op_in_unsafe_fn, missing_debug_implementations)]
#![cfg_attr(
    not(debug_assertions),
    deny(clippy::dbg_macro, clippy::todo, clippy::unimplemented)
)]
#![cfg_attr(
    feature = "lint_reasons",
    deny(clippy::allow_attributes, clippy::allow_attributes_without_reason)
)]

/// An implementation of the [rope] data structure.
///
/// [rope]: https://en.wikipedia.org/wiki/Rope_(data_structure)
pub mod rope;
pub use rope::Rope;

/// An implementation of the [splay tree] data structure.
///
/// [splay tree]: https://en.wikipedia.org/wiki/Splay_tree
pub mod splay_tree;
