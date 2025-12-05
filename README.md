# schema_strict workspace

Two crates for producing OpenAI-style "strict" JSON Schemas and compact string-enum schemas.

## Crates
- **schema_strict** — runtime helpers that transform/validate `schemars` output to match OpenAI strict expectations (`required`, `additionalProperties: false`, descriptions everywhere, disallow `oneOf`/`allOf`/`not`, etc.). Re-exports the `#[string_enum]` attribute for convenience.
- **schema_strict_macros** — proc-macro crate providing `#[string_enum]` for unit enums; honors `serde` rename/rename_all and can emit nullable variants.

## Adding to another workspace
After this repo is published, depend on the main crate (the macro is re-exported):
```toml
[dependencies]
schema_strict = { git = "https://github.com/<org>/schema_strict.git" }
```
For local development with a sibling checkout:
```toml
[dependencies]
schema_strict = { path = "../schema_strict/schema_strict" }
```

## Quick start
```rust
use schema_strict::{strict_schema_for_type, string_enum};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[string_enum(description = "Fulfillment modes")]
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
enum FulfillmentMode { Delivery, Pickup }

#[derive(Debug, Serialize, Deserialize, JsonSchema)]
#[schemars(description = "Request selecting a fulfillment mode.")]
struct FulfillmentRequest {
    #[schemars(description = "Chosen mode.")]
    mode: FulfillmentMode,
}

let (schema, report) = strict_schema_for_type::<FulfillmentRequest>();
assert!(report.is_valid());
```

## Development
- Workspace uses Rust 2024 edition and resolver 2.
- Run `cargo check` or `cargo test -p schema_strict` to validate changes.

## License
MIT
