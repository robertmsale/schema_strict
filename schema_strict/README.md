# schema_strict

Strict JSON Schema utilities for OpenAI-style function/tool/structured-output requests.

## What it does
- Enforces OpenAI “strict” expectations: root object only, required properties, `additionalProperties: false`, descriptions everywhere, no `oneOf`/`allOf`/`not`, no array `items` lists, and `$ref` has no siblings.
- Produces `Schema` (schemars) or `Value` that is validated and annotated with a `ValidationReport` explaining any remaining issues.
- Supplies compact string-enum builders that match OpenAI’s preferred `type/enum` form.

## Core APIs
- `strict_schema_for_type<T: JsonSchema>() -> (Schema, ValidationReport)`: generate + transform + validate.
- `strict_value_for_type<T: JsonSchema>() -> (serde_json::Value, ValidationReport)`: JSON form plus report.
- `strict_schema_for!(T)`: macro shorthand for the function above.
- `string_enum_schema(variants: &[&str], description: Option<&str>) -> Schema`: string enum (`type: "string"`, `enum: [...]`).
- `string_enum_nullable_schema(variants: &[&str], description: Option<&str>) -> Schema`: nullable string enum (`type: ["string","null"]`, `enum` excludes `null` to satisfy OpenAI strict mode).
- Re-exported proc macro `#[string_enum(...)]` from `schema_strict_macros` to derive the compact enum schema directly from an enum (respects `serde` rename/rename_all, optional `description`, optional `nullable` flag).

## Usage patterns
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

## Design rules (enforced)
- Objects: `required` present for all fields; `additionalProperties: false`.
- Descriptions: root object + every property must have a non-empty description.
- Disallowed keywords flagged: `oneOf`, `allOf`, `not`, `if/then/else`, `contains`, `propertyNames`, `additionalItems`, `items` as array; `$ref` cannot have siblings.
- Root must be an object (not bare oneOf/anyOf).

## When to use what
- For structs/records: derive `JsonSchema` + call `strict_schema_for_type`.
- For enums of strings: prefer `#[string_enum]` so variant changes are reflected automatically; use `nullable` flag instead of wrapping in `Option`.
- Avoid `schema_for!` directly in OpenAI-facing code; go through the strict helpers so violations are caught early.

## ValidationReport
- `is_valid()` is the quick gate.
- Contains lists for missing descriptions, unsupported keywords, and root-shape problems — log them in tests to see all issues at once.

## Limitations
- Only targets OpenAI “strict” JSON Schema expectations; not a general-purpose JSON Schema linter.
- Nullable enums emit `type: ["string","null"]` and keep `enum` free of `null` to pass OpenAI strict validation.

