# schema_strict_macros

Proc-macro helpers for `schema_strict`, focused on OpenAI strict-mode string enums.

## #[string_enum]
Attribute macro for unit enums that need compact `type/enum` schemas.

**Signature**
```rust
#[string_enum(description = "optional description", nullable)] // nullable is optional
enum MyEnum { ... }
```

**Behavior**
- Generates a `schemars::JsonSchema` impl that returns a compact schema:
  - Non-nullable: `{"type": "string", "enum": ["v1","v2"]}`
  - Nullable: `{"type": ["string","null"], "enum": ["v1","v2"]}` (no `null` in the enum list to satisfy OpenAI strict mode).
- Respects `#[serde(rename = "...")]` and `#[serde(rename_all = "...")]`; the serialized names become the enum values.
- Compile-time checks: only unit variants are allowed; unsupported `rename_all` values produce a compile error.
- Inline schema (no `$ref`) to match OpenAI tool/response expectations.

**Example**
```rust
use schema_strict::{string_enum, strict_schema_for_type};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[string_enum(description = "Fulfillment modes")]
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
enum FulfillmentMode { Delivery, StorePickup }

#[derive(Debug, Serialize, Deserialize, JsonSchema)]
#[schemars(description = "Request selecting a fulfillment mode.")]
struct FulfillmentRequest {
    #[schemars(description = "Chosen mode.")]
    mode: FulfillmentMode,
}

let (schema, report) = strict_schema_for_type::<FulfillmentRequest>();
assert!(report.is_valid());
```

## When to use
- Anytime you expose an enum of string literals to OpenAI functions/responses; avoids hand-maintaining variant lists.
- Use `nullable` instead of wrapping the enum in `Option` when you need explicit `null` in the type list.

