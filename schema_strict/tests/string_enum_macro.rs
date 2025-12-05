use schema_strict::{strict_schema_for_type, string_enum};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

#[string_enum(description = "Fulfillment modes")]
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
enum FulfillmentMode {
    Delivery,
    StorePickup,
    DroneDrop,
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
#[schemars(description = "Request selecting a fulfillment mode.")]
struct FulfillmentRequest {
    #[schemars(description = "Chosen fulfillment mode.")]
    fulfillment: FulfillmentMode,
}

#[test]
fn string_enum_macro_generates_compact_enum() {
    let (schema, report) = strict_schema_for_type::<FulfillmentRequest>();
    assert!(report.is_valid(), "report: {:?}", report);

    let obj = schema.as_object().expect("root object");
    assert_eq!(
        obj.get("strict"),
        Some(&json!(true)),
        "strict flag should be set"
    );
    assert_eq!(
        obj.get("additionalProperties"),
        Some(&json!(false)),
        "root additionalProperties must be false"
    );
    assert_eq!(
        obj.get("required"),
        Some(&json!(["fulfillment"])),
        "fulfillment must be required"
    );

    let props = obj
        .get("properties")
        .and_then(|p| p.as_object())
        .expect("properties object");
    let f = props
        .get("fulfillment")
        .and_then(|v| v.as_object())
        .expect("fulfillment schema");
    assert_eq!(f.get("type"), Some(&json!("string")));
    assert_eq!(
        f.get("enum"),
        Some(&json!(["delivery", "store_pickup", "drone_drop"]))
    );
    assert_eq!(
        f.get("description"),
        Some(&json!("Chosen fulfillment mode."))
    );
}

#[string_enum(description = "Shift preference", nullable)]
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
enum ShiftPreference {
    DayShift,
    NightShift,
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
#[schemars(description = "Optional shift selection allowing explicit null.")]
struct PreferencePayload {
    #[schemars(description = "Optional shift; null means no preference.")]
    shift: ShiftPreference,
}

#[test]
fn string_enum_macro_nullable_emits_string_or_null_without_null_enum() {
    let (schema, report) = strict_schema_for_type::<PreferencePayload>();
    assert!(report.is_valid(), "report: {:?}", report);

    let obj = schema.as_object().expect("root object");
    let props = obj
        .get("properties")
        .and_then(|p| p.as_object())
        .expect("properties object");
    let shift = props
        .get("shift")
        .and_then(|v| v.as_object())
        .expect("shift schema");

    assert_eq!(shift.get("type"), Some(&json!(["string", "null"])));
    assert_eq!(
        shift.get("enum"),
        Some(&json!(["day-shift", "night-shift"])),
        "null must not appear in enum list"
    );
    assert_eq!(
        shift.get("description"),
        Some(&json!("Optional shift; null means no preference."))
    );
}
