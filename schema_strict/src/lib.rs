use schemars::{JsonSchema, Schema, schema_for, transform::Transform};
use serde_json::{Map as JsonMap, Value, json};

pub use schema_strict_macros::string_enum;

// ---------------------------------------------------------------------------
// Strict-mode schema rules enforced here (OpenAI function-call style):
// 1) Every object must list all its properties in `required`.
// 2) Every object must set `additionalProperties: false`.
// 3) Every property (and the root object) must have a non-empty string `description`.
// 4) Disallowed/flagged keywords: not, if, then, else, contains, propertyNames,
//    additionalItems, allOf, oneOf, prefixItems, items-as-array.
//    anyOf is allowed but still traversed; items-as-object is allowed and traversed.
// 5) `additionalProperties` must be a boolean and must be false; any other value is flagged.
// 6) Traversal visits all nested subschemas so violations are caught deeply.
// 7) Root must be an object with properties; otherwise we record that fact.
// 8) A `$ref` must not have sibling keywords (e.g., description); if present, it is flagged.
// ---------------------------------------------------------------------------

/// Applies the given [`Transform`] to all direct subschemas of the [`Schema`].
pub fn transform_subschemas(t: &mut StrictStatefulTransform, schema: &mut Schema) {
    for (key, value) in schema.as_object_mut().into_iter().flatten() {
        match key.as_str() {
            // Forbidden logical/validation keywords — flag and still recurse so nested descriptions are checked
            "not" | "if" | "then" | "else" | "contains" | "propertyNames" | "additionalItems" => {
                t.unsupported_types_found.push(
                    "not, if, then, else, contains, propertyNames, additionalItems are invalid types"
                        .to_owned(),
                );
                if let Ok(subschema) = value.try_into() {
                    t.transform(subschema);
                }
            }
            "additionalProperties" => {
                // Must be boolean false; any other value is recorded as a violation
                if let Some(val) = value.as_bool() {
                    if val {
                        t.unsupported_types_found
                            .push("additionalProperties must be false".to_owned());
                    }
                } else {
                    t.unsupported_types_found
                        .push("additionalProperties must be a boolean".to_owned());
                }
            }
            "allOf" | "oneOf" | "prefixItems" => {
                // Disallowed structural combinators; record and recurse into branches
                t.unsupported_types_found
                    .push("allOf, oneOf, prefixItems".to_owned());
                if let Some(array) = value.as_array_mut() {
                    for value in array {
                        if let Ok(subschema) = value.try_into() {
                            t.transform(subschema);
                        }
                    }
                }
            }
            "anyOf" => {
                // anyOf is tolerated but still traversed for description/required checks
                if let Some(array) = value.as_array_mut() {
                    for value in array {
                        if let Ok(subschema) = value.try_into() {
                            t.transform(subschema);
                        }
                    }
                }
            }
            // Support `items` array even though this is not allowed in draft 2020-12 (see above
            // comment)
            "items" => {
                // items as array is flagged; items as single schema is allowed
                if let Some(array) = value.as_array_mut() {
                    t.unsupported_types_found.push("items as array".to_owned());
                    for value in array {
                        if let Ok(subschema) = value.try_into() {
                            t.transform(subschema);
                        }
                    }
                } else if let Ok(subschema) = value.try_into() {
                    t.transform(subschema);
                }
            }
            "properties" | "patternProperties" | "$defs" | "definitions" => {
                if let Some(obj) = value.as_object_mut() {
                    for (key, value) in obj.iter_mut() {
                        if let Ok(subschema) = value.try_into() {
                            t.transform(subschema);
                        }
                        // Rule: every property schema must declare a type (unless it's a $ref).
                        if value.get("$ref").is_none() && value.get("type").is_none() {
                            t.unsupported_types_found
                                .push(format!("property '{key}' missing type"));
                        }
                        // Rule: every property must have a non-empty string description.
                        // Exception: if the schema is a pure $ref, skip the description requirement
                        // because OpenAI disallows sibling keywords with $ref.
                        if value.get("$ref").is_none() {
                            match value.get("description") {
                                Some(desc) => {
                                    if let Some(d) = desc.as_str() {
                                        if d.is_empty() {
                                            t.properties_without_descriptions.push(key.to_owned());
                                        }
                                    } else {
                                        t.properties_without_descriptions.push(key.to_owned());
                                    }
                                }
                                None => {
                                    t.properties_without_descriptions.push(key.to_owned());
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

#[derive(Debug, Clone)]
pub struct StrictStatefulTransform {
    root_object_processed: bool,
    root_object_is_object: bool,
    root_requires_properties_message: Option<String>,
    properties_without_descriptions: Vec<String>,
    unsupported_types_found: Vec<String>,
}

impl StrictStatefulTransform {
    fn new() -> StrictStatefulTransform {
        StrictStatefulTransform {
            root_object_processed: false,
            root_object_is_object: false,
            root_requires_properties_message: None,
            properties_without_descriptions: Vec::new(),
            unsupported_types_found: Vec::new(),
        }
    }
}

impl Transform for StrictStatefulTransform {
    fn transform(&mut self, schema: &mut Schema) {
        // Guard: OpenAI forbids any sibling keywords with $ref. If present, record and stop.
        if let Some(obj) = schema.as_object() {
            if obj.get("$ref").is_some() && obj.len() > 1 {
                self.unsupported_types_found
                    .push("$ref must not have sibling keywords".to_string());
                return;
            }
        }

        if let Some(props) = schema.get("properties").and_then(|p| p.as_object()) {
            // Rule 1: make all listed properties required
            let mut keys: Vec<String> = Vec::new();
            for (key, _value) in props {
                keys.push(key.clone());
            }
            schema.insert("required".to_owned(), json!(keys));
            // Rule 2: force additionalProperties = false on every object
            schema.insert("additionalProperties".to_owned(), json!(false));
            if !self.root_object_processed {
                self.root_object_is_object = true;
                schema.insert("strict".to_owned(), json!(true));
                // Rule 3: root object must also have a non-empty string description
                if let Some(desc) = schema.get("description") {
                    if let Some(desc) = desc.as_str() {
                        if desc.is_empty() {
                            self.properties_without_descriptions
                                .push("Root Object".to_owned());
                        }
                    } else {
                        self.properties_without_descriptions
                            .push("Root Object".to_owned());
                        self.unsupported_types_found
                            .push("RootObject::description not a string".to_owned());
                    }
                } else {
                    self.properties_without_descriptions
                        .push("Root Object".to_owned());
                }
            }
        } else if !self.root_object_processed {
            // Root schema must be an object with properties
            self.root_object_is_object = false;
            self.root_requires_properties_message =
                Some("Root schema must be an object with properties".to_string());
            self.unsupported_types_found.push(
                "Root schema must be an object with properties (no properties found)".to_string(),
            );
            // Still tag the root as strict to make the intent visible to consumers.
            schema.insert("strict".to_owned(), json!(true));
        }

        self.root_object_processed = true;
        // Always traverse subschemas to catch violations even when no properties present.
        transform_subschemas(self, schema);
    }
}

/// If a schema node contains a `$ref`, strip any sibling keywords to comply with
/// OpenAI's restriction that `$ref` cannot be combined with other keywords.
pub fn strip_sibling_keywords_from_ref(schema: &mut Schema) {
    if let Some(obj) = schema.as_object_mut() {
        if let Some(r) = obj.get("$ref").cloned() {
            if obj.len() > 1 {
                obj.clear();
                obj.insert("$ref".to_owned(), r);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ValidationReport {
    pub root_object_processed: bool,
    pub root_object_is_object: bool,
    pub root_requires_properties_message: Option<String>,
    pub properties_without_descriptions: Vec<String>,
    pub unsupported_types_found: Vec<String>,
}

impl ValidationReport {
    pub fn is_valid(&self) -> bool {
        self.root_object_processed
            && self.properties_without_descriptions.is_empty()
            && self.root_requires_properties_message.is_none()
            && self.unsupported_types_found.is_empty()
    }
}

pub fn run_strict_transform(schema: &mut Schema) -> ValidationReport {
    let mut t = StrictStatefulTransform::new();
    t.transform(schema);
    ValidationReport {
        root_object_processed: t.root_object_processed,
        root_object_is_object: t.root_object_is_object,
        root_requires_properties_message: t.root_requires_properties_message,
        properties_without_descriptions: t.properties_without_descriptions,
        unsupported_types_found: t.unsupported_types_found,
    }
}

/// Convenience macro: build a schema, apply strict transform once, and return both.
#[macro_export]
macro_rules! strict_schema_for {
    ($ty:ty) => {{
        let mut schema = schemars::schema_for!($ty);
        let report = $crate::run_strict_transform(&mut schema);
        (schema, report)
    }};
}

/// Generate a strict JSON Schema (OpenAI function-call ready) for a type.
pub fn strict_schema_for_type<T: JsonSchema>() -> (Schema, ValidationReport) {
    let mut schema = schema_for!(T);
    let report = run_strict_transform(&mut schema);
    (schema, report)
}

/// Generate a strict JSON value for a type; pairs with the validation report.
pub fn strict_value_for_type<T: JsonSchema>() -> (Value, ValidationReport) {
    let (schema, report) = strict_schema_for_type::<T>();
    let value =
        serde_json::to_value(&schema).expect("strict schema should always serialize to JSON");
    (value, report)
}

/// Build a simple string-enum schema (no oneOf/const branches) with an optional description.
/// Useful when OpenAI requires the compact `enum: ["a","b"]` form.
pub fn string_enum_schema(variants: &[&str], description: Option<&str>) -> Schema {
    let mut obj = JsonMap::new();
    obj.insert("type".to_string(), json!("string"));
    obj.insert(
        "enum".to_string(),
        Value::Array(
            variants
                .iter()
                .map(|v| Value::String((*v).to_string()))
                .collect(),
        ),
    );
    if let Some(desc) = description {
        obj.insert("description".to_string(), Value::String(desc.to_string()));
    }
    Schema::try_from(Value::Object(obj)).expect("string enum schema must be an object")
}

/// Like `string_enum_schema` but permits explicit `null` as a value; useful for optional enums
/// that are still marked required (OpenAI style).
pub fn string_enum_nullable_schema(variants: &[&str], description: Option<&str>) -> Schema {
    let mut obj = JsonMap::new();
    obj.insert("type".to_string(), json!(["string", "null"]));
    obj.insert(
        "enum".to_string(),
        Value::Array(
            variants
                .iter()
                .map(|v| Value::String((*v).to_string()))
                .collect(),
        ),
    );
    if let Some(desc) = description {
        obj.insert("description".to_string(), Value::String(desc.to_string()));
    }
    Schema::try_from(Value::Object(obj)).expect("nullable string enum schema must be an object")
}

#[cfg(test)]
mod tests {
    use super::*;
    use schemars::generate::SchemaGenerator;
    use serde::{Deserialize, Serialize};

    #[derive(Deserialize, Serialize, JsonSchema)]
    #[schemars(description = "Struct missing property descriptions (should be flagged)")]
    struct NoDescriptions {
        pub a: i32,
    }

    #[derive(Deserialize, Serialize, JsonSchema)]
    #[schemars(description = "Struct with property descriptions (should pass description rule)")]
    struct WithDescriptions {
        #[schemars(description = "an int")]
        pub a: i32,
        #[schemars(description = "a bool")]
        pub b: bool,
    }

    #[derive(Deserialize, Serialize, JsonSchema)]
    #[schemars(description = "Struct used to test anyOf traversal")]
    struct AnyOfNested {
        #[schemars(description = "union field")]
        pub x: String,
    }

    // Custom schema_with to produce disallowed oneOf
    fn one_of_schema(_: &mut SchemaGenerator) -> Schema {
        serde_json::from_value(json!({
            "oneOf": [
                { "type": "string" },
                { "type": "integer" }
            ]
        }))
        .unwrap()
    }

    #[derive(Deserialize, Serialize, JsonSchema)]
    #[schemars(description = "Wrapper that injects oneOf to ensure it is flagged")]
    struct OneOfWrapper {
        #[schemars(schema_with = "one_of_schema")]
        pub value: String,
    }

    // Custom schema_with to produce items as array (disallowed)
    fn items_array_schema(_: &mut SchemaGenerator) -> Schema {
        serde_json::from_value(json!({
            "type": "array",
            "items": [
                { "type": "string" },
                { "type": "integer" }
            ]
        }))
        .unwrap()
    }

    #[derive(Deserialize, Serialize, JsonSchema)]
    #[schemars(description = "Wrapper that injects items as array to ensure it is flagged")]
    struct ItemsArrayWrapper {
        #[schemars(schema_with = "items_array_schema")]
        pub list: Vec<String>,
    }

    // Custom schema_with to omit a type (should be flagged).
    fn missing_type_schema(_: &mut SchemaGenerator) -> Schema {
        serde_json::from_value(json!({
            "description": "missing type schema"
        }))
        .unwrap()
    }

    #[derive(Deserialize, Serialize, JsonSchema)]
    #[schemars(description = "Wrapper that injects a property with no type")]
    struct MissingTypeWrapper {
        #[schemars(schema_with = "missing_type_schema")]
        pub value: String,
    }

    // Nested struct fixtures
    #[derive(Deserialize, Serialize, JsonSchema)]
    #[schemars(description = "outer container with child struct")]
    struct OuterWithChild {
        #[schemars(description = "child struct field")]
        pub child: ChildDescribed,
    }

    #[derive(Deserialize, Serialize, JsonSchema)]
    #[schemars(description = "child struct desc")]
    struct ChildDescribed {
        #[schemars(description = "value desc")]
        pub val: String,
    }

    #[derive(Deserialize, Serialize, JsonSchema)]
    #[schemars(description = "outer container missing child desc")]
    struct OuterWithChildMissing {
        #[schemars(description = "child struct field")]
        pub child: ChildMissing,
    }

    #[derive(Deserialize, Serialize, JsonSchema)]
    #[schemars(description = "child missing property description")]
    struct ChildMissing {
        pub val: String,
    }

    // Enum + nested struct fixtures ($defs/$ref)
    #[derive(Deserialize, Serialize, JsonSchema)]
    #[schemars(description = "enum that wraps structs")]
    #[serde(tag = "type", rename_all = "camelCase")]
    enum WrapperEnum {
        #[schemars(description = "variant A")]
        A(VariantStruct),
        #[schemars(description = "variant B")]
        B {
            #[schemars(description = "b value")]
            value: i32,
        },
    }

    #[derive(Deserialize, Serialize, JsonSchema)]
    #[schemars(description = "variant struct desc")]
    struct VariantStruct {
        #[schemars(description = "variant payload")]
        payload: String,
    }

    // A schema that is purely oneOf at the root (should be rejected)
    fn root_one_of_schema(_: &mut SchemaGenerator) -> Schema {
        serde_json::from_value(json!({
            "oneOf": [
                { "type": "string" },
                { "type": "integer" }
            ]
        }))
        .unwrap()
    }

    #[derive(Deserialize, Serialize, JsonSchema)]
    #[schemars(schema_with = "root_one_of_schema")]
    struct RootOneOfOnly;

    #[test]
    fn strict_macro_sets_required_and_additional_props() {
        let (schema, _) = strict_schema_for!(WithDescriptions);
        let obj = schema.as_object().unwrap();
        assert_eq!(
            obj.get("required").unwrap(),
            &json!(["a", "b"]),
            "all properties should be required"
        );
        assert_eq!(
            obj.get("additionalProperties").unwrap(),
            &json!(false),
            "additionalProperties forced to false"
        );
    }

    #[test]
    fn flags_missing_descriptions() {
        let (_, report) = strict_schema_for!(NoDescriptions);
        assert!(
            !report.properties_without_descriptions.is_empty(),
            "should flag missing descriptions"
        );
    }

    #[test]
    fn allows_anyof_but_traverses() {
        // Manually inject anyOf and ensure traversal still happens without marking unsupported
        let mut schema = schema_for!(AnyOfNested);
        let obj = schema.as_object_mut().unwrap();
        obj.insert(
            "anyOf".to_string(),
            json!([{ "type": "string" }, { "type": "integer" }]),
        );
        let report = run_strict_transform(&mut schema);
        assert!(
            report.unsupported_types_found.is_empty(),
            "anyOf should not be marked unsupported"
        );
        assert!(
            report.properties_without_descriptions.is_empty(),
            "description on x should satisfy traversal"
        );
    }

    #[test]
    fn flags_oneof_usage() {
        let (_, report) = strict_schema_for!(OneOfWrapper);
        assert!(
            report
                .unsupported_types_found
                .iter()
                .any(|m| m.contains("oneOf")),
            "oneOf should be reported unsupported"
        );
    }

    #[test]
    fn flags_items_array_usage() {
        let (_, report) = strict_schema_for!(ItemsArrayWrapper);
        assert!(
            report
                .unsupported_types_found
                .iter()
                .any(|m| m.contains("items as array")),
            "items array form should be reported unsupported"
        );
    }

    #[test]
    fn nested_structs_propagate_descriptions() {
        let (_, report) = strict_schema_for!(OuterWithChild);
        assert!(
            report.properties_without_descriptions.is_empty(),
            "all nested properties should have descriptions"
        );
    }

    #[test]
    fn nested_structs_missing_description_are_flagged() {
        let (_, report) = strict_schema_for!(OuterWithChildMissing);
        assert!(
            report
                .properties_without_descriptions
                .iter()
                .any(|p| p == "val"),
            "missing description in nested child property should be flagged"
        );
    }

    #[test]
    fn enum_variants_and_refs_are_traversed() {
        let (schema, report) = strict_schema_for!(WrapperEnum);
        // Serde tagging introduces synthetic `type` discriminators without descriptions; allow them.
        let missing = &report.properties_without_descriptions;
        assert!(
            !missing.is_empty() && missing.iter().all(|m| m == "type"),
            "only discriminator 'type' fields should lack description; got {:?}",
            missing
        );
        assert!(
            report
                .unsupported_types_found
                .iter()
                .any(|m| m.contains("oneOf")),
            "oneOf must be flagged as unsupported; got {:?}",
            report.unsupported_types_found
        );
        // For sanity, ensure the strict flag was inserted.
        assert!(
            schema
                .as_object()
                .and_then(|o| o.get("strict"))
                .and_then(|v| v.as_bool())
                .unwrap_or(false),
            "strict flag should be present on root schema"
        );
    }

    #[test]
    fn root_must_be_object_and_no_oneof_root() {
        let (_, report) = strict_schema_for!(RootOneOfOnly);
        assert!(
            report
                .unsupported_types_found
                .iter()
                .any(|m| m.contains("Root schema must be an object")),
            "root lacking properties must be flagged"
        );
        assert!(
            report
                .unsupported_types_found
                .iter()
                .any(|m| m.contains("oneOf")),
            "oneOf at root must be flagged"
        );
    }

    #[test]
    fn ref_with_description_is_flagged() {
        let mut obj = serde_json::Map::new();
        obj.insert("$ref".to_string(), json!("#/$defs/Foo"));
        obj.insert("description".to_string(), json!("should not be here"));
        let mut schema = Schema::try_from(Value::Object(obj)).unwrap();
        let report = run_strict_transform(&mut schema);
        assert!(
            report
                .unsupported_types_found
                .iter()
                .any(|m| m.contains("$ref must not have sibling keywords")),
            "expected $ref sibling keywords to be flagged"
        );
    }

    #[test]
    fn missing_property_type_is_reported() {
        let (_schema, report) = strict_schema_for_type::<MissingTypeWrapper>();
        assert!(
            report.unsupported_types_found.iter().any(|err| err.contains("missing type")),
            "expected missing type to be flagged: {:?}",
            report
        );
    }

    #[test]
    #[ignore]
    fn dump_search_pricebook_by_decoder_schema() {
        #[derive(Debug, Serialize, Deserialize, JsonSchema)]
        #[schemars(
            description = "Structured pricebook search using decoded model fields (brand, equipment kind, nominal tons, family) with optional AHRI performance filters (SEER2, HSPF2, capacities, Energy Star/IRA)"
        )]
        struct SearchPricebookByDecoderArgs {
            #[schemars(description = "Brand name as decoded (e.g., 'Trane/AS')")]
            #[serde(default)]
            pub brand: Option<String>,
            #[schemars(description = "Equipment kind")]
            #[serde(default)]
            pub equipment_kind: Option<EquipmentKindParam>,
            #[schemars(description = "Nominal tons", range(min = 0.0))]
            #[serde(default)]
            pub nominal_tons: Option<f64>,
            #[schemars(description = "Minimum nominal tons", range(min = 0.0))]
            #[serde(default)]
            pub min_nominal_tons: Option<f64>,
            #[schemars(description = "Maximum nominal tons", range(min = 0.0))]
            #[serde(default)]
            pub max_nominal_tons: Option<f64>,
            #[schemars(description = "Decoder family or series code prefix")]
            #[serde(default)]
            pub family_code: Option<String>,
            #[schemars(
                description = "Minimum SEER2 (filters via AHRI table)",
                range(min = 0.0)
            )]
            #[serde(default)]
            pub min_seer2: Option<f64>,
            #[schemars(
                description = "Minimum HSPF2 (filters via AHRI table)",
                range(min = 0.0)
            )]
            #[serde(default)]
            pub min_hspf2: Option<f64>,
            #[schemars(
                description = "Minimum cooling capacity (BTUH) from AHRI",
                range(min = 0)
            )]
            #[serde(default)]
            pub min_cooling_btuh: Option<i64>,
            #[schemars(
                description = "Minimum heating @47F capacity (BTUH) from AHRI",
                range(min = 0)
            )]
            #[serde(default)]
            pub min_heating47_btuh: Option<i64>,
            #[schemars(description = "Require Energy Star rated AHRI match")]
            #[serde(default)]
            pub energy_star_only: bool,
            #[schemars(description = "Require Energy Star Cold Climate AHRI match")]
            #[serde(default)]
            pub cold_climate_only: bool,
            #[schemars(description = "Require IRA tax-credit eligible AHRI match")]
            #[serde(default)]
            pub ira_tax_credit_only: bool,
            #[schemars(
                description = "Max rows to return (default 50, max 200)",
                range(min = 1, max = 200)
            )]
            #[serde(default = "default_decoder_limit")]
            pub limit: i64,
            #[schemars(description = "Offset for pagination (default 0)", range(min = 0))]
            #[serde(default = "default_offset")]
            pub offset: i64,
        }

        #[derive(Debug, Serialize, Deserialize, JsonSchema)]
        #[schemars(description = "Equipment kind for decoder search")]
        #[serde(rename_all = "snake_case")]
        enum EquipmentKindParam {
            Outdoor,
            AirHandler,
            Coil,
            Furnace,
            Indoor,
        }

        const fn default_decoder_limit() -> i64 {
            50
        }

        const fn default_offset() -> i64 {
            0
        }

        let (schema, report) = crate::strict_schema_for!(SearchPricebookByDecoderArgs);
        println!(
            "{}",
            serde_json::to_string_pretty(&serde_json::to_value(schema).unwrap()).unwrap()
        );
        println!("report: {:?}", report);
        panic!("Purposeful fail")
    }
}
