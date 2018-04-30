# Records

## Contents

- [Record values and record types](#record-values-and-record-types)
- [Field lookups](#field-lookups)
- [Dependent record types](#dependent_record_types)

## Record values and record types

You can group together multiple values by using records:

```pikelet-repl
Pikelet> record { x = 3.0 : F32, y = 3.0 : F32 }
record { x = 3, y = 3 } : Record { x : F32, y : F32 }
```

Take note of the following:

- record values use the lower case `record` keyword
- record types use the upper case `Record` keyword
- we had to [annotate](#type-annotations) ambiguous field values

We can make a new definition for point types:

```pikelet
Point2d = Record {
  x : F32,
  y : F32,
};
```

You can then use this type to make it easier to define a point record:

```pikelet-repl
Pikelet> record { x = 3.0, y = 3.0 } : Point2d
record { x = 3, y = 3 } : Record { x : F32, y : F32 }
```

Note thaw we no longer need to annotate each field! Pikelet was able to pick up
the type of each field from the type definition during type checking. You can
read more about Pikelet's type inference on [the type inference page](./type-inference).

## Field lookups

You can access the value associated with a field by using the dot operator:

```pikelet-repl
Pikelet> record { name = "Jane" }.name
"Jane" : String
```

## Dependent record types

Field types can depend on data from previous fields. Here we turn a
fixed-length array into a dynamically sized array, by using the `len` field
later on to define the `data` field's annotation:

```
DArray (a : Type) = Record {
    len : I32,
    data : Box (Array len a),
}
```
