# Records

## Contents

- [Record values and record types](#record-values-and-record-types)
- [Field lookups](#field-lookups)
- [Dependent record types](#dependent-record-types)
- [External vs. internal field names](#external-vs-internal-field-names)

## Record values and record types

You can group together multiple values by using records:

```pikelet-repl
Pikelet> record { x = 3.0 : F32; y = 3.0 : F32 }
record { x = 3; y = 3 } : Record { x : F32; y : F32 }
```

Take note of the following:

- record values use the lower case `record` keyword
- record types use the upper case `Record` keyword
- we have to [annotate](#type-annotations) ambiguous field values

We can make a new definition for point types:

```pikelet
Point2d = Record {
  x : F32;
  y : F32;
};
```

You can then use this type to make it easier to define a point record:

```pikelet-repl
Pikelet> record { x = 3.0; y = 3.0 } : Point2d
record { x = 3; y = 3 } : Record { x : F32; y : F32 }
```

Note that we no longer need to annotate each field! Pikelet was able to pick up
the type of each field from the type definition during type checking. You can
read more about Pikelet's type inference on [the type inference page](./type-inference).

## Field lookups

You can access the value associated with a field name by using the dot operator:

```pikelet-repl
Pikelet> record { name = "Jane" }.name
"Jane" : String
```

## Dependent record types

Field types can depend on data from previous fields. Here we turn a
fixed-length array into a dynamically sized array, by using the `len` field
later on to define the `data` field's annotation:

```pikelet
DArray (a : Type) = Record {
    len : I32;
    data : Box (Array len a);
};
```

## External vs. internal field names

Sometimes we'll run into rare cases where a field name might shadow a binding
from a higher scope. In this case we can give the field a new, internal name
using the `as` notation:

```pikelet
Foo = Record {
  -- external name
  --  |
  --  |    internal name
  --  |        |
  --  v        v
  String as String1 : Type;

  -- refers to the built in `String` type
  --     |
  --     v
  x : String;

  -- refers to the local `String` field
  --     |
  --     v
  y : String1;
};
```

We define the following terms:

- _external field name_: the name that we use when projecting on the record
- _internal field name_: the name that we use internally, in dependent fields

Note that most of the time the internal and external field names are the same.
For example:

```pikelet
Point2d = Record {
  x : F32;
  y : F32;
};
```

Is actually desugared to:

```pikelet
Point2d = Record {
  x as x : F32;
  y as y : F32;
};
```
