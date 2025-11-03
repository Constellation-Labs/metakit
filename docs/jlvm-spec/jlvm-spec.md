# JSON Logic VM (JLVM) Specification

## Overview

The JSON Logic Virtual Machine (JLVM) is a Scala-based implementation of JSON Logic with significant extensions for blockchain and distributed systems use cases. It provides a stack-safe, gas-metered execution environment for evaluating logic expressions encoded as JSON.

**Implementation Location**: `src/main/scala/io/constellationnetwork/metagraph_sdk/json_logic`

**Version**: Based on commit 9024552

## Architecture

### Core Components

1. **Expression Layer** (`core/JsonLogicExpression.scala`)
   - AST representation of JSON Logic programs
   - Codec support for JSON serialization/deserialization

2. **Value Layer** (`core/JsonLogicValue.scala`)
   - Runtime value types
   - Type coercion and conversion

3. **Operator Layer** (`core/JsonLogicOp.scala`)
   - Enumeration of all supported operations
   - 87 total operators (vs ~20 in standard JSON Logic)

4. **Runtime Layer** (`runtime/`)
   - Stack-safe tail-recursive evaluator (default)
   - Direct recursive evaluator (optional)
   - Result context abstraction for gas metering

5. **Semantics Layer** (`semantics/`)
   - Operation implementation
   - Variable resolution
   - Gas-aware semantics wrapper

6. **Gas Metering** (`gas/`)
   - Configurable gas costs per operation
   - Depth penalties
   - Gas limit enforcement
   - Operation counting

## Type System

### Expression Types

All expressions in JLVM are instances of `JsonLogicExpression`:

```scala
sealed trait JsonLogicExpression

case class ApplyExpression(op: JsonLogicOp, args: List[JsonLogicExpression])
case class ConstExpression(value: JsonLogicValue)
case class ArrayExpression(value: List[JsonLogicExpression])
case class MapExpression(value: Map[String, JsonLogicExpression])
case class VarExpression(
  value: Either[String, JsonLogicExpression],
  default: Option[JsonLogicValue] = None
)
```

**Key Differences from Standard JSON Logic**:
- `MapExpression`: Allows dynamic object construction with expression values
- `VarExpression`: Supports expression-based variable paths (e.g., `{"var": {"var": "keyName"}}`)
- `FunctionValue`: Special value type for lazy evaluation callbacks

### Value Types

Runtime values are instances of `JsonLogicValue`:

```scala
sealed trait JsonLogicValue { val tag: String }

// Primitives
case object NullValue                           // tag: "null"
case class BoolValue(value: Boolean)            // tag: "bool"
case class IntValue(value: BigInt)              // tag: "int"
case class FloatValue(value: BigDecimal)        // tag: "float"
case class StrValue(value: String)              // tag: "string"

// Collections
case class ArrayValue(value: List[JsonLogicValue])        // tag: "array"
case class MapValue(value: Map[String, JsonLogicValue])   // tag: "map"

// Special
case class FunctionValue(expr: JsonLogicExpression)       // tag: "function"
```

**Key Differences**:
- Uses `BigInt` and `BigDecimal` for arbitrary-precision arithmetic
- Separate `IntValue` and `FloatValue` types (vs single number type)
- `MapValue` for objects (standard JSON Logic has limited object support)
- `FunctionValue` for lazy-evaluated expressions in callbacks

### Truthiness

Values evaluate to truthy/falsy following these rules:

| Type | Truthy When |
|------|-------------|
| `NullValue` | Never (always falsy) |
| `BoolValue(v)` | `v == true` |
| `IntValue(i)` | `i != 0` |
| `FloatValue(d)` | `d != 0.0` |
| `StrValue(s)` | `s.nonEmpty` |
| `ArrayValue(v)` | `v.nonEmpty` |
| `MapValue(v)` | `v.nonEmpty` |
| `FunctionValue(_)` | Never (always falsy) |

## Writing JSON Logic Payloads

This section provides comprehensive guidance on constructing correct JSON Logic payloads, from simple expressions to complex nested structures.

### Basic Payload Structure

A JSON Logic payload consists of two parts:
1. **Expression (Rule)**: The logic to evaluate
2. **Data**: The context against which the rule is evaluated

**Basic Example**:
```json
// Expression
{
  "==": [
    {"var": "temperature"},
    "hot"
  ]
}

// Data
{
  "temperature": "hot",
  "humidity": 85
}

// Result: true
```

### Nested Data Structures

#### Working with Nested Objects

**Data Structure**:
```json
{
  "user": {
    "profile": {
      "name": "Alice",
      "age": 30,
      "address": {
        "city": "San Francisco",
        "state": "CA",
        "zip": "94102"
      }
    },
    "preferences": {
      "theme": "dark",
      "notifications": true
    }
  },
  "account": {
    "type": "premium",
    "balance": 1250.50
  }
}
```

**Accessing Nested Fields**:
```json
// Get city
{"var": "user.profile.address.city"}
// => "San Francisco"

// Complex condition on nested data
{
  "and": [
    {"==": [{"var": "user.profile.address.state"}, "CA"]},
    {">": [{"var": "account.balance"}, 1000]},
    {"==": [{"var": "account.type"}, "premium"]}
  ]
}
// => true
```

#### Working with Arrays of Objects

**Data Structure**:
```json
{
  "orders": [
    {
      "id": "ORD-001",
      "customer": "Alice",
      "items": [
        {"name": "Widget", "price": 29.99, "quantity": 2},
        {"name": "Gadget", "price": 49.99, "quantity": 1}
      ],
      "status": "shipped",
      "total": 109.97
    },
    {
      "id": "ORD-002",
      "customer": "Bob",
      "items": [
        {"name": "Gizmo", "price": 19.99, "quantity": 3}
      ],
      "status": "pending",
      "total": 59.97
    },
    {
      "id": "ORD-003",
      "customer": "Charlie",
      "items": [
        {"name": "Widget", "price": 29.99, "quantity": 1},
        {"name": "Doohickey", "price": 39.99, "quantity": 2}
      ],
      "status": "delivered",
      "total": 109.97
    }
  ],
  "minimumOrderValue": 100
}
```

**Example 1: Filter orders by status**:
```json
{
  "filter": [
    {"var": "orders"},
    {"==": [{"var": "status"}, "shipped"]}
  ]
}
// => [{"id": "ORD-001", "customer": "Alice", ...}]
```

**Example 2: Get all customer names from high-value orders**:
```json
{
  "map": [
    {
      "filter": [
        {"var": "orders"},
        {">=": [{"var": "total"}, {"var": "minimumOrderValue"}]}
      ]
    },
    {"var": "customer"}
  ]
}
// => ["Alice", "Charlie"]
```

**Example 3: Calculate total revenue from shipped orders**:
```json
{
  "reduce": [
    {
      "filter": [
        {"var": "orders"},
        {"==": [{"var": "status"}, "shipped"]}
      ]
    },
    {"+": [{"var": "accumulator"}, {"var": "current.total"}]},
    0
  ]
}
// => 109.97
```

**Example 4: Nested array processing - count total items across all orders**:
```json
{
  "reduce": [
    {
      "map": [
        {"var": "orders"},
        {
          "reduce": [
            {"var": "items"},
            {"+": [{"var": "accumulator"}, {"var": "current.quantity"}]},
            0
          ]
        }
      ]
    },
    {"+": [{"var": "accumulator"}, {"var": "current"}]},
    0
  ]
}
// => 9 (2+1+3+1+2)
```

### Complex Variable Expressions

#### Dynamic Variable Access

**Use Case**: Access a field whose name is stored in another field.

**Data**:
```json
{
  "fieldToCheck": "temperature",
  "temperature": 72,
  "humidity": 65,
  "pressure": 1013
}
```

**Expression**:
```json
{
  "var": {
    "var": "fieldToCheck"
  }
}
// => 72
```

**Multi-level Dynamic Access**:
```json
// Data
{
  "sectionName": "user",
  "fieldName": "age",
  "user": {
    "age": 30,
    "name": "Alice"
  },
  "admin": {
    "age": 45,
    "name": "Bob"
  }
}

// Expression: Get user.age dynamically
{
  "var": {
    "cat": [
      {"var": "sectionName"},
      ".",
      {"var": "fieldName"}
    ]
  }
}
// => 30
```

#### Conditional Variable Access

**Data**:
```json
{
  "userType": "premium",
  "premiumLimit": 1000,
  "standardLimit": 100,
  "usage": 500
}
```

**Expression**: Check against appropriate limit based on user type:
```json
{
  "<": [
    {"var": "usage"},
    {
      "if": [
        {"==": [{"var": "userType"}, "premium"]},
        {"var": "premiumLimit"},
        {"var": "standardLimit"}
      ]
    }
  ]
}
// => true (500 < 1000)
```

#### Array Index from Variable

**Data**:
```json
{
  "selectedIndex": 2,
  "items": ["apple", "banana", "cherry", "date"]
}
```

**Expression**:
```json
{
  "var": {
    "cat": [
      "items.",
      {"var": "selectedIndex"}
    ]
  }
}
// => "cherry"
```

### Deep Nesting Patterns

#### Multi-level Object Traversal

**Data**:
```json
{
  "company": {
    "departments": {
      "engineering": {
        "teams": {
          "backend": {
            "members": [
              {
                "name": "Alice",
                "skills": ["Scala", "Cats", "Blockchain"],
                "level": "senior"
              },
              {
                "name": "Bob",
                "skills": ["Python", "Django"],
                "level": "junior"
              }
            ]
          },
          "frontend": {
            "members": [
              {
                "name": "Charlie",
                "skills": ["React", "TypeScript"],
                "level": "mid"
              }
            ]
          }
        }
      }
    }
  }
}
```

**Expression**: Find all senior engineers with Scala skills:
```json
{
  "filter": [
    {"var": "company.departments.engineering.teams.backend.members"},
    {
      "and": [
        {"==": [{"var": "level"}, "senior"]},
        {"in": ["Scala", {"var": "skills"}]}
      ]
    }
  ]
}
// => [{"name": "Alice", "skills": ["Scala", "Cats", "Blockchain"], "level": "senior"}]
```

#### Deeply Nested Calculations

**Data**:
```json
{
  "invoice": {
    "lineItems": [
      {
        "product": "Widget",
        "pricing": {
          "base": 100,
          "discounts": [
            {"type": "volume", "amount": 10},
            {"type": "seasonal", "amount": 5}
          ],
          "tax": {
            "rate": 0.08,
            "included": false
          }
        },
        "quantity": 3
      },
      {
        "product": "Gadget",
        "pricing": {
          "base": 50,
          "discounts": [
            {"type": "loyalty", "amount": 7}
          ],
          "tax": {
            "rate": 0.08,
            "included": false
          }
        },
        "quantity": 2
      }
    ]
  }
}
```

**Expression**: Calculate total invoice with discounts and tax:
```json
{
  "reduce": [
    {
      "map": [
        {"var": "invoice.lineItems"},
        {
          "*": [
            {"var": "quantity"},
            {
              "*": [
                {
                  "-": [
                    {"var": "pricing.base"},
                    {
                      "reduce": [
                        {"var": "pricing.discounts"},
                        {"+": [{"var": "accumulator"}, {"var": "current.amount"}]},
                        0
                      ]
                    }
                  ]
                },
                {"+": [1, {"var": "pricing.tax.rate"}]}
              ]
            }
          ]
        }
      ]
    },
    {"+": [{"var": "accumulator"}, {"var": "current"}]},
    0
  ]
}
// => 366.12
// Calculation:
// Widget: (100 - 10 - 5) * 1.08 * 3 = 275.4
// Gadget: (50 - 7) * 1.08 * 2 = 92.88
// Total: 368.28
```

### Real-World Use Cases

#### Example 1: E-commerce Order Validation

**Data**:
```json
{
  "order": {
    "items": [
      {"sku": "WIDGET-001", "quantity": 2, "price": 29.99},
      {"sku": "GADGET-001", "quantity": 1, "price": 49.99}
    ],
    "customer": {
      "id": "CUST-12345",
      "tier": "gold",
      "region": "US"
    },
    "shipping": {
      "method": "express",
      "address": {
        "country": "US",
        "state": "CA"
      }
    }
  },
  "config": {
    "minOrderValue": 50,
    "freeShippingThreshold": 100,
    "allowedRegions": ["US", "CA", "EU"]
  }
}
```

**Expression**: Validate order eligibility:
```json
{
  "and": [
    // Check minimum order value
    {
      ">=": [
        {
          "reduce": [
            {"var": "order.items"},
            {
              "+": [
                {"var": "accumulator"},
                {"*": [{"var": "current.quantity"}, {"var": "current.price"}]}
              ]
            },
            0
          ]
        },
        {"var": "config.minOrderValue"}
      ]
    },
    // Check region allowed
    {"in": [{"var": "order.shipping.address.country"}, {"var": "config.allowedRegions"}]},
    // Check items in stock (all quantities > 0)
    {
      "all": [
        {"var": "order.items"},
        {">": [{"var": "quantity"}, 0]}
      ]
    }
  ]
}
// => true
```

#### Example 2: User Permissions Check

**Data**:
```json
{
  "user": {
    "id": "user-123",
    "roles": ["editor", "reviewer"],
    "permissions": {
      "documents": ["read", "write", "comment"],
      "settings": ["read"]
    },
    "department": "engineering"
  },
  "resource": {
    "type": "document",
    "id": "doc-456",
    "owner": "user-789",
    "department": "engineering",
    "visibility": "department"
  },
  "requestedAction": "write"
}
```

**Expression**: Check if user can perform action:
```json
{
  "or": [
    // User is owner
    {"==": [{"var": "user.id"}, {"var": "resource.owner"}]},
    // User has explicit permission
    {
      "and": [
        {
          "in": [
            {"var": "requestedAction"},
            {
              "var": {
                "cat": [
                  "user.permissions.",
                  {"var": "resource.type"}
                ]
              }
            }
          ]
        },
        // AND resource is visible to user's department
        {
          "or": [
            {"==": [{"var": "resource.visibility"}, "public"]},
            {
              "and": [
                {"==": [{"var": "resource.visibility"}, "department"]},
                {"==": [{"var": "user.department"}, {"var": "resource.department"}]}
              ]
            }
          ]
        }
      ]
    }
  ]
}
// => true
```

#### Example 3: Time-based Pricing Rules

**Data**:
```json
{
  "booking": {
    "checkIn": "2024-07-15",
    "checkOut": "2024-07-20",
    "roomType": "deluxe",
    "guests": 2
  },
  "pricing": {
    "baseRates": {
      "standard": 100,
      "deluxe": 150,
      "suite": 250
    },
    "seasonalMultipliers": [
      {
        "name": "summer",
        "startDate": "2024-06-01",
        "endDate": "2024-08-31",
        "multiplier": 1.5
      },
      {
        "name": "winter",
        "startDate": "2024-12-01",
        "endDate": "2024-02-28",
        "multiplier": 1.2
      }
    ],
    "guestSurcharge": {
      "perGuest": 25,
      "freeGuests": 2
    }
  }
}
```

**Expression**: Calculate total price:
```json
{
  "*": [
    // Base rate
    {
      "var": {
        "cat": [
          "pricing.baseRates.",
          {"var": "booking.roomType"}
        ]
      }
    },
    // Seasonal multiplier
    {
      "if": [
        {
          "some": [
            {"var": "pricing.seasonalMultipliers"},
            {
              "and": [
                {"<=": [{"var": "startDate"}, {"var": "booking.checkIn"}]},
                {">=": [{"var": "endDate"}, {"var": "booking.checkOut"}]}
              ]
            }
          ]
        },
        {
          "var": {
            "cat": [
              "pricing.seasonalMultipliers.",
              {
                "var": {
                  "cat": [
                    "pricing.seasonalMultipliers.",
                    {
                      "find": [
                        {"var": "pricing.seasonalMultipliers"},
                        {
                          "and": [
                            {"<=": [{"var": "startDate"}, {"var": "booking.checkIn"}]},
                            {">=": [{"var": "endDate"}, {"var": "booking.checkOut"}]}
                          ]
                        }
                      ]
                    },
                    ".multiplier"
                  ]
                }
              }
            ]
          }
        },
        1
      ]
    },
    // Number of nights (simplified - actual date calc would need custom operator)
    5
  ]
}
```

### Common Patterns

#### Pattern 1: Null-Safe Navigation

Always check for null before accessing nested properties:

```json
{
  "if": [
    {"!==": [{"var": "user"}, null]},
    {
      "if": [
        {"!==": [{"var": "user.profile"}, null]},
        {"var": "user.profile.email"},
        "no-email@example.com"
      ]
    },
    "no-user@example.com"
  ]
}
```

Or use default values:
```json
{"var": ["user.profile.email", "default@example.com"]}
```

#### Pattern 2: Array Existence Check

```json
{
  "and": [
    {"!==": [{"var": "items"}, null]},
    {">": [{"length": [{"var": "items"}]}, 0]}
  ]
}
```

Or using truthiness:
```json
{"!!": [{"var": "items"}]}
```

#### Pattern 3: Multiple Field Validation

```json
{
  "if": [
    {"!==": [{"missing": ["name", "email", "password"]}, []]},
    {
      "cat": [
        "Missing required fields: ",
        {"join": [{"missing": ["name", "email", "password"]}, ", "]}
      ]
    },
    "valid"
  ]
}
```

#### Pattern 4: Conditional Aggregation

Sum only items that meet a condition:
```json
{
  "reduce": [
    {
      "filter": [
        {"var": "transactions"},
        {"==": [{"var": "type"}, "debit"]}
      ]
    },
    {"+": [{"var": "accumulator"}, {"var": "current.amount"}]},
    0
  ]
}
```

#### Pattern 5: Nested Object Construction

Build new objects from existing data:
```json
{
  "map": [
    {"var": "users"},
    {
      "name": {"var": "fullName"},
      "contact": {"var": "email"},
      "age": {"var": "profile.age"}
    }
  ]
}
```

Note: This uses `MapExpression` which is a JLVM extension.

### Anti-Patterns to Avoid

#### Anti-Pattern 1: Accessing Array by Wrong Type

**Bad**:
```json
{"var": "items.first"}  // Wrong - array indices must be numeric
```

**Good**:
```json
{"var": "items.0"}      // Correct
```

#### Anti-Pattern 2: Missing Null Checks

**Bad**:
```json
{">": [{"var": "user.age"}, 18]}
// Fails if user is null
```

**Good**:
```json
{
  "and": [
    {"!==": [{"var": "user"}, null]},
    {">": [{"var": "user.age"}, 18]}
  ]
}
```

#### Anti-Pattern 3: Type Confusion with Equality

**Bad**:
```json
{"==": [{"var": "count"}, "5"]}  // May give unexpected results with coercion
```

**Good**:
```json
{"===": [{"var": "count"}, 5]}   // Strict equality, no coercion
```

#### Anti-Pattern 4: Deeply Nested Without Intermediate Variables

**Bad** (hard to read and debug):
```json
{
  "+": [
    {"var": "a.b.c.d.e.f"},
    {"*": [{"var": "x.y.z.w.v"}, {"-": [{"var": "p.q.r"}, {"var": "m.n"}]}]}
  ]
}
```

**Good** (break into logical steps):
```json
{
  "if": [
    true,  // Use as let-binding pattern
    {
      "+": [
        {"var": "a.b.c.d.e.f"},
        {
          "*": [
            {"var": "x.y.z.w.v"},
            {"-": [{"var": "p.q.r"}, {"var": "m.n"}]}
          ]
        }
      ]
    }
  ]
}
```

Or restructure data to be flatter.

#### Anti-Pattern 5: Overusing `reduce` for Simple Cases

**Bad**:
```json
{
  "reduce": [
    {"var": "numbers"},
    {"+": [{"var": "accumulator"}, {"var": "current"}]},
    0
  ]
}
```

**Good** (when just summing):
```json
{"+": [{"var": "numbers"}]}  // Many operations handle arrays directly
```

### Tips for Writing Maintainable Rules

1. **Keep Nesting Shallow**: Limit to 3-4 levels when possible
2. **Use Meaningful Data Structure**: Flatten deeply nested data when feasible
3. **Validate Inputs**: Always check for null/undefined before accessing
4. **Use Default Values**: Leverage `{"var": ["path", "default"]}` pattern
5. **Comment Complex Logic**: While JSON doesn't support comments, document rules externally
6. **Test Incrementally**: Build complex rules step-by-step, testing each addition
7. **Consider Gas Costs**: Deeply nested operations consume more gas
8. **Use Strict Equality**: Prefer `===` over `==` when type is known
9. **Avoid Magic Numbers**: Store thresholds in data context, not hard-coded in rules
10. **Leverage Type System**: Use appropriate types (int vs float) for clarity

### Debugging Complex Payloads

When a complex rule doesn't work as expected:

1. **Test Subexpressions**: Evaluate parts independently
2. **Check Data Structure**: Verify data matches expected shape
3. **Verify Variable Paths**: Ensure dot-notation paths are correct
4. **Inspect Intermediate Results**: Use simpler operations to see intermediate values
5. **Check Truthiness**: Remember empty arrays/strings are falsy
6. **Validate Types**: Ensure types match for strict operations

**Example Debugging Approach**:
```json
// Original (not working)
{
  "filter": [
    {"var": "orders"},
    {">": [{"var": "items.0.price"}, 100]}
  ]
}

// Debug: First check if items exist
{"var": "orders.0.items"}

// Debug: Check if items is an array
{"typeof": [{"var": "orders.0.items"}]}

// Debug: Get first item
{"var": "orders.0.items.0"}

// Debug: Get price
{"var": "orders.0.items.0.price"}

// Now we know the correct path structure
```

## Data Access

### Variable Access: `var`

Retrieves values from the data context.

**Syntax**:
```json
{"var": "path"}
{"var": ["path", defaultValue]}
{"var": {"var": "dynamicKey"}}
```

**Features**:
- Dot-notation path traversal: `"user.profile.age"`
- Array index access: `"items.0.name"`
- Default values when path not found
- Dynamic expression-based paths
- Empty string `""` returns entire data context
- Paths ending with `.` return `null`

**Examples**:
```json
// Basic access
{"var": "name"}                    // data: {"name": "Alice"} => "Alice"

// Nested access
{"var": "user.age"}               // data: {"user": {"age": 25}} => 25

// Array access
{"var": "items.0"}                // data: {"items": [1,2,3]} => 1

// Default value
{"var": ["missing", "default"]}   // data: {} => "default"

// Dynamic key
{"var": {"var": "keyName"}}       // data: {"keyName": "age", "age": 30} => 30

// Root access
{"var": ""}                       // data: {"a": 1} => {"a": 1}
```

**Variable Scoping**:
In array operations (`map`, `filter`, `reduce`, etc.), `var` accesses the current element:
- Within `map`/`filter`/`all`/`some`/`none`/`find`: current element
- Within `reduce`: access via `{"var": "current"}` and `{"var": "accumulator"}`

**Context Merging**:
When a local context is provided (e.g., in array callbacks), it merges with the global data:
- Primitives in context: use global data
- Collections: merge collection data (context takes precedence)

## Control Flow

### Conditional: `if`

**Syntax**:
```json
{"if": [condition, thenBranch, elseBranch]}
{"if": [cond1, then1, cond2, then2, ..., defaultElse]}
```

**Key Feature**: **Lazy Evaluation** - branches are NOT evaluated unless selected.

**Differences from Standard JSON Logic**:
- Standard JSON Logic eagerly evaluates all branches
- JLVM only evaluates the selected branch
- Branches can contain expensive operations without penalty

**Examples**:
```json
// Simple if-else
{
  "if": [
    {">": [{"var": "age"}, 18]},
    "adult",
    "minor"
  ]
}

// Multi-condition (if-elseif-else)
{
  "if": [
    {">": [{"var": "score"}, 90]}, "A",
    {">": [{"var": "score"}, 80]}, "B",
    {">": [{"var": "score"}, 70]}, "C",
    "F"
  ]
}
```

**Implementation Note**:
Lazy evaluation is achieved by wrapping branches in `FunctionValue` and only evaluating the selected branch during execution.

### Default Value Selection: `default`

Returns the first truthy value from arguments.

**Syntax**:
```json
{"default": [value1, value2, ..., fallback]}
```

**Example**:
```json
{"default": [{"var": "email"}, {"var": "phone"}, "no-contact"]}
// Returns first non-null, non-empty value
```

**Extension**: Not present in standard JSON Logic.

## Logical Operators

### NOT: `!`

Negates truthiness of value.

```json
{"!": [value]}
```

### Double NOT: `!!`

Coerces to boolean (returns truthiness as boolean).

```json
{"!!": [value]}
```

### OR: `or`

Returns first truthy value, or last value if all falsy.

```json
{"or": [value1, value2, ...]}
```

**Example**:
```json
{"or": [false, 0, "hello", "world"]}  // => "hello"
{"or": [false, null, 0]}              // => 0
```

### AND: `and`

Returns first falsy value, or last value if all truthy.

```json
{"and": [value1, value2, ...]}
```

**Example**:
```json
{"and": [true, 1, "hello"]}    // => "hello"
{"and": [true, 0, "hello"]}    // => 0
```

## Comparison Operators

### Equality

**Loose Equality**: `==` (with type coercion)
```json
{"==": [left, right]}
```

**Strict Equality**: `===` (no coercion)
```json
{"===": [left, right]}
```

**Coercion Rules** (for `==`):
1. Empty string coerces to 0
2. Single-element arrays coerce to their element
3. Empty arrays coerce to 0
4. Single-key objects coerce to their value
5. Empty objects coerce to 0
6. Booleans: `true` = 1, `false` = 0
7. String-to-number parsing attempted

**Strict Equality** (`===`):
- Types must match exactly
- No coercion performed
- Arrays and objects compared by value equality

**Examples**:
```json
{"==": ["1", 1]}        // => true (string coerced to int)
{"===": ["1", 1]}       // => false (different types)
{"==": [[], 0]}         // => true (empty array coerces to 0)
{"===": [[], 0]}        // => false (different types)
```

### Inequality

**Loose**: `!=`
**Strict**: `!==`

Same coercion rules as equality operators.

### Relational

**Less Than**: `<`
```json
{"<": [left, right]}
{"<": [a, b, c]}  // Chained: a < b < c
```

**Less Than or Equal**: `<=`
```json
{"<=": [left, right]}
{"<=": [a, b, c]}  // Chained: a <= b <= c
```

**Greater Than**: `>`
```json
{">": [left, right]}
```

**Greater Than or Equal**: `>=`
```json
{">=": [left, right]}
```

**Chained Comparisons**:
```json
{"<": [18, {"var": "age"}, 65]}  // 18 < age < 65
{"<=": [0, {"var": "score"}, 100]}  // 0 <= score <= 100
```

**Extension**: Three-argument chained comparisons for `<` and `<=`.

## Arithmetic Operators

All arithmetic operations use arbitrary-precision `BigInt` and `BigDecimal`.

### Basic Operations

**Addition**: `+`
```json
{"+": [1, 2, 3]}              // => 6
{"+": [{"var": "price"}, {"var": "tax"}]}
```

**Subtraction**: `-`
```json
{"-": [10, 3]}                // => 7
{"-": [5]}                    // => -5 (negation)
```

**Multiplication**: `*`
```json
{"*": [2, 3, 4]}              // => 24
```

**Division**: `/`
```json
{"/": [10, 2]}                // => 5
{"/": [10, 3]}                // => 3 (integer division)
{"/": [10.0, 3]}              // => 3.333... (float division)
```

**Modulo**: `%`
```json
{"%": [10, 3]}                // => 1
```

**Division by Zero**: Returns error (not infinity/NaN).

### Aggregate Operations

**Maximum**: `max`
```json
{"max": [1, 5, 3]}            // => 5
{"max": [{"var": "values"}]}  // max of array
```

**Minimum**: `min`
```json
{"min": [1, 5, 3]}            // => 1
```

### Extended Math Operations

**Absolute Value**: `abs`
```json
{"abs": [-5]}                 // => 5
```

**Rounding**: `round`
```json
{"round": [3.7]}              // => 4 (half-up rounding)
```

**Floor**: `floor`
```json
{"floor": [3.7]}              // => 3
```

**Ceiling**: `ceil`
```json
{"ceil": [3.2]}               // => 4
```

**Power**: `pow`
```json
{"pow": [2, 10]}              // => 1024
{"pow": [2.5, 3]}             // => 15.625
```

**Power Limits**:
- Exponent limited to ±999 for safety
- Returns error for infinity/NaN results

**Extensions**: `abs`, `round`, `floor`, `ceil`, `pow` are not in standard JSON Logic.

### Type Preservation

Integer operations return `IntValue` when result is whole number:
```json
{"+": [1, 2]}           // => IntValue(3)
{"/": [10, 2]}          // => IntValue(5)
{"/": [10, 3]}          // => IntValue(3)
```

Mixed or non-whole results return `FloatValue`:
```json
{"+": [1.5, 2]}         // => FloatValue(3.5)
{"/": [10.0, 3]}        // => FloatValue(3.333...)
```

## Array Operations

### Transformation: `map`

Applies expression to each element.

**Syntax**:
```json
{"map": [array, expression]}
```

**Example**:
```json
{
  "map": [
    {"var": "items"},
    {"*": [{"var": "price"}, {"var": "quantity"}]}
  ]
}
```

**Data**:
```json
{
  "items": [
    {"price": 10, "quantity": 2},
    {"price": 15, "quantity": 3}
  ]
}
```

**Result**: `[20, 45]`

### Filtering: `filter`

Returns elements where expression is truthy.

**Syntax**:
```json
{"filter": [array, expression]}
```

**Example**:
```json
{
  "filter": [
    {"var": "users"},
    {">": [{"var": "age"}, 18]}
  ]
}
```

### Reduction: `reduce`

Reduces array to single value.

**Syntax**:
```json
{"reduce": [array, expression, initialValue]}
{"reduce": [array, expression]}  // uses first element as initial
```

**Special Variables**:
- `{"var": "current"}`: current element
- `{"var": "accumulator"}`: accumulated value

**Example (sum)**:
```json
{
  "reduce": [
    [1, 2, 3, 4],
    {"+": [{"var": "accumulator"}, {"var": "current"}]},
    0
  ]
}
// => 10
```

### Merging: `merge`

Combines arrays or objects.

**For Arrays**:
```json
{"merge": [[1, 2], [3, 4]]}        // => [1, 2, 3, 4]
{"merge": [1, [2, 3], 4]}          // => [1, 2, 3, 4]
```

**For Objects**:
```json
{"merge": [{"a": 1}, {"b": 2}]}    // => {"a": 1, "b": 2}
```

### Testing: `all`, `some`, `none`

**All Elements Match**: `all`
```json
{"all": [array, expression]}
```

**Some Elements Match**: `some`
```json
{"some": [array, expression]}
{"some": [array, expression, minCount]}
```

**No Elements Match**: `none`
```json
{"none": [array, expression]}
```

**Examples**:
```json
{"all": [[2, 4, 6], {"%": [{"var": ""}, 2]}]}  // false (checks if all even)
{"some": [[1, 3, 4], {"%": [{"var": ""}, 2]}]} // true (at least one even)
```

### Finding: `find`

Returns first element where expression is truthy, or `null`.

**Syntax**:
```json
{"find": [array, expression]}
```

**Example**:
```json
{
  "find": [
    {"var": "users"},
    {"==": [{"var": "id"}, 42]}
  ]
}
```

**Extension**: Not in standard JSON Logic.

### Counting: `count`

**Count Elements**: `count`
```json
{"count": [array]}              // => array length
{"count": [array, expression]}  // => count of matching elements
```

**Example**:
```json
{"count": [[1, 2, 3, 4], {">": [{"var": ""}, 2]}]}  // => 2
```

**Extension**: Predicate form not in standard JSON Logic.

### Membership: `in`

Check if value is in array or substring is in string.

**Syntax**:
```json
{"in": [value, array]}
{"in": [substring, string]}
```

**Examples**:
```json
{"in": [3, [1, 2, 3, 4]]}        // => true
{"in": ["@", "user@example.com"]} // => true
```

### Intersection: `intersect`

Check if all elements of first array are in second array.

**Syntax**:
```json
{"intersect": [needles, haystack]}
```

**Example**:
```json
{"intersect": [[1, 2], [1, 2, 3, 4]]}  // => true
{"intersect": [[1, 5], [1, 2, 3, 4]]}  // => false
```

**Extension**: Not in standard JSON Logic.

### Utility Operations

**Unique Elements**: `unique`
```json
{"unique": [[1, 2, 2, 3, 1]]}    // => [1, 2, 3]
```

**Slice**: `slice`
```json
{"slice": [array, start]}
{"slice": [array, start, end]}
```

Negative indices count from end.

**Reverse**: `reverse`
```json
{"reverse": [[1, 2, 3]]}         // => [3, 2, 1]
```

**Flatten**: `flatten`
```json
{"flatten": [[1, [2, 3], [4, [5]]]]}  // => [1, 2, 3, 4, [5]]
```

One level of flattening only.

**Extensions**: `unique`, `slice`, `reverse`, `flatten` not in standard JSON Logic.

## String Operations

### Concatenation: `cat`

Concatenates values to string.

**Syntax**:
```json
{"cat": [value1, value2, ...]}
```

**Example**:
```json
{"cat": ["Hello, ", {"var": "name"}, "!"]}
// data: {"name": "Alice"} => "Hello, Alice!"
```

**Type Handling**:
- `null` => `""`
- Booleans, numbers => `.toString()`
- Collections => error

### Substring: `substr`

Extract substring.

**Syntax**:
```json
{"substr": [string, start]}
{"substr": [string, start, length]}
```

**Features**:
- Negative start: count from end
- Negative length: count from end
- Out-of-bounds: clamped to valid range

**Examples**:
```json
{"substr": ["Hello", 1, 3]}     // => "ell"
{"substr": ["Hello", -3]}       // => "llo"
{"substr": ["Hello", 1, -1]}    // => "ell"
```

### Case Transformation

**Lowercase**: `lower`
```json
{"lower": ["HELLO"]}             // => "hello"
```

**Uppercase**: `upper`
```json
{"upper": ["hello"]}             // => "HELLO"
```

**Extensions**: Not in standard JSON Logic.

### Join and Split

**Join**: `join`
```json
{"join": [array, separator]}
```

**Example**:
```json
{"join": [["a", "b", "c"], "-"]}  // => "a-b-c"
```

**Split**: `split`
```json
{"split": [string, separator]}
```

**Example**:
```json
{"split": ["a-b-c", "-"]}        // => ["a", "b", "c"]
```

**Extensions**: Not in standard JSON Logic.

### String Testing

**Trim Whitespace**: `trim`
```json
{"trim": ["  hello  "]}          // => "hello"
```

**Starts With**: `startsWith`
```json
{"startsWith": ["hello", "hel"]} // => true
```

**Ends With**: `endsWith`
```json
{"endsWith": ["hello", "lo"]}    // => true
```

**Extensions**: `trim`, `startsWith`, `endsWith` not in standard JSON Logic.

## Object/Map Operations

### Extract Values: `values`

Get all values from object.

**Syntax**:
```json
{"values": [object]}
```

**Example**:
```json
{"values": [{"a": 1, "b": 2}]}   // => [1, 2]
```

### Extract Keys: `keys`

Get all keys from object.

**Syntax**:
```json
{"keys": [object]}
```

**Example**:
```json
{"keys": [{"a": 1, "b": 2}]}     // => ["a", "b"]
```

### Get Value: `get`

Get value by key (errors if not found).

**Syntax**:
```json
{"get": [object, key]}
```

**Example**:
```json
{"get": [{"a": 1, "b": 2}, "a"]} // => 1
```

**Note**: Throws `JsonLogicException` if key not found (unlike `var` which returns `null`).

### Has Key: `has`

Check if object has key.

**Syntax**:
```json
{"has": [object, key]}
```

**Example**:
```json
{"has": [{"a": 1}, "a"]}         // => true
{"has": [{"a": 1}, "b"]}         // => false
```

### Entries: `entries`

Convert object to array of `[key, value]` pairs.

**Syntax**:
```json
{"entries": [object]}
```

**Example**:
```json
{"entries": [{"a": 1, "b": 2}]}  // => [["a", 1], ["b", 2]]
```

**Extensions**: All object operations are extensions not in standard JSON Logic.

## Utility Operations

### Length: `length`

Get length of string or array.

**Syntax**:
```json
{"length": [value]}
```

**Examples**:
```json
{"length": ["hello"]}            // => 5
{"length": [[1, 2, 3]]}          // => 3
```

### Type Checking: `typeof`

Get type tag of value.

**Syntax**:
```json
{"typeof": [value]}
```

**Returns**: One of `"null"`, `"bool"`, `"int"`, `"float"`, `"string"`, `"array"`, `"map"`, `"function"`

**Example**:
```json
{"typeof": [42]}                 // => "int"
{"typeof": [42.5]}               // => "float"
{"typeof": [[1, 2]]}             // => "array"
```

**Extension**: Not in standard JSON Logic.

### Missing Fields: `missing`

Returns array of field names that are missing (null/undefined) from data.

**Syntax**:
```json
{"missing": [field1, field2, ...]}
```

**Example**:
```json
{"missing": ["name", "email", "age"]}
// data: {"name": "Alice", "email": "a@example.com"}
// => ["age"]
```

### Missing Some: `missing_some`

Checks if at least N of the specified fields are present.

**Syntax**:
```json
{"missing_some": [minRequired, [field1, field2, ...]]}
```

**Returns**:
- Empty array if at least `minRequired` fields present
- Array of missing fields otherwise

**Example**:
```json
{"missing_some": [2, ["email", "phone", "address"]]}
// data: {"email": "a@example.com", "phone": "555-1234"}
// => [] (2 present, requirement met)
```

### Exists: `exists`

Check if values are not null.

**Syntax**:
```json
{"exists": [value1, value2, ...]}
```

**Returns**: `true` if none of the values are `null`.

**Extension**: Not in standard JSON Logic.

## Gas Metering

JLVM includes built-in gas metering for resource control in blockchain environments.

### Gas Concepts

**Gas Cost**: Amount of gas consumed by an operation
**Gas Limit**: Maximum gas allowed for evaluation
**Gas Used**: Total gas consumed during evaluation
**Depth Penalty**: Additional cost based on expression nesting depth

### Gas Configuration

Default gas costs (`GasConfig.Default`):

| Operation | Cost | Notes |
|-----------|------|-------|
| `const` | 0 | Constants are free |
| `varAccess` | 2 + path length | Variable lookup |
| `if` | 10 | Control flow |
| Logical (`!`, `!!`, `or`, `and`) | 1-2 | Cheap operations |
| Comparison (`==`, `<`, etc.) | 2-3 | Simple comparisons |
| Arithmetic (`+`, `-`, `*`) | 5-8 | Basic math |
| Division/Modulo (`/`, `%`) | 10 | Expensive math |
| `pow` | 20-50 | Very expensive |
| Array ops (`map`, `filter`, `reduce`) | 5-15 | Base cost + per-element |
| String ops (`split`, `join`) | 5-15 | Base cost + length-based |
| Object ops (`get`, `has`) | 3-10 | Map operations |

**Depth Penalty**: `depth * depthPenaltyMultiplier`
- Default multiplier: 5
- Mainnet multiplier: 10

### Additional Costs

Some operations have **additional costs** based on data size:

| Operation | Additional Cost |
|-----------|-----------------|
| `cat` | String length |
| `split` | Array size * 2 |
| `merge` | Result size |
| `unique` | Array size² / 10 |
| `pow` | Exponent value (capped at 1000) |
| Arithmetic on arrays | Array size |

### Gas Presets

**Development** (`GasConfig.Dev`):
- Lower costs for testing
- `map`, `filter`, `reduce` cheaper

**Mainnet** (`GasConfig.Mainnet`):
- Higher costs for expensive operations
- `pow`: 50, `unique`: 30, `split`: 25
- Depth penalty multiplier: 10

### Using Gas Metering

```scala
import io.constellationnetwork.metagraph_sdk.json_logic._
import io.constellationnetwork.metagraph_sdk.json_logic.gas._

val evaluator = JsonLogicEvaluator.make[IO]

evaluator.evaluateWithGas(
  expr = expression,
  data = dataValue,
  ctx = None,
  gasLimit = GasLimit(1_000_000L),
  gasConfig = GasConfig.Default
)
// Returns: Either[JsonLogicException, EvaluationResult[JsonLogicValue]]
```

**Evaluation Result**:
```scala
case class EvaluationResult[A](
  value: A,              // Computed result
  gasUsed: GasUsed,      // Total gas consumed
  maxDepth: Int,         // Maximum nesting depth
  operationCount: Long   // Number of operations executed
)
```

### Gas Exhaustion

When gas limit is exceeded:
```scala
case class GasExhaustedException(
  required: GasCost,
  available: GasLimit
) extends JsonLogicException
```

**Behavior**: Evaluation stops immediately, partial results discarded.

## Evaluation Strategies

JLVM provides two evaluation strategies:

### 1. Tail-Recursive (Default)

**Factory**: `JsonLogicEvaluator.make[F]` or `JsonLogicEvaluator.tailRecursive[F]`

**Features**:
- Stack-safe using `tailRecM`
- Handles deeply nested expressions
- Uses continuation-passing style
- Slightly higher memory overhead

**Use When**:
- Evaluating untrusted/user-provided logic
- Expressions may be deeply nested
- Stack overflow is a concern

### 2. Direct Recursive

**Factory**: `JsonLogicEvaluator.recursive[F]`

**Features**:
- Direct recursive calls
- Simpler implementation
- Lower memory overhead
- May cause stack overflow on deep nesting

**Use When**:
- Expressions are known to be shallow
- Performance is critical
- Maximum control over execution

### Lazy Evaluation

**If/Else Branches**:
Branches are wrapped in `FunctionValue` and only evaluated when selected:

```json
{
  "if": [
    {"var": "expensiveCheck"},
    {"expensive": "operation1"},  // Only evaluated if condition true
    {"expensive": "operation2"}   // Only evaluated if condition false
  ]
}
```

**Array Operation Callbacks**:
Callback expressions in `map`, `filter`, etc. are stored as `FunctionValue` and evaluated on-demand for each element.

## JSON Codec Support

### Encoding

**Expressions to JSON**:
```scala
import io.circe.syntax._
val json: Json = expression.asJson
```

**Values to JSON**:
```scala
val json: Json = value.asJson
```

### Decoding

**JSON to Expression**:
```scala
import io.circe.parser._
val expr: Either[Error, JsonLogicExpression] =
  parse(jsonString).flatMap(_.as[JsonLogicExpression])
```

**JSON to Value**:
```scala
val value: Either[Error, JsonLogicValue] =
  parse(jsonString).flatMap(_.as[JsonLogicValue])
```

### Special Encoding Rules

**Operator Syntax**:
```json
// Single argument
{"op": arg}

// Multiple arguments
{"op": [arg1, arg2, ...]}
```

**Array Syntax** (alternative):
```json
["op", arg1, arg2, ...]
```

**Variable Syntax**:
```json
// Object form
{"var": "path"}
{"var": ["path", default]}

// Empty-key shorthand
{"": "path"}
```

**Functions**:
`FunctionValue` types encode as `null` (not serializable).

## Error Handling

### Exception Types

All errors extend `JsonLogicException`:

```scala
case class JsonLogicException(message: String)
  extends Exception with NoStackTrace

case class GasExhaustedException(required: GasCost, available: GasLimit)
  extends JsonLogicException
```

### Common Error Conditions

| Scenario | Error Message Pattern |
|----------|----------------------|
| Division by zero | `"Division by zero"` |
| Type mismatch | `"Unexpected input for \`op\` got ..."` |
| Missing key | `"Could not find key X in ..."` |
| Invalid coercion | `"Cannot convert X to number"` |
| Gas exhausted | `"Gas exhausted: required X, available Y"` |
| Power overflow | `"Exponent X exceeds maximum ..."` |
| Empty reduction | `"Cannot reduce empty list"` |

### Error Result

Evaluation returns `F[Either[JsonLogicException, Result]]`:

```scala
evaluator.evaluate(expr, data, ctx).map {
  case Right(value) => // Success
  case Left(error)  => // JsonLogicException
}
```

## Differences from Standard JSON Logic

### Type System

| Feature | Standard JSON Logic | JLVM |
|---------|-------------------|------|
| Number type | Single `number` type | Separate `IntValue` and `FloatValue` |
| Precision | IEEE 754 double | Arbitrary precision (`BigInt`, `BigDecimal`) |
| Object support | Limited | Full `MapValue` support |
| Function values | Not supported | `FunctionValue` for lazy evaluation |

### Operators

**Extended Operators** (not in standard):
- Math: `abs`, `round`, `floor`, `ceil`, `pow`
- String: `lower`, `upper`, `join`, `split`, `trim`, `startsWith`, `endsWith`
- Array: `unique`, `slice`, `reverse`, `flatten`, `find`, `intersect`, `count` (with predicate)
- Object: `values`, `keys`, `get`, `has`, `entries`
- Utility: `default`, `exists`, `typeof`

**Modified Behavior**:
- `if`: Lazy evaluation (branches not eagerly evaluated)
- `<`, `<=`: Support 3-argument chaining
- `some`: Optional third argument for minimum count
- `var`: Supports expression-based dynamic paths

### Evaluation

| Feature | Standard | JLVM |
|---------|----------|------|
| Stack safety | Not guaranteed | Tail-recursive by default |
| Gas metering | Not supported | Built-in with configurable costs |
| Lazy evaluation | Not supported | If-else branches lazy |
| Depth tracking | No | Yes (with penalties) |

### JSON Encoding

**Array Syntax**:
Both object `{"op": [...]}` and array `["op", ...]` forms supported.

**Variable Shorthand**:
```json
{"": "path"}  // Equivalent to {"var": "path"}
```

**Dynamic Expressions**:
JLVM supports `MapExpression` and `ArrayExpression` with nested expressions:
```json
{
  "map": [
    [1, 2, 3],
    {"+": [{"var": ""}, 10]}  // Expression as array element
  ]
}
```

## Best Practices

### 1. Type Safety

Prefer strict equality when type matters:
```json
{"===": [{"var": "active"}, true]}  // Better than ==
```

### 2. Gas Management

For blockchain use:
- Set appropriate gas limits
- Use `GasConfig.Mainnet` for production
- Monitor operation counts for complexity
- Test with gas metering enabled

### 3. Error Handling

Provide defaults for potentially missing data:
```json
{"var": ["user.email", "no-email@example.com"]}
```

### 4. Null Safety

Check for null before accessing nested properties:
```json
{
  "and": [
    {"!==": [{"var": "user"}, null]},
    {">": [{"var": "user.age"}, 18]}
  ]
}
```

### 5. Validation

Use `missing` and `missing_some` for input validation:
```json
{
  "if": [
    {"!==": [{"missing": ["name", "email"]}, []]},
    "Missing required fields",
    "valid"
  ]
}
```

### 6. Performance

- Use tail-recursive evaluator for untrusted input
- Minimize depth for better gas efficiency
- Avoid `unique` on large arrays (O(n²) cost)
- Prefer `filter` over manual iteration

## Implementation Notes

### Stack Machine Architecture

The tail-recursive evaluator uses a stack machine with continuation frames:

```scala
sealed trait Frame
case class Eval(expr: JsonLogicExpression, contOpt: Option[Continuation])
case class ApplyValue(value: Result[JsonLogicValue], cont: Continuation)

case class Continuation(
  op: JsonLogicOp,
  processed: List[Result[JsonLogicValue]],
  remaining: List[JsonLogicExpression],
  parent: Option[Continuation],
  isArray: Boolean,
  mapKeys: List[String],
  isIfElse: Boolean,
  varDefault: Option[JsonLogicValue]
)
```

### Result Context Abstraction

Supports both plain and gas-metered evaluation:

```scala
trait ResultContext[Result[_]] {
  def pure[A](value: A): Result[A]
  def map[A, B](ra: Result[A])(f: A => B): Result[B]
  def flatMap[A, B](ra: Result[A])(f: A => Result[B]): Result[B]
  def sequence[A](results: List[Result[A]]): Result[List[A]]
  def extract[A](ra: Result[A]): A
}
```

**Instances**:
- `ResultContext[Id]`: Plain evaluation (no gas tracking)
- `ResultContext[WithGas]`: Gas-metered evaluation (`(A, GasMetrics)`)

### Semantics Layering

**Base Semantics** (`JsonLogicSemantics`):
- Implements operation logic
- Variable resolution
- No gas awareness

**Gas-Aware Semantics** (`GasAwareSemantics`):
- Wraps base semantics
- Adds gas consumption
- Manages gas limit ref
- Computes depth penalties

## Testing

### Unit Tests

Location: `src/test/scala/json_logic/`

**Test Suites**:
- `JsonLogicSpec`: Core functionality tests
- `JsonLogicCodecSuite`: JSON encoding/decoding tests

**Testing Pattern**:
```scala
test("description") {
  val expr = /* JsonLogicExpression */
  val data = /* JsonLogicValue */
  val expected = /* JsonLogicValue */

  JsonLogicEvaluator.tailRecursive[IO]
    .evaluate(expr, data, None)
    .map {
      case Right(result) => expect(result == expected)
      case Left(error) => failure(error.getMessage)
    }
}
```

### Property-Based Testing

Using Weaver and ScalaCheck:
```scala
object JsonLogicSpec extends SimpleIOSuite with Checkers
```

## Future Extensions

Potential areas for extension:

1. **Regular Expressions**: Add `regex` operator for pattern matching
2. **Date/Time**: Operations on temporal values
3. **Cryptographic**: Hash functions, signature verification
4. **Data Structures**: Sets, stacks, queues
5. **Custom Operators**: Plugin system for domain-specific operations
6. **Optimization**: Expression simplification, constant folding
7. **Debugging**: Step-through evaluation, breakpoints
8. **Profiling**: Per-operation timing, gas profiling

## References

- **Standard JSON Logic**: https://jsonlogic.com/
- **JLVM Implementation**: `src/main/scala/io/constellationnetwork/metagraph_sdk/json_logic`
- **Cats Effect**: For functional effect system
- **Circe**: For JSON codec support
- **Enumeratum**: For operator enumeration

---

**Document Version**: 1.0
**Last Updated**: 2025-10-24
**Based on Commit**: 9024552