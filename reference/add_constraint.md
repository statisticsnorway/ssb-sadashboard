# Add a constraint to a constraint data frame object and open for editing.

Add a constraint to a constraint data frame object and open for editing.

## Usage

``` r
add_constraint(dt, constraint, type = "chr", default = "")
```

## Arguments

- dt:

  Data frame object

- constraint:

  Name of the new constraint to be added

- type:

  The type of constraint it is. "int" for integer, "bool" for boolean

- default:

  The default value to give the constraint

## Value

Updated constraint data frame object
