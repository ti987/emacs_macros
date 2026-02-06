# VHDL Lookup Table - Enhanced Output Format

## Changes Implemented

### 1. Handle Package Names with "_pkg" Suffix

**Before:**
- If use statement was `use work.constants_pkg.all;`
- Function would search for `constants_pkg_pkg.vhd` (incorrect!)

**After:**
- If use statement is `use work.constants_pkg.all;`
- Function correctly searches for `constants_pkg.vhd` (no duplicate _pkg)
- If use statement is `use work.constants.all;`
- Function searches for `constants_pkg.vhd` (adds _pkg as before)

### 2. Enhanced Table with Type and Value Columns

**Old Output (2 columns):**
```
VHDL Lookup Table
=================

Identifier                               Defined In
----------                               ----------

ADDR_WIDTH                               constants_pkg.vhd
DATA_WIDTH                               constants_pkg.vhd
MAX_COUNT                                constants_pkg.vhd
log2                                     constants_pkg.vhd
parity                                   utils_pkg.vhd
```

**New Output (4 columns):**
```
VHDL Lookup Table
=================

Identifier                     Type                      Value                Defined In
----------                     ----                      -----                ----------

ADDR_WIDTH                     integer                   16                   constants_pkg.vhd
DATA_WIDTH                     integer                   32                   constants_pkg.vhd
ENABLE_DEBUG                   boolean                   true                 utils_pkg.vhd
MAX_COUNT                      integer                   1024                 constants_pkg.vhd
RESET_VALUE                    std_logic_vector(7 down   x"00"                constants_pkg.vhd
TIMEOUT_CYCLES                 integer                   1000                 utils_pkg.vhd
log2                           natural                   (function)           constants_pkg.vhd
parity                         std_logic                 (function)           utils_pkg.vhd
to_gray                        std_logic_vector          (function)           constants_pkg.vhd
```

## Implementation Details

### Data Structure Changes

**Before:**
- Functions returned: `(ID . FILE)` pairs
- Example: `("DATA_WIDTH" . "/path/to/constants_pkg.vhd")`

**After:**
- Functions return: `(ID TYPE VALUE FILE)` lists
- Example: `("DATA_WIDTH" "integer" "32" "/path/to/constants_pkg.vhd")`

### Regular Expression Extraction

For **constants**, the regex `vhdl-re-decl-constant` has these capture groups:
- Group 1: "constant" keyword
- Group 2: Identifier name
- Group 3: Type
- Group 4: Initial value

For **functions**, enhanced regex captures:
- Group 1: Function name
- Group 2: Return type
- Value shown as "(function)" to indicate it's not a constant

## Test Files

### test_pkg_suffix.vhd
New test file that uses `use work.constants_pkg.all;` (with _pkg suffix already included) to verify the fix for requirement #1.

## Modified Functions

1. **vhdl-lookup-find-package-file**: Added check for existing "_pkg" suffix
2. **vhdl-lookup-extract-definitions**: Enhanced to extract type and value
3. **vhdl-lookup-build-table**: Updated to work with new data structure
4. **vhdl-lookup-display-table**: Modified to show 4 columns instead of 2
