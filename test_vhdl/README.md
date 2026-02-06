# VHDL Lookup Table Test

This directory contains test VHDL files to demonstrate the VHDL lookup table functionality.

## Structure

```
test_vhdl/
├── pkg/                      # Package files directory
│   ├── constants_pkg.vhd    # Contains constants and functions
│   └── utils_pkg.vhd        # Contains utility constants and functions
└── rtl/                      # RTL design files
    ├── test_design.vhd      # Test design that uses the packages
    └── test_pkg_suffix.vhd  # Test with _pkg suffix in use statement
```

## Usage

1. Load the vhdl-helper.el file in Emacs:
   ```elisp
   (load-file "/path/to/vhdl-helper.el")
   ```

2. Open the test_design.vhd file in Emacs

3. Run the lookup table display function:
   ```elisp
   M-x vhdl-lookup-display-table
   ```

## What It Does

The `vhdl-lookup-display-table` function will:

1. Extract `use work.PKG.all` statements from the current buffer
   - Handles package names with or without "_pkg" suffix
2. Search for package files (PKG_pkg.vhd or PKG_pkg.vhdl) in:
   - Existing Emacs buffers
   - Same directory as current file
   - Current directory's subdirectories
   - Parent directory
   - Parent's subdirectories
3. Extract constant and function definitions with their types and values
4. Identify which constants/functions are used but not defined locally
5. Display a sorted table showing identifier, type, value, and defining file

## Expected Output

When run on test_design.vhd, the lookup table should show:

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

Note: `LOCAL_CONST` and `local_signal` are not included because they are defined locally in test_design.vhd.

## New Features

### 1. Package Name with _pkg Suffix
The test file `test_pkg_suffix.vhd` uses `use work.constants_pkg.all;` (with _pkg already included).
The function correctly handles this and doesn't add a duplicate "_pkg" suffix when searching for files.

### 2. Type and Value Columns
The table now shows:
- **Type**: The data type of the constant or return type of the function
- **Value**: The initial value for constants, "(function)" for functions

