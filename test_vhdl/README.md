# VHDL Lookup Table Test

This directory contains test VHDL files to demonstrate the VHDL lookup table functionality.

## Structure

```
test_vhdl/
├── pkg/                      # Package files directory
│   ├── constants_pkg.vhd    # Contains constants and functions
│   └── utils_pkg.vhd        # Contains utility constants and functions
└── rtl/                      # RTL design files
    └── test_design.vhd      # Test design that uses the packages
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
2. Search for package files (PKG_pkg.vhd or PKG_pkg.vhdl) in:
   - Existing Emacs buffers
   - Same directory as current file
   - Current directory's subdirectories
   - Parent directory
   - Parent's subdirectories
3. Extract constant and function definitions from those package files
4. Identify which constants/functions are used but not defined locally
5. Display a sorted table showing the identifier and the file where it's defined

## Expected Output

When run on test_design.vhd, the lookup table should show:

```
VHDL Lookup Table
=================

Identifier                               Defined In
----------                               ----------

ADDR_WIDTH                               constants_pkg.vhd
DATA_WIDTH                               constants_pkg.vhd
ENABLE_DEBUG                             utils_pkg.vhd
MAX_COUNT                                constants_pkg.vhd
RESET_VALUE                              constants_pkg.vhd
TIMEOUT_CYCLES                           utils_pkg.vhd
log2                                     constants_pkg.vhd
parity                                   utils_pkg.vhd
to_gray                                  constants_pkg.vhd
```

Note: `LOCAL_CONST` and `local_signal` are not included because they are defined locally in test_design.vhd.
