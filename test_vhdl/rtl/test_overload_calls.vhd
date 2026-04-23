-- Test file for vhdl-find-declaration-overloaded.
--
-- HOW TO USE:
--   Place the cursor as described in each test case, then run:
--     M-x vhdl-find-declaration-overloaded
--   The cursor should jump to the declaration labelled "EXPECTED" below.
--
--   vhdl-gpt-package-dirs must include the path to overload_test_pkg.vhd.
--   From this file's directory that is "../pkg", which is the default.
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.overload_test_pkg.all;

entity test_overload_calls is end entity;

architecture rtl of test_overload_calls is

  -- Typed signals used as actual arguments so vhdl-find-decl--arg-type
  -- can resolve their types by looking up the declarations below.
  signal i1, i2   : integer;
  signal n1       : natural;
  signal slv1     : std_logic_vector(7 downto 0);
  signal slv2     : std_logic_vector(7 downto 0);
  signal uns1     : unsigned(7 downto 0);
  signal uns2     : unsigned(7 downto 0);
  signal sl1      : std_logic;
  signal b1       : boolean;
  signal addr_s   : integer;
  signal data_s   : std_logic_vector(7 downto 0);

  signal r_slv    : std_logic_vector(7 downto 0);
  signal r_int    : integer;
  signal r_nat    : natural;
  signal r_uns    : unsigned(7 downto 0);
  signal r_sl     : std_logic;

begin

  process
    variable vi1, vi2 : integer;
    variable vslv1    : std_logic_vector(7 downto 0);
    variable vslv2    : std_logic_vector(7 downto 0);
    variable vuns1    : unsigned(7 downto 0);
  begin

    --------------------------------------------------------------------------
    -- GROUP A: Type-based disambiguation (1 arg, 3 overloads)
    --
    --   encode(integer)          -> A1: function encode(x : integer)
    --   encode(std_logic_vector) -> A2: function encode(x : std_logic_vector)
    --   encode(boolean)          -> A3: function encode(x : boolean)
    --------------------------------------------------------------------------

    -- A-1  Cursor on 'encode' or anywhere inside 'i1'
    --      EXPECTED: A1  encode(x : integer) return std_logic_vector
    r_slv <= encode(i1);

    -- A-2  Cursor on 'encode' or anywhere inside 'slv1'
    --      EXPECTED: A2  encode(x : std_logic_vector) return std_logic_vector
    r_slv <= encode(slv1);

    -- A-3  Cursor on 'encode' or inside 'true' literal
    --      EXPECTED: A3  encode(x : boolean) return std_logic
    r_sl <= encode(true);

    -- A-4  Cursor inside 'b1' (boolean signal)
    --      EXPECTED: A3  encode(x : boolean) return std_logic
    r_sl <= encode(b1);

    --------------------------------------------------------------------------
    -- GROUP B: Count-based disambiguation (no defaults)
    --
    --   log2(x)           -> B1: function log2(x : natural)
    --   log2(x, round_up) -> B2: function log2(x : natural; round_up : boolean)
    --------------------------------------------------------------------------

    -- B-1  1 arg — EXPECTED: B1  log2(x : natural)
    r_nat <= log2(n1);

    -- B-2  2 args — EXPECTED: B2  log2(x : natural; round_up : boolean)
    r_nat <= log2(n1, false);

    -- B-3  Named, 2 args in declaration order
    --      EXPECTED: B2  log2(x : natural; round_up : boolean)
    r_nat <= log2(x => n1, round_up => true);

    -- B-4  Named, 2 args reversed
    --      EXPECTED: B2  log2(x : natural; round_up : boolean)
    r_nat <= log2(round_up => false, x => n1);

    --------------------------------------------------------------------------
    -- GROUP C: Default values — count range overlap
    --
    --   clamp(x)         -> ambiguous: C1 and C2 both accept 1 arg
    --                       completing-read shows both candidates
    --   clamp(x, hi)     -> C2  (2 args fits C2 range 1-2; C3 requires exactly 3)
    --   clamp(x, lo, hi) -> C3  (3 args, exact match)
    --   clamp(x, hi=>h)  -> C2  (named; lo is omitted, has default; C3 has no defaults)
    --------------------------------------------------------------------------

    -- C-1  1 arg — AMBIGUOUS: completing-read shows C1 and C2
    --      Both   clamp(x : integer)  and  clamp(x : integer; hi := 255)
    --      accept 1 integer argument.
    r_int <= clamp(i1);

    -- C-2  2 args positional — EXPECTED: C2  clamp(x : integer; hi : integer := 255)
    r_int <= clamp(i1, 100);

    -- C-3  3 args positional — EXPECTED: C3  clamp(x : integer; lo : integer; hi : integer)
    r_int <= clamp(i1, 0, 255);

    -- C-4  Named, hi supplied, lo omitted (lo has default in C2, C3 has no defaults)
    --      EXPECTED: C2  clamp(x : integer; hi : integer := 255)
    r_int <= clamp(x => i1, hi => 200);

    -- C-5  Named, all 3 supplied out of order
    --      EXPECTED: C3  clamp(x : integer; lo : integer; hi : integer)
    r_int <= clamp(hi => 200, lo => 10, x => i1);

    --------------------------------------------------------------------------
    -- GROUP D: Procedures — type distinguishes overloads
    --
    --   swap(integer, integer)                    -> D1
    --   swap(std_logic_vector, std_logic_vector)  -> D2
    --------------------------------------------------------------------------

    -- D-1  Positional, integer pair — EXPECTED: D1  swap(a, b : inout integer)
    swap(vi1, vi2);

    -- D-2  Positional, slv pair — EXPECTED: D2  swap(a, b : inout std_logic_vector)
    swap(vslv1, vslv2);

    -- D-3  Named, integer pair in order — EXPECTED: D1
    swap(a => vi1, b => vi2);

    -- D-4  Named, slv pair reversed — EXPECTED: D2
    swap(b => vslv2, a => vslv1);

    --------------------------------------------------------------------------
    -- GROUP E: Multi-name parameter groups  (a, b : TYPE syntax)
    --
    --   min(integer, integer)   -> E1
    --   min(unsigned, unsigned) -> E2
    --------------------------------------------------------------------------

    -- E-1  Integer pair — EXPECTED: E1  min(a, b : integer)
    r_int <= min(i1, i2);

    -- E-2  Unsigned pair — EXPECTED: E2  min(a, b : unsigned)
    r_uns <= min(uns1, uns2);

    -- E-3  Named, unsigned, reversed
    --      EXPECTED: E2  min(a, b : unsigned)
    r_uns <= min(b => uns2, a => uns1);

    --------------------------------------------------------------------------
    -- GROUP F: Named association distinguishes swapped-type overloads
    --
    -- Positional with unknown types would be ambiguous; named association
    -- resolves unambiguously because each formal name maps to a distinct type.
    --
    --   blend(color => slv, weight => int) -> F1: blend(color : slv; weight : int)
    --   blend(color => int, weight => slv) -> F2: blend(color : int; weight : slv)
    --------------------------------------------------------------------------

    -- F-1  Named, matches F1 type layout
    --      EXPECTED: F1  blend(color : std_logic_vector; weight : integer)
    r_slv <= blend(color => slv1, weight => i1);

    -- F-2  Named, matches F2 type layout (arguments reversed relative to F1)
    --      EXPECTED: F2  blend(color : integer; weight : std_logic_vector)
    r_int <= blend(color => i1, weight => slv1);

    -- F-3  Named, out-of-order AND type-swapped (same as F-2 but different order)
    --      EXPECTED: F2  blend(color : integer; weight : std_logic_vector)
    r_int <= blend(weight => slv1, color => i1);

    --------------------------------------------------------------------------
    -- GROUP G: Procedure with optional default argument
    --
    --   write(addr, data)             -> G1  (enable omitted, uses default '1')
    --   write(addr, data, '0')        -> G1  (all 3 args; std_logic literal '0')
    --   write(addr => a, data => d)   -> G1  (named, enable omitted)
    --   write(addr, data, enable=>sl) -> G1  (mixed: 2 positional + 1 named)
    --------------------------------------------------------------------------

    -- G-1  2 args, enable omitted — EXPECTED: G1
    write(addr_s, data_s);

    -- G-2  3 args, '1' literal (std_logic) — EXPECTED: G1
    write(addr_s, data_s, '1');

    -- G-3  3 args, '0' literal — EXPECTED: G1
    write(addr_s, data_s, '0');

    -- G-4  Named, enable omitted — EXPECTED: G1
    write(addr => addr_s, data => data_s);

    -- G-5  Named out-of-order, enable omitted — EXPECTED: G1
    write(data => data_s, addr => addr_s);

    -- G-6  Mixed: positional addr and data, named enable
    --      EXPECTED: G1
    write(addr_s, data_s, enable => sl1);

    --------------------------------------------------------------------------
    -- GROUP H: Subtype family compatibility
    --
    -- natural is in the integer family, so a 'natural' signal satisfies
    -- both an 'integer' formal and a 'natural' formal.
    --
    --   process_nat(natural_signal) -> ambiguous: H1 and H2 both match
    --                                  completing-read shows both candidates
    --------------------------------------------------------------------------

    -- H-1  natural arg — AMBIGUOUS: H1 (natural) and H2 (integer) both accept it
    --      completing-read shows:
    --        function process_nat(x : natural)
    --        function process_nat(x : integer)
    r_int <= process_nat(n1);

    -- H-2  integer arg — EXPECTED: H2  process_nat(x : integer)
    --      (H1 requires natural; integer does NOT satisfy natural subtype
    --       in strict VHDL, but vhdl-find-decl--types-compat-p treats both
    --       as compatible, so completing-read shows both here too)
    r_int <= process_nat(i1);

    --------------------------------------------------------------------------
    -- GROUP I: Nested calls — cursor inside inner call
    --
    -- The enclosing-call detector walks backward through nested parens, so
    -- placing the cursor inside the inner call should resolve the inner one.
    --------------------------------------------------------------------------

    -- I-1  Cursor anywhere inside 'encode(i1)' (the inner call)
    --      EXPECTED: A1  encode(x : integer)
    --      (outer log2 call is ignored because cursor is inside inner parens)
    r_nat <= log2(encode(i1)'length);

    -- I-2  Cursor anywhere on 'log2' or outside the inner parens
    --      EXPECTED: B1  log2(x : natural)
    r_nat <= log2(encode(i1)'length);

    wait;
  end process;

end architecture rtl;
