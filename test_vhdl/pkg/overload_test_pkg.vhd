-- Declarations for testing vhdl-find-declaration-overloaded.
-- Organised in labelled groups; each group targets one resolution mechanism.
--
-- NOTE: vhdl-find-declaration-overloaded searches for  function/procedure
-- NAME(  patterns in both the package declaration and the package body, so
-- it will find two matches per subprogram when both exist in this file.
-- The first match (the prototype in "package ... is") is the declaration
-- you normally want to navigate to.
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package overload_test_pkg is

  ---------------------------------------------------------------------------
  -- GROUP A  Type-based disambiguation, same argument count (1 arg)
  --
  --  encode(integer)          -> A1
  --  encode(std_logic_vector) -> A2
  --  encode(boolean)          -> A3
  ---------------------------------------------------------------------------
  function encode(x : integer)          return std_logic_vector;  -- A1
  function encode(x : std_logic_vector) return std_logic_vector;  -- A2
  function encode(x : boolean)          return std_logic;         -- A3

  ---------------------------------------------------------------------------
  -- GROUP B  Count-based disambiguation, no default values
  --
  --  log2(x)              -> B1  (1 arg, exact)
  --  log2(x, round_up)    -> B2  (2 args, exact)
  ---------------------------------------------------------------------------
  function log2(x : natural)                     return natural;  -- B1
  function log2(x : natural; round_up : boolean) return natural;  -- B2

  ---------------------------------------------------------------------------
  -- GROUP C  Default values — count range overlap, type distinguishes
  --
  --  clamp(x)              -> C1 only  (total=1, required=1; C2 required=1 too
  --                           → ambiguous; completing-read shows C1 and C2)
  --  clamp(x, hi_val)      -> C2 only  (2 args: within C2 range 1-2; C3 needs 3)
  --  clamp(x, lo_val, hi_val) -> C3 only (3 args, exact)
  --  clamp(x, hi => hi_val)   -> C2 only (named, lo defaulted)
  ---------------------------------------------------------------------------
  function clamp(x : integer)                             return integer;  -- C1
  function clamp(x : integer; hi : integer := 255)        return integer;  -- C2
  function clamp(x : integer; lo : integer; hi : integer) return integer;  -- C3

  ---------------------------------------------------------------------------
  -- GROUP D  Procedures — type distinguishes two overloads
  --
  --  swap(integer, integer)              -> D1
  --  swap(std_logic_vector, std_logic_vector) -> D2
  ---------------------------------------------------------------------------
  procedure swap(a : inout integer;          b : inout integer);           -- D1
  procedure swap(a : inout std_logic_vector; b : inout std_logic_vector);  -- D2

  ---------------------------------------------------------------------------
  -- GROUP E  Multi-name parameter groups  (a, b : TYPE syntax)
  --
  --  min(integer, integer)   -> E1
  --  min(unsigned, unsigned) -> E2
  ---------------------------------------------------------------------------
  function min(a, b : integer)  return integer;   -- E1
  function min(a, b : unsigned) return unsigned;  -- E2

  ---------------------------------------------------------------------------
  -- GROUP F  Named association — out-of-order distinguishes overloads
  --
  -- The two overloads share the same formal names but with types swapped.
  -- Positional would be ambiguous when types are unknown (both nil).
  -- Named association resolves unambiguously.
  --
  --  blend(color => slv, weight => int)  -> F1
  --  blend(color => int, weight => slv)  -> F2
  ---------------------------------------------------------------------------
  function blend(color : std_logic_vector; weight : integer)          -- F1
    return std_logic_vector;
  function blend(color : integer; weight : std_logic_vector)          -- F2
    return integer;

  ---------------------------------------------------------------------------
  -- GROUP G  Procedure with default value (optional enable)
  --
  --  write(addr, data)            -> G1  (enable defaults to '1')
  --  write(addr, data, '0')       -> G1  (all 3 supplied)
  --  write(addr => a, data => d)  -> G1  (named, enable omitted)
  ---------------------------------------------------------------------------
  procedure write(addr   : in  integer;                                -- G1
                  data   : in  std_logic_vector;
                  enable : in  std_logic := '1');

  ---------------------------------------------------------------------------
  -- GROUP H  Subtype family compatibility
  --
  --  A 'natural' signal is accepted by an 'integer' formal because
  --  vhdl-find-decl--types-compat-p treats integer/natural/positive as
  --  mutually compatible.
  --
  --  process_nat(x : natural)  -> H1  (exact match on natural)
  --  process_nat(x : integer)  -> H2  (natural is compatible with integer;
  --                               both H1 and H2 match, completing-read shown)
  ---------------------------------------------------------------------------
  function process_nat(x : natural) return integer;  -- H1
  function process_nat(x : integer) return integer;  -- H2

end package overload_test_pkg;

------------------------------------------------------------------------------

package body overload_test_pkg is

  -- A
  function encode(x : integer)          return std_logic_vector is
  begin return std_logic_vector(to_signed(x, 32)); end function;
  function encode(x : std_logic_vector) return std_logic_vector is
  begin return x; end function;
  function encode(x : boolean)          return std_logic is
  begin if x then return '1'; else return '0'; end if; end function;

  -- B
  function log2(x : natural) return natural is
    variable n : natural := 0; variable v : natural := x;
  begin
    while v > 1 loop v := v / 2; n := n + 1; end loop; return n;
  end function;
  function log2(x : natural; round_up : boolean) return natural is
    variable n : natural := log2(x);
  begin
    if round_up and (2**n < x) then return n + 1; end if; return n;
  end function;

  -- C
  function clamp(x : integer) return integer is
  begin return x; end function;
  function clamp(x : integer; hi : integer := 255) return integer is
  begin if x > hi then return hi; end if; return x; end function;
  function clamp(x : integer; lo : integer; hi : integer) return integer is
  begin
    if x < lo then return lo; elsif x > hi then return hi; end if; return x;
  end function;

  -- D
  procedure swap(a : inout integer; b : inout integer) is
    variable t : integer;
  begin t := a; a := b; b := t; end procedure;
  procedure swap(a : inout std_logic_vector; b : inout std_logic_vector) is
    variable t : std_logic_vector(a'range);
  begin t := a; a := b; b := t; end procedure;

  -- E
  function min(a, b : integer)  return integer  is
  begin if a < b then return a; else return b; end if; end function;
  function min(a, b : unsigned) return unsigned is
  begin if a < b then return a; else return b; end if; end function;

  -- F
  function blend(color : std_logic_vector; weight : integer)
    return std_logic_vector is
  begin return color; end function;
  function blend(color : integer; weight : std_logic_vector)
    return integer is
  begin return color; end function;

  -- G
  procedure write(addr : in integer; data : in std_logic_vector;
                  enable : in std_logic := '1') is
  begin null; end procedure;

  -- H
  function process_nat(x : natural) return integer is begin return x; end function;
  function process_nat(x : integer) return integer is begin return x; end function;

end package body overload_test_pkg;
