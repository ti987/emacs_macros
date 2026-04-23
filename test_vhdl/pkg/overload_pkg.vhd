-- Package demonstrating overloaded function/procedure declarations.
-- Used as a test target for vhdl-find-declaration-overloaded.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package overload_pkg is

  -- Overloaded function: max
  -- Positional: max(a_int, b_int)    -> resolves to integer overload
  -- Positional: max(a_slv, b_slv)    -> resolves to std_logic_vector overload
  -- Named:      max(b => x, a => y)  -> resolved by formal name lookup
  function max(a : integer; b : integer) return integer;
  function max(a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function max(a : unsigned; b : unsigned) return unsigned;

  -- Overloaded procedure: convert
  -- Positional arg order distinguishes these two overloads.
  procedure convert(din : in std_logic_vector; dout : out integer);
  procedure convert(din : in integer; dout : out std_logic_vector);

  -- Functions with default values — callers may omit trailing args.
  -- clamp(x)           -> resolves to 1-arg overload (no defaults needed)
  -- clamp(x, 0, 255)   -> resolves to 3-arg overload
  -- clamp(x, hi => 10) -> resolves to 3-arg overload (lo defaults to 0)
  function clamp(x : integer) return integer;
  function clamp(x   : integer;
                 lo  : integer := 0;
                 hi  : integer := integer'high) return integer;

  -- Procedure with optional enable flag (default active-high).
  -- write(addr, data)           -> 2 required args
  -- write(addr, data, '0')      -> all 3 args supplied
  -- write(addr => a, data => d) -> named association, enable omitted
  procedure write(addr   : in  integer;
                  data   : in  std_logic_vector;
                  enable : in  std_logic := '1');

end package overload_pkg;

package body overload_pkg is

  function max(a : integer; b : integer) return integer is
  begin
    if a > b then return a; else return b; end if;
  end function max;

  function max(a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
  begin
    if unsigned(a) > unsigned(b) then return a; else return b; end if;
  end function max;

  function max(a : unsigned; b : unsigned) return unsigned is
  begin
    if a > b then return a; else return b; end if;
  end function max;

  procedure convert(din : in std_logic_vector; dout : out integer) is
  begin
    dout := to_integer(unsigned(din));
  end procedure convert;

  procedure convert(din : in integer; dout : out std_logic_vector) is
  begin
    dout := std_logic_vector(to_unsigned(din, dout'length));
  end procedure convert;

  function clamp(x : integer) return integer is
  begin
    return x;
  end function clamp;

  function clamp(x : integer; lo : integer := 0; hi : integer := integer'high)
    return integer is
  begin
    if    x < lo then return lo;
    elsif x > hi then return hi;
    else              return x;
    end if;
  end function clamp;

  procedure write(addr   : in  integer;
                  data   : in  std_logic_vector;
                  enable : in  std_logic := '1') is
  begin
    null;
  end procedure write;

end package body overload_pkg;
