-- Package demonstrating overloaded function/procedure declarations.
-- Used as a test target for vhdl-find-declaration-overloaded.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package overload_pkg is

  -- Overloaded function: max
  -- Calling max(a, b) with integer arguments should resolve here.
  function max(a : integer; b : integer) return integer;

  -- Calling max(a, b) with std_logic_vector arguments should resolve here.
  function max(a : std_logic_vector; b : std_logic_vector) return std_logic_vector;

  -- Calling max(a, b) with unsigned arguments should resolve here.
  function max(a : unsigned; b : unsigned) return unsigned;

  -- Overloaded procedure: convert
  -- Calling convert(din, dout) with (std_logic_vector, integer) resolves here.
  procedure convert(din : in std_logic_vector; dout : out integer);

  -- Calling convert(din, dout) with (integer, std_logic_vector) resolves here.
  procedure convert(din : in integer; dout : out std_logic_vector);

  -- Single-arg overloads of clamp
  function clamp(x : integer) return integer;
  function clamp(x : integer; lo : integer; hi : integer) return integer;

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

  function clamp(x : integer; lo : integer; hi : integer) return integer is
  begin
    if x < lo then return lo;
    elsif x > hi then return hi;
    else return x;
    end if;
  end function clamp;

end package body overload_pkg;
