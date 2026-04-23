-- Test design exercising overloaded functions from overload_pkg.
-- Place cursor on any function/procedure call and invoke
-- M-x vhdl-find-declaration-overloaded to jump to the matching declaration.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.overload_pkg.all;

entity test_overload is
  port (
    a_int   : in  integer;
    b_int   : in  integer;
    a_slv   : in  std_logic_vector(7 downto 0);
    b_slv   : in  std_logic_vector(7 downto 0);
    a_uns   : in  unsigned(7 downto 0);
    b_uns   : in  unsigned(7 downto 0);
    addr    : in  integer;
    en      : in  std_logic;
    result  : out integer
  );
end entity test_overload;

architecture rtl of test_overload is

  signal max_int  : integer;
  signal max_slv  : std_logic_vector(7 downto 0);
  signal max_uns  : unsigned(7 downto 0);
  signal conv_int : integer;
  signal conv_slv : std_logic_vector(7 downto 0);

begin

  -- POSITIONAL ASSOCIATION ---------------------------------------------------

  -- Cursor on 'max' -> resolves to max(integer; integer)
  max_int <= max(a_int, b_int);

  -- Cursor on 'max' -> resolves to max(std_logic_vector; std_logic_vector)
  max_slv <= max(a_slv, b_slv);

  -- Cursor on 'max' -> resolves to max(unsigned; unsigned)
  max_uns <= max(a_uns, b_uns);

  -- NAMED ASSOCIATION (in order) --------------------------------------------

  -- Same result as positional; formal names match declaration order.
  max_int <= max(a => a_int, b => b_int);

  -- NAMED ASSOCIATION (out of order) ----------------------------------------

  -- Arguments reversed: 'b => a_int, a => b_int'.
  -- Named matching looks up each formal by name, so still resolves to
  -- the integer overload regardless of order.
  max_int <= max(b => b_int, a => a_int);

  -- OMITTED DEFAULTS ---------------------------------------------------------

  -- clamp with only required arg -> resolves to 1-arg overload
  result <= clamp(a_int);

  -- clamp with all 3 args -> resolves to 3-arg overload (lo/hi supplied)
  result <= clamp(a_int, 0, 255);

  -- clamp with 2 args (lo omitted via named, hi supplied) ->
  -- resolves to 3-arg overload; 'lo' uses its default of 0
  result <= clamp(x => a_int, hi => 100);

  process(a_slv, a_int, addr, en)
  begin
    -- POSITIONAL convert
    -- Cursor on 'convert' -> resolves to convert(std_logic_vector, integer)
    convert(a_slv, conv_int);

    -- Cursor on 'convert' -> resolves to convert(integer, std_logic_vector)
    convert(a_int, conv_slv);

    -- NAMED convert (out of order)
    -- 'dout => conv_int, din => a_slv' reversed but still resolves correctly
    convert(dout => conv_int, din => a_slv);

    -- PROCEDURE WITH DEFAULT: enable omitted -> write(addr, data) is valid
    write(addr, a_slv);

    -- All 3 args supplied (explicit enable)
    write(addr, a_slv, en);

    -- Named with enable omitted
    write(addr => addr, data => a_slv);
  end process;

end architecture rtl;
