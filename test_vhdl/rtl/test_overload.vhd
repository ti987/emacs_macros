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

  -- Cursor on 'max' or inside args -> resolves to max(integer; integer)
  max_int <= max(a_int, b_int);

  -- Cursor on 'max' or inside args -> resolves to max(std_logic_vector; std_logic_vector)
  max_slv <= max(a_slv, b_slv);

  -- Cursor on 'max' or inside args -> resolves to max(unsigned; unsigned)
  max_uns <= max(a_uns, b_uns);

  -- Cursor on 'clamp' -> resolves to single-arg overload
  result <= clamp(a_int);

  -- Cursor on 'clamp' -> resolves to three-arg overload
  result <= clamp(a_int, 0, 255);

  process(a_slv, a_int)
  begin
    -- Cursor inside 'convert' args -> resolves to convert(std_logic_vector, integer)
    convert(a_slv, conv_int);

    -- Cursor inside 'convert' args -> resolves to convert(integer, std_logic_vector)
    convert(a_int, conv_slv);
  end process;

end architecture rtl;
