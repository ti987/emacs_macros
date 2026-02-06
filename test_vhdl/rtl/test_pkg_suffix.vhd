-- Test design that uses constants_pkg with _pkg suffix in use statement
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Use statements with _pkg already included
use work.constants_pkg.all;

entity test_pkg_suffix is
  port (
    clk : in std_logic;
    rst : in std_logic;
    data_in : in std_logic_vector(DATA_WIDTH-1 downto 0);
    addr : in std_logic_vector(ADDR_WIDTH-1 downto 0);
    data_out : out std_logic_vector(DATA_WIDTH-1 downto 0)
  );
end entity test_pkg_suffix;

architecture rtl of test_pkg_suffix is
  
  signal counter : integer range 0 to MAX_COUNT;
  signal reset_vec : std_logic_vector(7 downto 0);
  
begin
  
  process(clk, rst)
  begin
    if rst = '1' then
      counter <= 0;
      reset_vec <= RESET_VALUE;
    elsif rising_edge(clk) then
      if counter < MAX_COUNT then
        counter <= counter + 1;
      end if;
    end if;
  end process;
  
end architecture rtl;
