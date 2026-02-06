-- Test design that uses constants and functions from packages
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Use statements for work library packages
use work.constants_pkg.all;
use work.utils_pkg.all;

entity test_design is
  port (
    clk : in std_logic;
    rst : in std_logic;
    data_in : in std_logic_vector(DATA_WIDTH-1 downto 0);
    addr : in std_logic_vector(ADDR_WIDTH-1 downto 0);
    data_out : out std_logic_vector(DATA_WIDTH-1 downto 0);
    valid : out std_logic
  );
end entity test_design;

architecture rtl of test_design is
  
  signal counter : integer range 0 to MAX_COUNT;
  signal gray_data : std_logic_vector(DATA_WIDTH-1 downto 0);
  signal parity_bit : std_logic;
  signal local_signal : std_logic;
  
  constant LOCAL_CONST : integer := 10;
  
begin
  
  process(clk, rst)
  begin
    if rst = '1' then
      counter <= 0;
      data_out <= RESET_VALUE & x"000000";
      valid <= '0';
    elsif rising_edge(clk) then
      if counter < TIMEOUT_CYCLES then
        counter <= counter + 1;
      end if;
      
      -- Use the log2 function
      if counter = log2(MAX_COUNT) then
        valid <= '1';
      end if;
      
      -- Use the to_gray function
      gray_data <= to_gray(data_in);
      
      -- Use the parity function
      parity_bit <= parity(data_in);
      
      if ENABLE_DEBUG then
        local_signal <= parity_bit;
      end if;
    end if;
  end process;
  
end architecture rtl;
