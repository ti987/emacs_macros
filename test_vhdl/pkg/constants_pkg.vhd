-- Package file with constants and functions
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package constants_pkg is
  
  -- Constants
  constant DATA_WIDTH : integer := 32;
  constant ADDR_WIDTH : integer := 16;
  constant MAX_COUNT : integer := 1024;
  constant RESET_VALUE : std_logic_vector(7 downto 0) := x"00";
  
  -- Functions
  function log2(x : natural) return natural;
  function to_gray(binary : std_logic_vector) return std_logic_vector;
  
end package constants_pkg;

package body constants_pkg is
  
  function log2(x : natural) return natural is
    variable temp : natural := x;
    variable result : natural := 0;
  begin
    while temp > 1 loop
      temp := temp / 2;
      result := result + 1;
    end loop;
    return result;
  end function log2;
  
  function to_gray(binary : std_logic_vector) return std_logic_vector is
    variable gray : std_logic_vector(binary'range);
  begin
    gray(binary'left) := binary(binary'left);
    for i in binary'left-1 downto binary'right loop
      gray(i) := binary(i+1) xor binary(i);
    end loop;
    return gray;
  end function to_gray;
  
end package body constants_pkg;
