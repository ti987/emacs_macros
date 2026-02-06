-- Utilities package
library ieee;
use ieee.std_logic_1164.all;

package utils_pkg is
  
  constant TIMEOUT_CYCLES : integer := 1000;
  constant ENABLE_DEBUG : boolean := true;
  
  function parity(data : std_logic_vector) return std_logic;
  
end package utils_pkg;

package body utils_pkg is
  
  function parity(data : std_logic_vector) return std_logic is
    variable result : std_logic := '0';
  begin
    for i in data'range loop
      result := result xor data(i);
    end loop;
    return result;
  end function parity;
  
end package body utils_pkg;
