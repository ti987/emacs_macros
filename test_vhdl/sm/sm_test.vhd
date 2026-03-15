-- sm_test.vhd
-- Test design with state machines for extract-state-machines testing.
--
-- State machines present:
--   cur_state  in nsl_proc  (process with labeled state machine)
--   next_state in seq_proc  (unlabeled process, single state)
-- Non-state case present:
--   data_in    in data_proc (case on non-state signal; must NOT be detected)

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sm_test is
  port (
    clk     : in  std_logic;
    rst     : in  std_logic;
    start   : in  std_logic;
    data_in : in  std_logic_vector(7 downto 0);
    done    : out std_logic;
    data_out : out std_logic_vector(7 downto 0)
  );
end entity sm_test;

architecture rtl of sm_test is

  type state_type is (ST_IDLE, ST_RUNNING, ST_DONE, ST_ERROR);
  signal cur_state  : state_type;
  signal next_state : state_type;

begin

  -- -----------------------------------------------------------------------
  -- Labeled process: combinational next-state logic
  -- State machine: cur_state (ST_IDLE, ST_RUNNING, ST_DONE, ST_ERROR)
  -- -----------------------------------------------------------------------
  nsl_proc : process(cur_state, start)
  begin
    -- default outputs
    done <= '0';
    next_state <= ST_IDLE;

    case cur_state is
      when ST_IDLE =>
        -- Idle: wait for start
        if start = '1' then
          next_state <= ST_RUNNING;
        else
          next_state <= ST_IDLE;
        end if;
      when ST_RUNNING =>
        -- Running: process data
        next_state <= ST_DONE;
        done <= '0';
      when ST_DONE =>
        -- Done: assert done flag
        next_state <= ST_IDLE;
        done <= '1';
      when ST_ERROR =>
        next_state <= ST_IDLE;
      when others =>
        next_state <= ST_IDLE;
    end case;
  end process nsl_proc;

  -- -----------------------------------------------------------------------
  -- Unlabeled process: clocked next-state register
  -- State machine: next_state (single-entry: just ST_IDLE for reset)
  -- -----------------------------------------------------------------------
  process(clk, rst)
  begin
    if rst = '1' then
      cur_state <= ST_IDLE;
    elsif rising_edge(clk) then
      case next_state is
        when ST_IDLE =>
          cur_state <= ST_IDLE;
        when ST_RUNNING =>
          cur_state <= ST_RUNNING;
        when ST_DONE =>
          cur_state <= ST_DONE;
        when others =>
          cur_state <= ST_IDLE;
      end case;
    end if;
  end process;

  -- -----------------------------------------------------------------------
  -- data_proc: case on data_in (NOT a state machine - should be ignored)
  -- -----------------------------------------------------------------------
  data_proc : process(data_in)
  begin
    case data_in is
      when x"00"  => data_out <= x"FF";
      when x"FF"  => data_out <= x"00";
      when others => data_out <= data_in;
    end case;
  end process data_proc;

end architecture rtl;
