-- cdc_test.vhd
-- Test design exercising all CDC clock-domain detection rules.
--
-- A.1  sys_clk, fast_clk      (name ends with "clk")
-- A.2  ref_clock              (comment contains legacy keyword "dom_clk:")
-- A.2  aux_clock              (comment contains preferred keyword "cdc_clock")
-- A.3  extra_clk              (in vhdl-cdc-clock user variable)
-- B.1  process sensitivity list
-- B.2  annotated_sig  via legacy inline comment "clk_dom:CLK"
-- B.2  annotated_sig2 via alias inline comment "clock_domain:CLK"
-- B.2  annotated_sig3 via preferred inline comment "cdc_domain:CLK"
-- B.2  block comment annotation applies to all signals in the block
-- B.3  instance port connection rule

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cdc_test is
  port (
    sys_clk   : in  std_logic;              -- A.1: name ends with "clk"
    fast_clk  : in  std_logic;              -- A.1: name ends with "clk"
    ref_clock : in  std_logic;              -- A.2: dom_clk: (legacy keyword — still accepted)
    aux_clock : in  std_logic;              -- A.2: cdc_clock (preferred keyword, no colon needed)
    rst_n     : in  std_logic;
    data_in   : in  std_logic_vector(7 downto 0);
    data_out  : out std_logic_vector(7 downto 0);
    valid_out : out std_logic
  );
end entity cdc_test;

architecture rtl of cdc_test is

  -- -----------------------------------------------------------------------
  -- Signals clocked by sys_clk domain (B.1 via process)
  -- -----------------------------------------------------------------------
  signal counter      : integer range 0 to 255;
  signal stage1_data  : std_logic_vector(7 downto 0);
  signal sys_valid    : std_logic;

  -- -----------------------------------------------------------------------
  -- Signals clocked by fast_clk domain (B.1 via process)
  -- -----------------------------------------------------------------------
  signal fast_counter : unsigned(3 downto 0);
  signal fast_data    : std_logic_vector(7 downto 0);

  -- -----------------------------------------------------------------------
  -- Signals with explicit inline domain annotation (B.2)
  -- Three forms accepted; cdc_domain: is the preferred one.
  -- -----------------------------------------------------------------------
  signal annotated_sig  : std_logic;       -- clk_dom:sys_clk          (legacy)
  signal annotated_sig2 : std_logic;       -- clock_domain:sys_clk     (alias)
  signal annotated_sig3 : std_logic;       -- cdc_domain:sys_clk       (preferred)

  -- -----------------------------------------------------------------------
  -- Block annotation: cdc_domain:sys_clk in block comment applies to all
  -- signals below until the blank line (B.2 block annotation).
  -- -----------------------------------------------------------------------
  -- cdc_domain:sys_clk
  signal block_sig1 : std_logic;
  signal block_sig2 : std_logic_vector(3 downto 0);

  -- -----------------------------------------------------------------------
  -- CDC crossing signal (appears in both sys_clk and fast_clk processes)
  -- This will be flagged as a CDC violation (***).
  -- -----------------------------------------------------------------------
  signal crossing_data : std_logic_vector(7 downto 0);

  -- -----------------------------------------------------------------------
  -- CDC crossing signal that is intentionally ignored via vhdl-cdc-ignore
  -- -----------------------------------------------------------------------
  signal gray_count : unsigned(3 downto 0);

  -- -----------------------------------------------------------------------
  -- Signal assigned in sys_clk process, referenced in fast_clk process.
  -- This tests the reference+assignment CDC detection (B.1 ref/assign).
  -- -----------------------------------------------------------------------
  signal shared_ref_data : std_logic;

  -- -----------------------------------------------------------------------
  -- Signal referenced (read) in both sys_clk and fast_clk processes,
  -- but never assigned within the design.  It appears in both domains as
  -- "referenced" and IS a CDC signal: it is being sampled by flip-flops
  -- in two different clock domains.
  -- -----------------------------------------------------------------------
  signal ref_only_sig : std_logic;

  -- -----------------------------------------------------------------------
  -- Signal whose domain is determined by instance port connection (B.3)
  -- The sync_ff entity instance will tie this to sys_clk domain.
  -- -----------------------------------------------------------------------
  signal synced_input : std_logic;

begin

  -- -----------------------------------------------------------------------
  -- Process in sys_clk domain (B.1)
  -- counter, stage1_data, sys_valid are assigned here -> sys_clk domain
  -- crossing_data is ALSO assigned here -> sys_clk (and will also be fast_clk)
  -- -----------------------------------------------------------------------
  p_sys : process(sys_clk, rst_n)
  begin
    if rst_n = '0' then
      counter     <= 0;
      stage1_data <= (others => '0');
      sys_valid   <= '0';
      crossing_data <= (others => '0');
    elsif rising_edge(sys_clk) then
      counter     <= counter + 1;
      stage1_data <= data_in;
      sys_valid   <= '1';
      crossing_data   <= stage1_data;   -- assigned in sys_clk process
      shared_ref_data <= sys_valid;     -- assigned in sys_clk; referenced in fast_clk
      if ref_only_sig = '1' then        -- ref_only_sig referenced in sys_clk
        counter <= counter + 2;
      end if;
    end if;
  end process p_sys;

  -- -----------------------------------------------------------------------
  -- Process in fast_clk domain (B.1)
  -- fast_counter, fast_data are assigned here -> fast_clk domain
  -- crossing_data is ALSO assigned here -> creates CDC violation
  -- -----------------------------------------------------------------------
  p_fast : process(fast_clk, rst_n)
  begin
    if rst_n = '0' then
      fast_counter <= (others => '0');
      fast_data    <= (others => '0');
      crossing_data <= (others => '0');
    elsif rising_edge(fast_clk) then
      fast_counter <= fast_counter + 1;
      fast_data    <= std_logic_vector(fast_counter) & x"0";
      crossing_data <= fast_data;     -- ALSO assigned in fast_clk process -> CDC!
      gray_count   <= gray_count + 1; -- ALSO assigned in fast_clk -> CDC, but ignored
      if shared_ref_data = '1' then   -- shared_ref_data referenced in fast_clk -> CDC!
        fast_data <= (others => '1');
      end if;
      if ref_only_sig = '0' then      -- ref_only_sig referenced in fast_clk -> CDC!
        fast_data(0) <= '1';
      end if;
    end if;
  end process p_fast;

  -- -----------------------------------------------------------------------
  -- Process in ref_clock domain (A.2 clock)
  -- gray_count assigned here -> ref_clock domain
  -- gray_count is also written in fast_clk process -> CDC, but ignored
  -- -----------------------------------------------------------------------
  p_ref : process(ref_clock, rst_n)
  begin
    if rst_n = '0' then
      gray_count <= (others => '0');
    elsif rising_edge(ref_clock) then
      gray_count <= gray_count + 1;
    end if;
  end process p_ref;

  -- -----------------------------------------------------------------------
  -- Instance demonstrating B.3 rule.
  -- sync_ff entity: port "d" is clocked by port "clk".
  -- synced_input is connected to "d", sys_clk is connected to "clk"
  -- => synced_input belongs to sys_clk domain.
  -- -----------------------------------------------------------------------
  u_sync : sync_ff
    port map (
      clk => sys_clk,
      d   => synced_input,
      q   => data_out(0)
    );

  -- -----------------------------------------------------------------------
  -- Concurrent signal assignments
  -- -----------------------------------------------------------------------
  data_out(7 downto 1) <= stage1_data(7 downto 1);
  valid_out            <= sys_valid;

end architecture rtl;
