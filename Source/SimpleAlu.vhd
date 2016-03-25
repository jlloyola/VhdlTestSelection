-------------------------------------------------------------------------------
--
-- File: SimpleAlu.vhd
-- This file implements a basic Arithmetic logic unit. The operations it can
-- perform are:
-- Sum: DataA + DataB
-- Subtract: DataA - DataB
-- And: DataA AND DataB
-- Or: DataA OR DataB
--
-- When the result of a sum or subtract causes an overflow, the cOverflow
-- output will assert to indicate this.
--
-------------------------------------------------------------------------------
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity SimpleAlu is
  generic (
    kDataLength : positive := 8
  );
  port (
  Clk          : in  std_logic;
  aReset       : in  std_logic;
  -- Contains the operation to perform
  cOpCode      : in  std_logic_vector(1 downto 0);
  -- Contains the data to work with
  cDataA       : in  unsigned(kDataLength - 1 downto 0);
  cDataB       : in  unsigned(kDataLength - 1 downto 0);
  cPushData    : in  boolean;
  cResult      : out unsigned(kDataLength - 1 downto 0);
  cResultValid : out boolean;
  cOverflow    : out boolean
  );
end entity SimpleAlu;

architecture rtl of SimpleAlu is

  constant kSumOp     : std_logic_vector(cOpCode'range) := "00";
  constant kSubractOp : std_logic_vector(cOpCode'range) := "01";
  constant kAndOp     : std_logic_vector(cOpCode'range) := "10";
  constant kOrOp      : std_logic_vector(cOpCode'range) := "11";

begin -- architecture rtl

  PerformOperation: process(aReset, Clk)
    variable cTempResult : unsigned(kDataLength downto 0) := (others => '0');
  begin -- process PerformOperation
    if aReset='1' then
      cResult      <= (others => '0');
      cResultValid <= false;
      cOverflow    <= false;
    elsif rising_edge(Clk) then
      cResultValid <= false;
      cOverflow    <= false;
      cTempResult  := (others => '0');

      if cPushData then
        cResultValid <= true;
        case cOpCode is
          when kSumOp =>
            cTempResult := ( '0' & cDataA) + ('0' & cDataB);
            cResult <= cTempResult(cResult'range);
            -- Handle overflow flag
            -- An overflow has occurred when the MSB of cTempResult asserts.
            -- This indicates that the result did not fit inside the range of
            -- cResult
            cOverflow <= (cTempResult(kDataLength) = '1');

          when kSubractOp =>
            cTempResult := ( '0' & cDataA) - ('0' & cDataB);
            cResult <= cTempResult(cResult'range);
            -- Handle overflow flag
            -- An overflow has occurred when the MSB of cTempResult asserts.
            -- This indicates that the result did not fit inside the range of
            -- cResult
            cOverflow <= (cTempResult(kDataLength) = '1');

          when kAndOp =>
            cResult <= cDataA and cDataB;

          when others =>
            cResult <= cDataA or cDataB;

        end case;
      end if;
    end if;
  end process PerformOperation;

end architecture rtl;

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Component Level Testbench
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--synthesis translate_off
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;
  use std.textio.all;

library vunit_lib;
  context vunit_lib.vunit_context;

entity tb_SimpleAlu is
  generic(
    kDataLength : positive := 8;
    runner_cfg  : runner_cfg_t
  );
end entity tb_SimpleAlu;

architecture test of tb_SimpleAlu is

  -- DUT signals
  signal Clk          : std_logic := '0';
  signal aReset       : std_logic := '0';
  signal cOpCode      : std_logic_vector(1 downto 0) := (others => '0');
  signal cDataA       : unsigned(kDataLength - 1 downto 0) := (others => '0');
  signal cDataB       : unsigned(kDataLength - 1 downto 0) := (others => '0');
  signal cPushData    : boolean := false;
  signal cResult      : unsigned(kDataLength - 1 downto 0) := (others => '0');
  signal cResultValid : boolean := false;
  signal cOverflow    : boolean := false;

  -- Clock Frequency
  constant kMainClockFreq : time := 10 ns;

  signal StopSim : boolean := false;

  procedure ClkWait(
    X : integer := 1;
    signal Clk : std_logic) is
  begin -- procedure ClkWait
    for i in 1 to X loop
      wait until rising_edge(Clk);
    end loop;
  end procedure ClkWait;

  function Image(Arg : std_logic_vector) return string is
    variable x : line;
    variable rval : string(1 to Arg'length);
  begin
    rval := x.all;
    deallocate(x);
    return rval;
  end Image;

  function Image(Arg : std_logic) return string is
    variable x : line;
    variable rval : string(1 to 1);
  begin
    rval := x.all;
    deallocate(x);
    return rval;
  end Image;

  function to_StdLogic(b : boolean) return std_logic is
  begin
    if b then
      return '1';
    else
      return '0';
    end if;
  end to_StdLogic;

begin -- architecture test
  -- DUT: SimpleAlu
    -- generic map(
      -- kDataLength => kDataLength)
    -- port map(
      -- Clk          => Clk,
      -- aReset       => aReset,
      -- cOpCode      => cOpCode,
      -- cDataA       => cDataA,
      -- cDataB       => cDataB,
      -- cPushData    => cPushData,
      -- cResult      => cResult,
      -- cResultValid => cResultValid,
      -- cOverflow    => cOverflow);

  Clk <= not Clk after kMainClockFreq / 2 when not StopSim else
    '0';

  MainTest: process
    ----------------------------------------------------------------------------
    -- Tests
    ----------------------------------------------------------------------------
    procedure TestAluOp(
      Operation : std_logic_vector(1 downto 0);
      DataA     : unsigned(kDataLength - 1 downto 0);
      DataB     : unsigned(kDataLength - 1 downto 0)) is
      variable ExpectedResultInt : integer := 0;
      variable ExpectedResult   : unsigned(kDataLength downto 0) :=
        (others => '0');
      variable ExpectedOverflow : boolean := false;
    begin -- procedure TestAluOp
      -- Create expected outputs
      case Operation is
        when "00" => -- Sum
          ExpectedResultInt := to_integer(DataA) + to_integer(DataB);
          ExpectedResult := unsigned(std_logic_vector(to_signed(ExpectedResultInt,
            kDataLength+1)));
          ExpectedOverflow := (To_X01(ExpectedResult(kDataLength))='1');

        when "01" => -- Sub
          ExpectedResultInt := to_integer(DataA) - to_integer(DataB);
          ExpectedResult := unsigned(std_logic_vector(to_signed(ExpectedResultInt,
            kDataLength+1)));
          ExpectedOverflow := (To_X01(ExpectedResult(kDataLength))='1');

        when "10" => -- And
          ExpectedResult := ('0' & DataA) and ('0' & DataB);
          ExpectedOverflow := (To_X01(ExpectedResult(kDataLength))='1');

        when others => -- Or
          ExpectedResult := ('0' & DataA) or ('0' & DataB);
          ExpectedOverflow := (To_X01(ExpectedResult(kDataLength))='1');
      end case;

      -- Stimulate DUT
      cOpCode   <= Operation;
      cDataA    <= DataA;
      cDataB    <= DataB;
      cPushData <= true;
      wait until rising_edge(Clk);
      cPushData <= false;

      -- Capture results
      wait until cResultValid;

      assert cResult = ExpectedResult(cResult'range)
        report "cResult is different from the expected result" & LF &
          "Expected: " & Image(std_logic_vector(ExpectedResult(cResult'range)))&LF&
          "Received: " & Image(std_logic_vector(cResult))
          severity error;

      assert cOverflow = ExpectedOverflow
        report "cOverflow is different from the expected overflow" & LF &
          "Expected: " & Image(std_logic_vector(ExpectedResult(cResult'range)))&LF&
          "Received: " & Image(to_StdLogic(cOverflow))
          severity error;

      wait until rising_edge(Clk);
    end procedure TestAluOp;
  begin -- process MainTest
    test_runner_setup(runner, runner_cfg);

    aReset <= '1';
    ClkWait(2, Clk);
    aReset <= '0';
    ClkWait(1, Clk);

    -- Run each test in a separate simulation
    while test_suite loop
      if run("SimpleAddTest") then
        TestAluOp(
          Operation => "00", -- Sum
          DataA     => to_unsigned(3, kDataLength),
          DataB     => to_unsigned(2, kDataLength));

      elsif run("SimpleSubtractTest") then
        TestAluOp(
          Operation => "01", -- Subtract
          DataA     => to_unsigned(3, kDataLength),
          DataB     => to_unsigned(2, kDataLength));

      elsif run("SimpleAndTest") then
        TestAluOp(
          Operation => "10", -- And
          DataA     => to_unsigned(10, kDataLength),
          DataB     => to_unsigned(3, kDataLength));

      elsif run("SimpleOrTest") then
        TestAluOp(
          Operation => "11", -- Or
          DataA     => to_unsigned(10, kDataLength),
          DataB     => to_unsigned(5, kDataLength));
      end if;
    end loop;

    test_runner_cleanup(runner); -- Simulation ends here
    StopSim <= true;
    wait;
  end process MainTest;

  test_runner_watchdog(runner, 10 ms);

  DUT: entity work.SimpleAlu(rtl)
    generic map(
      kDataLength => kDataLength)
    port map(
      Clk          => Clk,
      aReset       => aReset,
      cOpCode      => cOpCode,
      cDataA       => cDataA,
      cDataB       => cDataB,
      cPushData    => cPushData,
      cResult      => cResult,
      cResultValid => cResultValid,
      cOverflow    => cOverflow);

end architecture test;
