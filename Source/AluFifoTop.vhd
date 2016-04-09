-------------------------------------------------------------------------------
--
-- File: AluFifoTop.vhd
-- This is the top-level file. It feeds data from the FIFO into the ALU.
--
-------------------------------------------------------------------------------
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

library lib;
  use lib.PkgAluFifoTop.all;

entity AluFifoTop is
  port (
    Clk          : in  std_logic;
    aReset       : in  std_logic;
    -- Asserts if the result of the last operation was 0
    cZeroFlag    : out std_logic;
    cOverflow    : out boolean;
    cResultValid : out boolean;
    cResult      : out unsigned(kDataLength - 1 downto 0);
    -- FIFO interface
    cWriteFifo   : in  boolean;
    cDataIn      : in  std_logic_vector (kFifoDataLength - 1 downto 0);
    cFifoEmpty   : out boolean;
    cFifoFull    : out boolean
  );

end entity AluFifoTop;

architecture rtl of AluFifoTop is

  signal cOpCode       : std_logic_vector(1 downto 0);
  signal cDataA        : unsigned(kDataLength - 1 downto 0);
  signal cDataB        : unsigned(kDataLength - 1 downto 0);
  signal cPushAluData  : boolean;
  signal cFifoEmptyLcl : boolean;
  signal cFifoOutValid : boolean;
  signal cReadFifo     : boolean;
  signal cFifoDataOut  : std_logic_vector(kFifoDataLength-1 downto 0);

begin -- architecture rtl

  Alu: entity work.SimpleAlu(rtl)
    generic map(
      kDataLength => kDataLength)
    port map(
      Clk          => Clk,
      aReset       => aReset,
      cOpCode      => cOpCode,
      cDataA       => cDataA,
      cDataB       => cDataB,
      cPushData    => cPushAluData,
      cResult      => cResult,
      cResultValid => cResultValid,
      cOverflow    => cOverflow);

  Fifo: entity work.StdFifo(Behavioral)
    generic map(
      kDataWidth  => kFifoDataLength,
      kFifoDepth  => kFifoDepth)
    port map (
      Clk        => Clk,
      aReset     => aReset,
      cWriteEn   => cWriteFifo,
      cDataIn    => cDataIn,
      cReadEn    => cReadFifo,
      cDataOut   => cFifoDataOut,
      cDataValid => cFifoOutValid,
      cEmpty     => cFifoEmptyLcl,
      cFull      => cFifoFull);

  cFifoEmpty <= cFifoEmptyLcl;

  ProcessFifoData: process(aReset, Clk)
  begin -- process ProcessFifoData
    if aReset='1' then
      cReadFifo    <= false;
      cPushAluData <= false;
      cOpCode      <= (others => '0');
      cDataA       <= (others => '0');
      cDataB       <= (others => '0');
    elsif rising_edge(Clk) then
      cPushAluData <= false;
      cReadFifo    <= false;
      if cFifoOutValid then
        cDataA  <= unsigned(cFifoDataOut(kDataA_Msb downto kDataA_Lsb));
        cDataB  <= unsigned(cFifoDataOut(kDataB_Msb downto kDataB_Lsb));
        cOpCode <= cFifoDataOut(kOpCodeMsb downto kOpCodeLsb);
        cPushAluData <= true;
      end if;

      if not cFifoEmptyLcl then
        cReadFifo <= true;
      end if;
    end if;
  end process ProcessFifoData;

  cZeroFlag <= '1' when (cResultValid and (cResult = kResultZero)) else '0';

end architecture rtl;

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Top Level Testbench
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--synthesis translate_off
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

library lib;
  use lib.PkgTbSimUtilities.all;
  use lib.PkgAluFifoTop.all;
  use lib.PkgSimpleAlu.all;

library vunit_lib;
  context vunit_lib.vunit_context;

entity tb_AluFifoTop is
  generic (
    runner_cfg  : runner_cfg_t);
end entity tb_AluFifoTop;

architecture test of tb_AluFifoTop is

  -- DUT signals
  signal Clk          : std_logic := '0';
  signal aReset       : std_logic := '0';
  signal cZeroFlag    : std_logic := '0';
  signal cOverflow    : boolean := false;
  signal cResultValid : boolean := false;
  signal cResult      : unsigned(kDataLength - 1 downto 0);
  signal cWriteFifo   : boolean := false;
  signal cDataIn      : std_logic_vector (kFifoDataLength - 1 downto 0) :=
    (others => '0');
  signal cFifoEmpty   : boolean;
  signal cFifoFull    : boolean;

  -- Clock Frequency
  constant kMainClockFreq : time := 10 ns;

  signal StopSim : boolean := false;

  type ResultCluster_t is record
    OperationResult : unsigned(kDataLength-1 downto 0);
    ZeroFlag        : std_logic;
    Overflow        : boolean;
  end record;

  constant kResultClusterZero : ResultCluster_t := (
    OperationResult => (others => '0'),
    ZeroFlag        => '0',
    Overflow        => false);

  type ResultClusterAry_t is array (natural range <>) of ResultCluster_t;
  type FifoInputAry_t is array (natural range <>) of std_logic_vector(
    kFifoDataLength-1 downto 0);

  -- Arbitrarily select the maximum number of continuous points to be
  -- 5*2**kFifoDepth
  constant kMaxContinuousTests : positive := 5*2**kFifoDepth;
  signal ExpectedDataArray : ResultClusterAry_t(0 to kMaxContinuousTests-1);
  signal ReceivedDataArray : ResultClusterAry_t(0 to kMaxContinuousTests-1);
  signal StimulusDataArray : FifoInputAry_t(0 to kMaxContinuousTests-1);
  signal LogResults : boolean := false;
  signal NumberOfPackets : natural := 0;
  signal LoggingComplete : boolean := false;

  function CalculateResult(
    OpCode : std_logic_vector(kOpCodeSize-1 downto 0);
    A      : unsigned(kDataLength-1 downto 0);
    B      : unsigned(kDataLength-1 downto 0)) return ResultCluster_t is
    variable RetVal : ResultCluster_t;
    variable ExpectedResInt : integer;
    variable ExpectedRes    : unsigned(kDataLength downto 0);
  begin -- function CalculateResult
    case OpCode is
      when kSumOp =>
        ExpectedResInt := to_integer(A) + to_integer(B);
        ExpectedRes := unsigned(std_logic_vector(to_signed(ExpectedResInt,
          kDataLength+1)));

      when kSubractOp =>
        ExpectedResInt := to_integer(A) - to_integer(B);
        ExpectedRes := unsigned(std_logic_vector(to_signed(ExpectedResInt,
          kDataLength+1)));

      when kAndOp =>
        ExpectedRes := ('0' & A) and ('0' & B);

      when others => -- Or
        ExpectedRes := ('0' & A) or ('0' & B);
    end case;

    RetVal.OperationResult := ExpectedRes(kDataLength-1 downto 0);
    RetVal.ZeroFlag := to_StdLogic(ExpectedRes(kDataLength-1 downto 0) =
      kResultZero);
    RetVal.Overflow := To_X01(ExpectedRes(kDataLength))='1';
    return RetVal;
  end function CalculateResult;

begin -- architecture test

  Clk <= not Clk after kMainClockFreq / 2 when not StopSim else
    '0';

  MainTest: process
    procedure PushDataIntoFifo(
      DataIn : std_logic_vector(kFifoDataLength-1 downto 0)) is
    begin -- procedure PushDataIntoFifo
      while cFifoFull loop
        ClkWait(1,Clk);
      end loop;
      cDataIn    <= DataIn;
      cWriteFifo <= true;
      ClkWait(1,Clk);
      wait for 0 ns;
      cWriteFifo <= false;
      ClkWait(1,Clk);
    end procedure PushDataIntoFifo;
    variable InputA, InputB : natural;
    variable OpCode : std_logic_vector(kOpCodeSize-1 downto 0);
    variable ExpectedZero, ReceivedZero : std_logic := '0';
    variable ExpectedRes, ReceivedRes : unsigned(kDataLength-1 downto 0);
  begin -- process MainTest
    test_runner_setup(runner, runner_cfg);

    aReset <= '1';
    ClkWait(2, Clk);
    aReset <= '0';
    ClkWait(1, Clk);

    ----------------------------------------------------------------------------
    -- Tests
    ----------------------------------------------------------------------------
    -- Run each test in a separate simulation
    while test_suite loop
      if run("SimpleTest") then
        ------------------------------------------------------------------------
        -- Build Stimulus Data
        InputA := 7;
        InputB := 8;
        NumberOfPackets <= 4;
        ClkWait(1, Clk);

        for i in 0 to NumberOfPackets-1 loop
          ExpectedDataArray(i) <= CalculateResult(
            std_logic_vector(to_unsigned(i,kOpCodeSize)),
            to_unsigned(InputA, kDataLength),
            to_unsigned(InputB, kDataLength));
          StimulusDataArray(i) <= BuildFifoPacket(
            std_logic_vector(to_unsigned(i,kOpCodeSize)),
            std_logic_vector(to_unsigned(InputA, kDataLength)),
            std_logic_vector(to_unsigned(InputB, kDataLength)));
        end loop;

        ------------------------------------------------------------------------
        -- Stimulate the DUT
        LogResults <= true;
        ClkWait(1, Clk);
        for i in 0 to NumberOfPackets-1 loop
          PushDataIntoFifo(StimulusDataArray(i));
        end loop;

        wait until LoggingComplete;

        ------------------------------------------------------------------------
        -- Verify Results

        for i in 0 to NumberOfPackets-1 loop
          ExpectedRes := ExpectedDataArray(i).OperationResult;
          ReceivedRes := ReceivedDataArray(i).OperationResult;
          assert ExpectedRes = ReceivedRes
            report "Test #" & integer'image(i) & LF &
              "cResult is different from the expected result" & LF &
              "Expected: " & lib.PkgTbSimUtilities.Image(std_logic_vector(
                ExpectedRes))&LF&
              "Received: " & lib.PkgTbSimUtilities.Image(std_logic_vector(
                ReceivedRes))
              severity error;

          ExpectedZero := ExpectedDataArray(i).ZeroFlag;
          ReceivedZero := ReceivedDataArray(i).ZeroFlag;
          assert ExpectedZero = ReceivedZero
            report "Test #" & integer'image(i) & LF &
              "cZeroFlag is different from the expected result" & LF &
              "Expected: " & std_logic'Image(ExpectedZero)&LF&
              "Received: " & std_logic'Image(ReceivedZero)
              severity error;

          assert (ReceivedDataArray(i).Overflow) =
            (ExpectedDataArray(i).Overflow)
            report "Test #" & integer'image(i) & LF &
              "cOverflow is different from the expected result" & LF &
              "Expected: " & boolean'Image(ExpectedDataArray(i).Overflow)&LF&
              "Received: " & boolean'Image(ReceivedDataArray(i).Overflow)
              severity error;
        end loop;
        LogResults <= false;
        ClkWait(1, Clk);


      elsif run("RandomInputsTest") then
        ------------------------------------------------------------------------
        -- Build Stimulus Data
        NumberOfPackets <= kMaxContinuousTests;
        ClkWait(1, Clk);

        for i in 0 to NumberOfPackets-1 loop
          InputA := RandNatural(0, (2**kDataLength)-1);
          InputB := RandNatural(0, (2**kDataLength)-1);
          OpCode := RandSlv(kOpCodeSize);
          ExpectedDataArray(i) <= CalculateResult(
            OpCode,
            to_unsigned(InputA, kDataLength),
            to_unsigned(InputB, kDataLength));
          StimulusDataArray(i) <= BuildFifoPacket(
            OpCode,
            std_logic_vector(to_unsigned(InputA, kDataLength)),
            std_logic_vector(to_unsigned(InputB, kDataLength)));
        end loop;

        ------------------------------------------------------------------------
        -- Stimulate the DUT
        LogResults <= true;
        ClkWait(1, Clk);
        for i in 0 to NumberOfPackets-1 loop
          PushDataIntoFifo(StimulusDataArray(i));
        end loop;

        wait until LoggingComplete;

        ------------------------------------------------------------------------
        -- Verify Results

        for i in 0 to NumberOfPackets-1 loop
          ExpectedRes := ExpectedDataArray(i).OperationResult;
          ReceivedRes := ReceivedDataArray(i).OperationResult;
          assert ExpectedRes = ReceivedRes
            report "Test #" & integer'image(i) & LF &
              "cResult is different from the expected result" & LF &
              "Expected: " & lib.PkgTbSimUtilities.Image(std_logic_vector(
                ExpectedRes))&LF&
              "Received: " & lib.PkgTbSimUtilities.Image(std_logic_vector(
                ReceivedRes))
              severity error;

          ExpectedZero := ExpectedDataArray(i).ZeroFlag;
          ReceivedZero := ReceivedDataArray(i).ZeroFlag;
          assert ExpectedZero = ReceivedZero
            report "Test #" & integer'image(i) & LF &
              "cZeroFlag is different from the expected result" & LF &
              "Expected: " & std_logic'Image(ExpectedZero)&LF&
              "Received: " & std_logic'Image(ReceivedZero)
              severity error;

          assert (ReceivedDataArray(i).Overflow) =
            (ExpectedDataArray(i).Overflow)
            report "Test #" & integer'image(i) & LF &
              "cOverflow is different from the expected result" & LF &
              "Expected: " & boolean'Image(ExpectedDataArray(i).Overflow)&LF&
              "Received: " & boolean'Image(ReceivedDataArray(i).Overflow)
              severity error;
        end loop;
        LogResults <= false;
        ClkWait(1, Clk);
      end if;
    end loop;

    test_runner_cleanup(runner); -- Simulation ends here
    StopSim <= true;
    wait;
  end process MainTest;

  test_runner_watchdog(runner, 10 ms);

  LatchResults: process(Clk)
    variable ResultNumber : natural := 0;
  begin -- process LatchResults
    if falling_edge(Clk) then
      if LogResults then
        if cResultValid then
          ReceivedDataArray(ResultNumber) <= (
            OperationResult => cResult,
            ZeroFlag        => cZeroFlag,
            Overflow        => cOverflow);
          LoggingComplete <= ((NumberOfPackets-1) = ResultNumber);
          ResultNumber := ResultNumber+1;
        end if;
      else
        ReceivedDataArray <= (others => kResultClusterZero);
        ResultNumber := 0;
        LoggingComplete <= false;
      end if;
    end if;
  end process LatchResults;

  DUT: entity work.AluFifoTop(rtl)
    port map(
      Clk          => Clk,
      aReset       => aReset,
      cZeroFlag    => cZeroFlag,
      cOverflow    => cOverflow,
      cResultValid => cResultValid,
      cResult      => cResult,
      cWriteFifo   => cWriteFifo,
      cDataIn      => cDataIn,
      cFifoEmpty   => cFifoEmpty,
      cFifoFull    => cFifoFull);

end architecture test;
--synthesis translate_on