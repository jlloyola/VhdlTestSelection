-------------------------------------------------------------------------------
-- Purpose:
-- This file implements a state machine that communicates with AD7685 using
-- SPI protocol. This state machine generates the required signals to request
-- and retrieve a sample from the ADC. The main purpose of this module is
-- to implement the physical interface to the ADC, hiding the actual
-- ADC from the rest of the circuit.
--
-- Theory of operation:
-- The state machine has 3 states: IdleSt, ConversionSt, RetrieveDataSt.
--
-- The FSM waits in the IdleSt until a pulse is received in the bGetSample
-- input. When a pulse is received, the state machine transitions to the
-- ConversionSt and drives the aCNV output high to start a in the ADC. Then
-- the FSM waits on the ConversionSt for kConvBrdClkCycles to let the ADC
-- perform the conversion.
--
-- After kConvBrdClkCycles have passed the state machine transitions to the
-- RetrieveDataSt and drives the aCNV output low to let the ADC know the block
-- will now generate a Clock signal in the aSCK output to clock the data out
-- of the ADC.
--
-- When the 16 bits of data have been received the state machine will transition
-- back to the IdleSt making the recent sample data available in the bAdcData
-- output. The bDataValidAdc output is also strobed in this transition to let the
-- host know new data is available. After doing this it waits for the next
-- bGetSample pulse.
--
-------------------------------------------------------------------------------

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

library work;
  use work.PkgAD7685TimeConstants.all;
  use work.PkgTbSimUtilities.all;

entity AdcSpiInterface is
  port (
    abReset          : in  boolean;
    BrdClk           : in  std_logic;
    bGetSample       : in  std_logic;
    aSDO             : in  std_logic;
    aCNV, aSCK, aSDI : out std_logic;
    bDataValidAdc    : out boolean;
    bAdcData         : out std_logic_vector(15 downto 0)
  );

end AdcSpiInterface;

architecture rtl of AdcSpiInterface is

  --vhook_sigstart
  signal cEn: boolean;
  signal cQ: std_logic;
  --vhook_sigend

  -----------------------------------------------------------------------------
  -- Constants
  -----------------------------------------------------------------------------
  -- This constant contains the number of BrdClk cycles the FSM needs to wait
  -- while the ADC performs a conversion. The constant is calculated by
  -- taking the maximum amount of time the ADC can take to do a conversion and
  -- dividing it by the BrdClk period.
  -- The time constants can be found in: PkgAD7685TimeConstants.
  constant kConvBrdClkCycles : positive := (kMaxConvTimePositive_t /
                                           kBrdClkPeriodPositive_t) + 1;


  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------
  type State is (IdleSt, ConversionSt, RetrieveDataSt);
  signal bCurrentState     : State := IdleSt;
  signal bCycleCounter     : natural := 0;
  signal bRetrieveCounter  : natural := 0;
  signal bAdcRetrievedData : std_logic_vector(bAdcData'range) := (others => '0');
  signal bSCK              : std_logic := '0';

begin
  --Keep aSDI high to select CS mode 3-Wire No busy indicator
  aSDI <= '1';

  --!STATE MACHINE STARTUP! This FSM is safe after reset because a Reset
  --Synchronous Deassertion (RSD) is being used.
  --If you are using this module outside of this project, make sure you
  --have RSD modules controlling your reset lines.
  --On the other hand, bGetSample is guaranteed to be '0' after reset.
  FSM: process (abReset, BrdClk)
  begin
    if abReset then
      bCurrentState     <= IdleSt;
      bAdcData          <= (others => '0');
      bDataValidAdc     <= false;
      aCNV              <= '0';
      bCycleCounter     <=  0;
      bRetrieveCounter  <=  0;

    elsif rising_edge(BrdClk) then
      case bCurrentState is
        when IdleSt =>
          bCurrentState    <= IdleSt;
          aCNV             <= '0';
          bCycleCounter    <=  0;
          bRetrieveCounter <=  0;
          bDataValidAdc    <= false;

          if bGetSample = '1' then
            bCurrentState <= ConversionSt;
            --Set aCNV to start a conversion.
            aCNV          <= '1';
            bCycleCounter <= 1;
          end if;


        when ConversionSt =>
          -- Check if we have waited for the conversion to be ready
          if bCycleCounter = kConvBrdClkCycles then
            bCurrentState <= RetrieveDataSt;
            --Clear aCNV to start data transmit.
            aCNV             <= '0';
            bCycleCounter    <=  0;
            bRetrieveCounter <=  1;

          else
            bCurrentState <= ConversionSt;
            aCNV          <= '1';
            --Increment Cycle counter
            bCycleCounter <=  bCycleCounter + 1;
          end if;


        when RetrieveDataSt =>
          -- Count 33 BrdClk cycles to receive the 16 bits from the ADC.
          -- It takes 33 BrdClk cycles because the ADC is being clocked by
          -- aSCK which is half the frequency from BrdClk. and the extra cycle
          -- is added because of the D-Flop being used as serial input.
          if bRetrieveCounter = 33 then
            bCurrentState    <= IdleSt;
            bDataValidAdc    <= true;
            bRetrieveCounter <=  0;
            --Output acquired data
            bAdcData         <= bAdcRetrievedData;

          else
            bCurrentState    <= RetrieveDataSt;
            aCNV             <= '0';
            --Increment Retrieve counter
            bRetrieveCounter <=  bRetrieveCounter + 1;
          end if;
      end case;
    end if;
  end process FSM;

  -- This process generates the aSCK signal to clock out the data
  -- of the ADC; aSCK will be half the frequency of BrdClk.
  -- The signal is enabled only during the RetrieveDataSt and it is
  -- disabled after sending 16 pulses.
  GenerateSCK: process(abReset, BrdClk)
  begin
    if abReset then
      bSCK <= '0';
    elsif rising_edge(BrdClk) then
      if (bCurrentState = RetrieveDataSt) and (bRetrieveCounter < 33) then
        bSCK <= not bSCK;
      else
        bSCK <= '0';
      end if;
    end if;
  end process GenerateSCK;

  aSCK <= bSCK;
  cEn  <= to_boolean(bSCK);
  -- This process is a shift register to retrieve the data from the ADC.
  -- This process is active only when the FSM is in the RetrieveDataSt. Data
  -- is acquired at the falling edge of BrdClk and when bSCK is low to guarantee
  -- the in D-Flop will not change.
  ShiftReg: process(abReset, BrdClk)
  begin
    if abReset then
      bAdcRetrievedData <= (others => '0');
    elsif falling_edge(BrdClk) then
      if (bCurrentState = RetrieveDataSt) and (bSCK = '0') and
         (bRetrieveCounter > 1) then
        bAdcRetrievedData <= bAdcRetrievedData(14 downto 0) & cQ;
      end if;
    end if;
  end process ShiftReg;


  CaptureOnFE: process(abReset, BrdClk)
  begin -- process CaptureOnFE
    if abReset then
      cQ <= '0';
    elsif falling_edge(BrdClk) then
      if cEn then
        cQ <= aSDO;
      end if;
    end if;
  end process CaptureOnFE;

end rtl;

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Component Level Testbench
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

library work;
  use work.PkgAD7685TimeConstants.all;
  use work.PkgTbSimUtilities.all;

library vunit_lib;
  context vunit_lib.vunit_context;

entity tb_AdcSpiInterface is
  generic(
    runner_cfg  : runner_cfg_t
  );
end tb_AdcSpiInterface;

architecture test of tb_AdcSpiInterface is
  constant kVIO : real := 5.0;

  signal abReset: boolean;
  signal aCNV: std_logic := '0';
  signal AnalogIn: unsigned(15 downto 0) := (others => '0');
  signal aSCK: std_logic := '0';
  signal aSDI: std_logic := '0';
  signal aSDO: std_logic := '0';
  signal bAdcData: std_logic_vector(15 downto 0) := (others => '0');
  signal bDataValidAdc: boolean;
  signal bGetSample: std_logic := '0';
  signal BrdClk: std_logic := '0';

  signal StopSim : boolean := false;

begin

  -- Set up the clock(s)
  BrdClk <= not BrdClk after kHalfBrdClkPeriod when not StopSim else '0';

  -- DUT
  DUT: entity work.AdcSpiInterface (rtl)
    port map (
      abReset       => abReset,        -- in  boolean
      BrdClk        => BrdClk,         -- in  std_logic
      bGetSample    => bGetSample,     -- in  std_logic
      aSDO          => aSDO,           -- in  std_logic
      aCNV          => aCNV,           -- out std_logic
      aSCK          => aSCK,           -- out std_logic
      aSDI          => aSDI,           -- out std_logic
      bDataValidAdc => bDataValidAdc,  -- out boolean
      bAdcData      => bAdcData);      -- out std_logic_vector(15 downto 0)


  -- AD7685
  AD7685x: entity work.AD7685 (rtl)
    generic map (
      kVIO => kVIO)  -- in  real := 5.0
    port map (
      AnalogIn => AnalogIn,  -- in  unsigned(15 downto 0)
      aCNV     => aCNV,      -- in  std_logic
      aSCK     => aSCK,      -- in  std_logic
      aSDI     => aSDI,      -- in  std_logic
      aSDO     => aSDO);     -- out std_logic := 'Z'

-----------------------------------------------------------------------------
  -- Main Test Process
-----------------------------------------------------------------------------
  MainTestProc: process
    --Testing Procedure:
    --This procedure requests an ADC sample by setting the bGetSample line
    --of the DUT. It also sends a random value to the ADC model.
    procedure Stimulus(Input : unsigned(AnalogIn'range)) is
    begin
      AnalogIn <= Input;
      bGetSample <= '1';
      ClkWait(1,BrdClk);
      bGetSample <= '0';
      --Wait until data is valid
      wait until bDataValidAdc for kTimeBetweenConv;
      ClkWait(38,BrdClk);
    end procedure Stimulus;

  begin
    --Reset DUT
    abReset <= true, false after 25 ns;
    wait on BrdClk until not abReset;
    ClkWait(1,BrdClk);
    test_runner_setup(runner, runner_cfg);

    -- Run each test in a separate simulation
    while test_suite loop
      if run("Test0") then
        Stimulus(x"1234");
      elsif run("Test1") then
        Stimulus(x"5678");
      elsif run("Test2") then
        Stimulus(x"ABCD");
      elsif run("Test3") then
        Stimulus(x"EF01");
      elsif run("Test4") then
        Stimulus(x"BE57");
      elsif run("Test5") then
        Stimulus(x"FFFF");
      elsif run("Test6") then
        Stimulus(x"5ABE");
      elsif run("Test7") then
        Stimulus(x"0000");
      elsif run("Test8") then
        Stimulus(x"AAAA");
      elsif run("Test9") then
        Stimulus(x"C54D");
      end if;
    end loop;
    test_runner_cleanup(runner); -- Simulation ends here
    StopSim <= true;
    wait;
  end process;

  test_runner_watchdog(runner, 10 ms);

  --This checker process compares the data sent to the ADC model with
  --the result obtained from the AdcSpiInterface.
  CheckerProc: process(BrdClk)
  begin
    if rising_edge(BrdClk) then
      if bDataValidAdc then
        assert std_logic_vector(AnalogIn) = bAdcData
        report "Wrong data was received!" & LF &
              "Expected: " & work.PkgTbSimUtilities.Image(AnalogIn) & LF &
              "Received: " & work.PkgTbSimUtilities.Image(bAdcData)
        severity error;
      end if;
    end if;
  end process;

end test;

