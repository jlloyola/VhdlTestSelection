-------------------------------------------------------------------------------
--
-- Purpose:
--  This file creates a simulation model for the AD7685 ADC.
--  http://www.analog.com/static/imported-files/data_sheets/AD7685.pdf
-------------------------------------------------------------------------------

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

library work;
  use work.PkgAD7685TimeConstants.all;

library vunit_lib;
  context vunit_lib.vunit_context;

entity AD7685 is
  generic (
    kVIO : real := 5.0);
  port (
    AnalogIn         : in  unsigned(15 downto 0);
    aCNV, aSCK, aSDI : in  std_logic;
    aSDO             : out std_logic := 'Z'
  );

end AD7685;

architecture rtl of AD7685 is

-------------------------------------------------------------------------------
-- Signals
-------------------------------------------------------------------------------
  type State is (ConversionSt, AcquisitionSt);
  signal aCurrentState : State := AcquisitionSt;


begin

-------------------------------------------------------------------------------
-- State Toggling Process
-------------------------------------------------------------------------------
  ToggleState: process(aCNV)
  begin
    if rising_edge(aCNV) and aCurrentState = AcquisitionSt then
      --Change to ConversionSt, then go back to AcquisitionSt after kMaxConvTime
      aCurrentState <= ConversionSt, AcquisitionSt after kMaxConvTime;
    end if;
  end process ToggleState;

-------------------------------------------------------------------------------
-- SPI Process
-------------------------------------------------------------------------------
  SPI: process(aCNV, aSCK)
    variable BitsRemaining : natural := 0;
    variable BitsSent      : natural := 0;
  begin
  --The ADC ignores input signals during the ConversionSt. Because of this,
  --the important behavior takes place in the AcquisitionSt.
    if aCurrentState = AcquisitionSt then
      if rising_edge(aCNV) then
        --Change output to High Z after kSDO_toHighZ
        aSDO <= 'Z' after kSDO_toHighZ;

      elsif falling_edge(aCNV) then
        --Output the MSB of the conversion after kCNV_LowToMSB
        BitsRemaining := AnalogIn'High;
        aSDO <= 'Z', AnalogIn(AnalogIn'High) after kCNV_LowToMSB;
        BitsSent := 1;
      elsif falling_edge(aSCK) then
        if BitsRemaining > 0 then
          BitsRemaining := BitsRemaining - 1;
          --Implement serial data transmision
          aSDO <= 'X' after kSDO_ValidAfterFE, AnalogIn(BitsRemaining) after
                            kSCK_FE_toDataValid - kSDO_ValidAfterFE;
          BitsSent := BitsSent + 1;
        elsif BitsSent = 16 then
        -- After all bits have been sent change output to High Z after kSDO_toHighZ
        -- but drive X after the last falling edge of aSCK
          aSDO <= 'X', 'Z' after kSDO_toHighZ;
          BitsSent := 0;
        else
          aSDO <= 'Z' after kSDO_toHighZ;
        end if;
      end if;
    end if;
  end process SPI;

-------------------------------------------------------------------------------
-- Validation of CS Mode only
-------------------------------------------------------------------------------
  --This process check only  CS, 3-Wire, no busy indicator mode is selected
  ModeSelectionCheck: process(aCNV, aCurrentState)
  begin
    if rising_edge(aCNV) and aCurrentState = AcquisitionSt then
      assert aSDI = '1'
        report "Mode not supported in this model" & LF &
              "Expected aSDI = '1'" & LF &
              "Actual = " & std_logic'Image(aSDI)
        severity failure;
    end if;
    if aCurrentState = AcquisitionSt then
      if falling_edge(aCNV) then
        assert aSDI = '1'
          report "Busy indicator not supported in this model"
          severity failure;
      elsif aCurrentState'delayed = ConversionSt then
        assert aCNV = '1'
          report "Busy indicator mode not supported in this model"
          severity failure;
      end if;
    end if;
  end process ModeSelectionCheck;
  --synthesis translate_on

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

entity tb_AD7685 is
  generic(
    runner_cfg  : runner_cfg_t
  );
end tb_AD7685;

architecture test of tb_AD7685 is
  constant kVIO : real := 5.0;


  signal aCNV: std_logic := '0';
  signal AnalogIn: unsigned(15 downto 0) := (others => '0');
  signal aSCK: std_logic := '0';
  signal aSDI: std_logic := '1';
  signal aSDO: std_logic := '0';

  signal StopSim : boolean := false;

  signal aSCK_Period : time := kBrdClkPeriod;

  shared variable Ad7685_Checker : checker_t;

begin

  -- Set up the clock(s)
  aSCK <= not aSCK after aSCK_Period when (not StopSim)  else '0';

  DUT: entity work.AD7685 (rtl)
    generic map (
      kVIO    => kVIO)
    port map (
      AnalogIn => AnalogIn,  -- in  unsigned(15 downto 0)
      aCNV     => aCNV,      -- in  std_logic
      aSCK     => aSCK,      -- in  std_logic
      aSDI     => aSDI,      -- in  std_logic
      aSDO     => aSDO);     -- out std_logic := 'Z'


  MainTestProc: process
    variable CheckResults : boolean := false;
    procedure StimulateModel(InputData : unsigned(AnalogIn'range)) is
      variable RxData : unsigned(AnalogIn'range) := (others => '0');
    begin
      --Send Data to the analog input
      AnalogIn <= InputData;
      --Set aSDI to select CS mode 3-wire, no busy indicator.
      aSDI <= '1';
      --Make sure we are not starting a conversion
      aCNV <= '0';
      --Check the output is in High-Z when no stimulus has been applied
      wait for kTimeBetweenConv;
      assert aSDO'stable(kTimeBetweenConv) and aSDO = 'Z'
        report "aSDO changed without any input"
        severity error;

      --Set aCNV to start conversion (without violating timing)
      aCNV <= '1', '0' after kMaxConvTime;
      ClkWait(44, aSCK);
      for i in AnalogIn'high downto 0 loop
        ClkWait(1, aSCK);
        RxData(i) := aSDO;
      end loop;
      --Check received data
      assert RxData = AnalogIn
        report "Data Mismatch" & LF &
               "Expected " & Image(AnalogIn) & LF &
               "Received " & Image(RxData)
        severity error;
      --Wait for a full ADC cycle (4 us)
      wait for kTimeBetweenConv;
    end procedure StimulateModel;
  begin

    test_runner_setup(runner, runner_cfg);

    -- Run each test in a separate simulation
    while test_suite loop
      if run("Test0") then
        StimulateModel(x"7894");
      elsif run("Test1") then
        StimulateModel(x"ABCD");
      elsif run("Test2") then
        StimulateModel(x"0000");
      elsif run("Test3") then
        StimulateModel(x"FFFF");
      elsif run("Test4") then
        StimulateModel(x"AAAA");
      elsif run("Test5") then
        StimulateModel(x"75AB");
      elsif run("Test6") then
        StimulateModel(x"FECD");
      elsif run("Test7") then
        StimulateModel(x"D342");
      elsif run("Test8") then
        StimulateModel(x"148B");
      elsif run("Test9") then
        StimulateModel(x"BEEF");
      end if;
    end loop;

    test_runner_cleanup(runner); -- Simulation ends here

    StopSim <= true;
    wait;
  end process;

  test_runner_watchdog(runner, 10 ms);

end test;