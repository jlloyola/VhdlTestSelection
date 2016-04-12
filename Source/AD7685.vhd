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


-------------------------------------------------------------------------------
-- Timing validations using VUnit Checker
-------------------------------------------------------------------------------
  --synthesis translate_off
  --This process checks Cycle time, Conversion time and acquisition time.
  --It uses scoreboard to keep track when a timing violation occurs on this
  --cycles.
  CheckConversionTimes: process(aCurrentState)
    variable StartTime : time := now;
  begin
    --Check that cycle time is kTimeBetweenConv (4 us)
    if aCurrentState = ConversionSt then
      check(Ad7685_Checker, (((now - StartTime) < kTimeBetweenConv) and
        (now > kTimeBetweenConv)), rtl'path_name & "TcycViolation");
      StartTime := now;
    end if;

    --Check Tacq is at least kAcquisitionTime (1.8 us)
    check(Ad7685_Checker, ((aCurrentState'delayed = AcquisitionSt) and
        (not(aCurrentState'delayed'stable(kAcquisitionTime)))),
        rtl'path_name & "TacqViolation");

    --Check if aCNV remains stable between kMinConvTime and kMaxConvTime
    --And if it has a value of 1 at kMinConvTime
    check(Ad7685_Checker, (aCurrentState'delayed = ConversionSt) and
      (not (aCNV'delayed(kMaxConvTime - kMinConvTime) = '1') or
       not (aCNV'delayed'stable(kMaxConvTime - kMinConvTime))),
      rtl'path_name & "TconvMinViolation");

  end process CheckConversionTimes;

  --This process checks aSCK signal meets timing requirements (Period, low time,
  --high time)
  CheckSCK_Times: process(aSCK)
    variable StartTime : time := now;
  begin
    --Check aSCK meets kSCK_LowTime (7 ns)
    if rising_edge(aSCK) then
      check(Ad7685_Checker, (not aSCK'delayed'stable(kSCK_LowTime)),
        rtl'path_name & "TSckLViolation");
    end if;

    --Check aSCK period
    if falling_edge(aSCK) then
      check(Ad7685_Checker, ((now - StartTime) < kSCK_MinPeriod),
        rtl'path_name & "TSckPeriodViolation");
      StartTime := now;
      --Check aSCK meets kSCK_HighTime (7 ns)
      check(Ad7685_Checker, (not aSCK'delayed'stable(kSCK_HighTime)),
        rtl'path_name & "TSckHViolation");
    end if;
  end process CheckSCK_Times;

  --This process checks aCNV signal meets timing requirements (Tcnvh)
  CheckCNVH: process(aCNV)
  begin
    if (falling_edge(aCNV) and aCurrentState = ConversionSt) then
       check(Ad7685_Checker, (not aCNV'delayed'stable(kCNVPulseWidth)),
         rtl'path_name & "TcnvhViolation");
    end if;
  end process CheckCNVH;

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
    variable RxData : unsigned(AnalogIn'range) := (others => '0');
  begin

    test_runner_setup(runner, runner_cfg);
    -- Set the checker error level to failure
    checker_init(Ad7685_Checker, failure);

    -- Run each test in a separate simulation
    while test_suite loop
      if run("NormalOperationTest") then
        --Send Data to the analog input
        AnalogIn <= x"ABCD";
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
        wait for kAcquisitionTime;


      elsif run("TconvTest") then
        --Violate Tconv timing
        aCNV <= '1', '0' after (kMinConvTime - 1 ns),
                '1' after (kMinConvTime + 2 ns), '0' after kMaxConvTime;
        wait for kTimeBetweenConv;
		checker_found_errors(Ad7685_Checker, CheckResults);
        assert CheckResults
          report "TconvTest failed!"
          severity error;


      elsif run("TcnvhTest") then
        --Violate Tcnvh timing
        aCNV <= '1', '0' after (kCNVPulseWidth - 1 ns),
                '1' after (kCNVPulseWidth * 2), '0' after kMaxConvTime;
        wait for kTimeBetweenConv;
        while not CheckResults loop
		  checker_found_errors(Ad7685_Checker, CheckResults);
          ClkWait(1, aSCK);
        end loop;


      elsif run("TacqTest") then
        --Violate Tacq timing
        --Give less than 1.8 us (kAcquisitionTime)
        aCNV <= '1', '0' after (kTimeBetweenConv - kAcquisitionTime + 30 ns);
        wait for kTimeBetweenConv;
        aCNV <= '1', '0' after (kMaxConvTime);
        wait for kTimeBetweenConv;
        while not CheckResults loop
		  checker_found_errors(Ad7685_Checker, CheckResults);
          ClkWait(1, aSCK);
        end loop;


      elsif run("Tcyc") then
        --Violate Tacq and Tcyc timing
        --Start a normal acquisition
        aCNV <= '1', '0' after (kMaxConvTime);
        --Wait until AcquisitionSt is half way done
        wait for kTimeBetweenConv - kAcquisitionTime / 2;
        --Start a second acquisition before the first one is done
        aCNV <= '1', '0' after (kMaxConvTime);
        wait for kTimeBetweenConv;
        while not CheckResults loop
		  checker_found_errors(Ad7685_Checker, CheckResults);
          ClkWait(1, aSCK);
        end loop;


      elsif run("TSckL_TSckPeriod_TSckH_Test") then
        --Violate aSCK requirements/
        aSCK_Period <= kSCK_MinPeriod - 1 ns;
        wait for 10 * (kSCK_MinPeriod);
        while not CheckResults loop
		  checker_found_errors(Ad7685_Checker, CheckResults);
          ClkWait(1, aSCK);
        end loop;

      end if;
    end loop;

    test_runner_cleanup(runner); -- Simulation ends here

    StopSim <= true;
    wait;
  end process;

  test_runner_watchdog(runner, 10 ms);

end test;