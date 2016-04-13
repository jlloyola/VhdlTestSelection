-------------------------------------------------------------------------------
--
-- Purpose:
-- This file generates a pulse train that lasts kBrdClkWaits cycles low and
-- 1 BrdClk cycle high for a defined number of times.
--
-- Theory of operation:
-- The number of pulses to be generated is defined by the 17 downto 0 bits from
-- the bNumPulses input. The 17 bit is the start signal. The pulse train
-- generation is done using a state machine with 3 states, IdleSt, HighSt,
-- LowSt. The state machine remains in IdleSt until a start signal is received
-- in the MSB of bNumPulses. When a start signal is received the FSM cycles
-- between LowSt and HighSt until bNumPulses has been generated; then it goes
-- back to idle and waits for the next start signal.
--
-- While the start signal received from the handshake module is true, the
-- FSM will continue to generate a pulse train until the number of pulses has
-- been reached. If at any time the start bit is set to '0' it means a Stop
-- signal has been sent by the host, so this module will return to IdleSt.
--
-- The generated pulse train look like this:
--
--      ___________________|¯¯¯¯¯¯¯¯|_____________________|¯¯¯¯¯¯¯¯|____...
--        kBrdClkWaits     |1 BrdClk|    kBrdClkWaits     |1 BrdClk|
--
-- The bNumPulses data comes from a handshake module so the count gets updated
-- every time there is new valid data from the handshake.
-- When new data is available the bDataValidHS input is strobed, data is stored
-- and the bDataAck output is strobed for 1 BoardClk cycle to let the handshake
-- module know the data was successfully read.
--
-- This module operates with 2 parallel processes, a 1 process FSM and the
-- UpdateCnt process. The FSM is in charge of generating the pulse train when
-- active. The UpdateCnt brings data from the handshake module when it
-- new data becomes available.
--
-- This file also implements a Packet Generator. This process grabs the 16-bit
-- data packets sent from the AdcSpiInteface and concatenates 2 of them together
--
-- If an odd number of samples is requested (e.g. 1001) the packet generator
-- will return 501 32 bit packets and the last data packet will have the valid
-- data on the lower 16 bits of the packet.
--
-- When a packet is ready to be sent, the module check if the downstream
-- FIFO has enough space to fit the data. If the FIFO has space, the data is
-- pushed.
--
-------------------------------------------------------------------------------

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

library work;
  use work.PkgTbSimUtilities.all;
  use work.PkgAdcFifoConfig.all;

entity PulseGenWithCount is
--Change this to select the number of BrdClk cycles the pulse will stay low.
  generic(kBrdClkWaits    : Integer := 159;
          kAiFifoPtrWidth : natural := 7);
  port (
    --Pulse train generator lines
    aReset        : in  boolean;
    BoardClk      : in  std_logic;
    bNumPulses    : in  std_logic_vector(17 downto 0);
    bDataValidHS  : in  boolean;
    bPulseOut     : out std_logic;
    bDataAck      : out boolean;
    --Packet generator lines
    bDataValidAdc             : in  boolean;
    bFifoOvrflwClr            : in  std_logic;
    bAdcData                  : in  std_logic_vector(15 downto 0);
    bAiFifoNumSpacesAvailable : in  unsigned(kAiFifoPtrWidth downto 0);
    bPktOut                   : out std_logic_vector(31 downto 0);
    bAiWriteFifo              : out boolean;
    bFifoOverflow             : out std_logic;
    bFifoDataReady            : out std_logic
  );
end PulseGenWithCount;

architecture rtl of PulseGenWithCount is
  type   StatePulGen_t is (IdleSt, HighSt, LowSt);
  signal bStatePulGen : StatePulGen_t := HighSt;
  signal bTotalPulses : integer;
  signal bPulseCnt    : integer;
  signal bClkCount    : integer;
  signal bStart       : boolean;
  signal bStop        : boolean;

  --PacketGenerator Signals
  type   StatePktGen_t is (IdleSt, HighPktSt, LowPktSt, TransmitSt, OverflowSt,
                           StopSt);
  signal bStatePktGen    : StatePktGen_t := StopSt;
  signal bOddSamples     : boolean;
  signal bUpper16Bit     : boolean;
  signal bPacket         : std_logic_vector(31 downto 0);
  signal bNumPackets     : positive := 1;
  signal bCurrentPacket  : natural  := 0;
  signal bAllSamplesSent : boolean  := false;



begin
  PulseGenFSM: process (aReset, BoardClk)
  begin
    if aReset then
      bStatePulGen <= IdleSt;
      bPulseCnt    <=  0;
      bClkCount    <=  0;
      bPulseOut    <= '0';
    elsif rising_edge(BoardClk) then
        case bStatePulGen is
          when IdleSt =>
            if bStart and (bStatePktGen /= OverflowSt) then
              bStatePulGen <= HighSt;
              bPulseOut    <= '1';
              bClkCount    <= kBrdClkWaits;
              bPulseCnt    <= bTotalPulses - 1;
            else
              bPulseOut <= '0';
              bStatePulGen <= IdleSt;
              bClkCount    <= 0;
              bPulseCnt    <= 0;
            end if;

          when HighSt =>
            bPulseOut    <= '0';
            if bStop or (bStatePktGen = OverflowSt) then
              bStatePulGen <= IdleSt;
            else
              bStatePulGen <= LowSt;
              bClkCount    <= kBrdClkWaits - 1;
            end if;

          when LowSt =>
            if bStop or (bStatePktGen = OverflowSt) then
              bStatePulGen <= IdleSt;
              bPulseOut    <= '0';
            else
              if bClkCount <= 0 then
                if bPulseCnt <= 0 then
                  bStatePulGen <= IdleSt;
                else
                  bPulseCnt    <= bPulseCnt - 1;
                  bStatePulGen <= HighSt;
                  bPulseOut    <= '1';
                end if;
              else
                bPulseOut <= '0';
                bClkCount <= bClkCount - 1;
              end if;
            end if;
        end case;
    end if;
  end process PulseGenFSM;

  -- Count Update Process
  UpdateCnt: process(aReset, BoardClk, bDataValidHS)
  begin
    if aReset then
      bTotalPulses  <= 0;
      bDataAck      <= false;
      bStart        <= false;
      bStop         <= true;
      bOddSamples   <= false;
      bNumPackets   <= 1;
    elsif rising_edge(BoardClk) then
      bDataAck <= false;
        bStart   <= false;
      if bDataValidHS then
        bTotalPulses <= to_Integer(unsigned(bNumPulses(16 downto 0)));
        bDataAck     <= true;
        bStart       <= to_boolean(bNumPulses(17));
        bStop        <= not (to_boolean(bNumPulses(17)));
        bOddSamples  <= to_boolean(bNumPulses(0));
        if(bNumPulses(0) = '1') then
          bNumPackets  <= ((to_Integer(unsigned(bNumPulses(16 downto 0)))) / 2) + 1;
        else
          bNumPackets  <= ((to_Integer(unsigned(bNumPulses(16 downto 0)))) / 2);
        end if;
      end if;
    end if;
  end process UpdateCnt;



  -- Packet generator
  --!STATE MACHINE STARTUP! This FSM is safe after reset because a Reset
  --Synchronous Deassertion (RSD) is being used.
  --If you are using this module outside of this project, make sure you
  --have RSD modules controlling your reset lines.
  --Besides that, this module needs a Start signal to transition to the next
  --state. The start signal is guaranteed to be false after reset.
  PacketGenerator: process(aReset, BoardClk)
  begin
    if aReset then
      bStatePktGen           <= StopSt;
      bPacket                <= (others => '0');
      bPktOut                <= (others => '0');
      bUpper16Bit            <= false;
      bAiWriteFifo           <= false;
      bFifoOverflow          <= '0';
      bCurrentPacket         <= 0;
      bAllSamplesSent        <= false;

    elsif rising_edge(BoardClk) then

    --Make sure bAiWriteFifo is true for 1 BoardClk cycle only
    bAiWriteFifo <= false;

    case bStatePktGen is
          --The state machine waits until a start signal is received from
          --the AdcRegisters.
          when StopSt =>
            if bStart then
              bStatePktGen   <= IdleSt;
              bAiWriteFifo   <= false;
            else
              bPacket         <= (others => '0');
              bUpper16Bit     <= false;
              bAiWriteFifo    <= false;
              bCurrentPacket  <= 0;
              bAllSamplesSent <= false;

            end if;

          --The state machine waits in its idle state until data arrives from
          --the AdcSpiInteface module. It check in which half of the 32-bit
          --packet the data needs to go.
          when IdleSt =>
            bAllSamplesSent <= false;
            if bStop then
              bStatePktGen <= StopSt;
            elsif(bDataValidAdc and (not bUpper16Bit)) then
              bStatePktGen    <= LowPktSt;
            elsif(bDataValidAdc and  bUpper16Bit) then
              bStatePktGen <= HighPktSt;
            end if;

          --In the LowPktSt the FSM check if an odd number of samples was
          --requested. If it was requested and only the last packet is missing,
          --it returns a packet with 16 bits of data in the lower 16 bits
          --and zeros in the upper bits.
          --If it is just a normal packet it returns to the idle state and
          --waits for the next sample.
          when LowPktSt =>
            if bStop then
              bStatePktGen <= StopSt;

            elsif(bOddSamples and (bCurrentPacket = bNumPackets - 1)) then
              bPacket        <= zeros(16) & bAdcData;
              bStatePktGen   <= TransmitSt;
              bCurrentPacket <= bCurrentPacket + 1;

            else
              bStatePktGen   <= IdleSt;
              bPacket        <= zeros(16) & bAdcData;
              bUpper16Bit    <= true;
            end if;

          --In the HighPktSt the FSM concatenates the newest sample with the
          --previous sample in the bPacket vector. Then it transitions to the
          --transmit state to send the data back to the host.
          when HighPktSt =>
            if bStop then
              bStatePktGen <= StopSt;
            else
              bStatePktGen           <= TransmitSt;
              bPacket(31 downto 16)  <= bAdcData;
              bUpper16Bit            <= false;
              bCurrentPacket         <= bCurrentPacket + 1;
            end if;

          --In the TransmitSt the FSM checks if the FIFO has space available to
          --store the new packet. If it has space, the data is pushed into the
          --FIFO and the FSM returns to IdleSt. If there is no space available
          --in the FIFO, the FSM goes to the OverflowSt.
          when TransmitSt =>
            if bStop then
              bStatePktGen <= StopSt;
            elsif(bAiFifoNumSpacesAvailable = 0) then
              bStatePktGen  <= OverflowSt;
              bFifoOverflow <= '1';
            else
              bStatePktGen   <= IdleSt;
              bPktOut        <= bPacket;
              bAiWriteFifo   <= true;
              --Check if this is the last packet
              if (bCurrentPacket = bNumPackets) then
                bCurrentPacket  <= 0;
                bAllSamplesSent <= true;
              end if;
            end if;


          --If bAiFifoNumSpacesAvailable = 0, the FSM will enter the OverflowSt.
          --In this state the bFifoOverflow signal will be set to let software
          --know that an overflow has occurred. The FSM will remain on this
          --state until the bFifoOvrflwClr bit in the StatusReg is set.
          --After the interrupt flag is cleared the FSM will go to StopSt.
          when OverflowSt =>
            if (bFifoOvrflwClr = '1') then
              bStatePktGen  <= StopSt;
              bFifoOverflow <= '0';
            else
              bStatePktGen  <= OverflowSt;
            end if;
        end case;
    end if;
  end process PacketGenerator;

  --This process controls the bFifoDataReady output.
  --If the number of samples requested is less than half the size of the FIFO,
  --the bFifoDataReady output is set when the last data packet has been pushed.
  --If the number of samples requested is bigger than half the size of the FIFO,
  --the bFifoDataReady output is set when the Fifo is half full.
  --The bFifoDataReady output will trigger an interrupt and
  --request a read from the host. bFifoDataReady clears when data is read.
  FifoDataReadyInterrupt: process(BoardClk, aReset)
  begin
    if(aReset) then
      bFifoDataReady <= '0';
    elsif rising_edge(BoardClk) then
      if(bAiFifoNumSpacesAvailable <= 2**(kAiFifoFullSize - 2)) or
        (bNumPackets = 2**(kAiFifoFullSize - 1) - bAiFifoNumSpacesAvailable) or
        (bAllSamplesSent) then
        bFifoDataReady <= '1';
      else
        bFifoDataReady <= '0';
      end if;
    end if;
  end process FifoDataReadyInterrupt;


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
  use work.PkgTbSimUtilities.all;
  use work.PkgAdcFifoConfig.all;
  use work.PkgAD7685TimeConstants.all;

library vunit_lib;
  context vunit_lib.vunit_context;

entity tb_PulseGenWithCount is
  generic(
    runner_cfg  : runner_cfg_t
  );
end tb_PulseGenWithCount;

architecture test of tb_PulseGenWithCount is
  constant kBrdClkPeriod     : time := 25 ns;
  constant kHalfBrdClkPeriod : time := kBrdClkPeriod / 2;
  constant kTimeBetweenAcq   : Integer := 159;
  constant kAiFifoPtrWidth   : Integer := 7;

  --vhook_sigstart
  signal aReset: boolean;
  signal bAdcData: std_logic_vector(15 downto 0) := (others => '0');
  signal bAiFifoNumSpacesAvailable: unsigned(kAiFifoPtrWidth downto 0) := (others => '0');
  signal bAiWriteFifo: boolean;
  signal bDataAck: boolean;
  signal bDataValidAdc: boolean;
  signal bDataValidHs: boolean;
  signal bFifoDataReady: std_logic := '0';
  signal bFifoOverflow: std_logic := '0';
  signal bFifoOvrflwClr: std_logic := '0';
  signal bNumPulses: std_logic_vector(17 downto 0) := (others => '0');
  signal bPktOut: std_logic_vector(31 downto 0) := (others => '0');
  signal bPulseOut: std_logic := '0';
  signal BrdClk: std_logic := '0';
  --vhook_sigend

  signal StopSim : boolean := false;

  signal TestingOverflow : boolean := false;
  signal NumPkts         : natural := 1;
  signal PktCount        : integer := 0;

begin

  -- Set up the clock(s)
  BrdClk  <= not BrdClk after kHalfBrdClkPeriod when not StopSim else '0';


  --vhook_e PulseGenWithCount DUT
  --vhook_g kBrdClkWaits kTimeBetweenAcq
  --vhook_g kAiFifoPtrWidth
  --vhook_a aReset
  --vhook_a BoardClk BrdClk
  --vhook_a bDataValidHs
  --vhook_a bNumPulses
  --vhook_a bPulseOut
  DUT: entity work.PulseGenWithCount (rtl)
    generic map (
      kBrdClkWaits    => kTimeBetweenAcq,  -- in  Integer := 159
      kAiFifoPtrWidth => kAiFifoPtrWidth)  -- in  natural := 7
    port map (
      aReset                    => aReset,                     -- in  boolean
      BoardClk                  => BrdClk,                     -- in  std_logic
      bNumPulses                => bNumPulses,                 -- in  std_logic_vector(17
      bDataValidHS              => bDataValidHs,               -- in  boolean
      bPulseOut                 => bPulseOut,                  -- out std_logic
      bDataAck                  => bDataAck,                   -- out boolean
      bDataValidAdc             => bDataValidAdc,              -- in  boolean
      bFifoOvrflwClr            => bFifoOvrflwClr,             -- in  std_logic
      bAdcData                  => bAdcData,                   -- in  std_logic_vector(15
      bAiFifoNumSpacesAvailable => bAiFifoNumSpacesAvailable,  -- in  unsigned(kAiFifoPtr
      bPktOut                   => bPktOut,                    -- out std_logic_vector(31
      bAiWriteFifo              => bAiWriteFifo,               -- out boolean
      bFifoOverflow             => bFifoOverflow,              -- out std_logic
      bFifoDataReady            => bFifoDataReady);            -- out std_logic




  MainTestProc: process
    -----------------------------------------------------------------------------
    -- Pulse Count Generation test
    -----------------------------------------------------------------------------
    procedure  NumberOfPulses (NumPulses: integer := 1) is
      variable ClkCount : integer := 0;
      variable StartTime  : time := 0 ns;

    begin
    --Set Valid values to PacketGenerator lines to avoid conflicts
      bDataValidAdc              <= false;
      bFifoOvrflwClr             <= '0';
      bAdcData                   <= (others => '0');


      ClkWait(1, BrdClk);
      --Start pulse train generation
      bNumPulses <= '1' & std_logic_vector((to_unsigned(NumPulses,bNumPulses'length-1)));
      --Send data valid
      bDataValidHs <= true;
      ClkWait(1, BrdClk);
      bDataValidHS <= false;
      --Wait for Ack
      wait until bDataAck;


      --Check pulse width is only 1 BrdClk cycle
      for i in 1 to NumPulses loop
        wait until  rising_edge(bPulseOut);
        StartTime := now;
        wait until  falling_edge(bPulseOut);

        assert (now - StartTime) = kBrdClkPeriod
          report "Pulse width different from 1 kBrdClkPeriod!" & LF &
                 "Expected: "  & time'Image(kBrdClkPeriod) & LF &
                 "Received: "  & time'Image(now - StartTime)
          severity error;
      end loop;


       for i in 1 to kTimeBetweenAcq * 2 loop
       wait until rising_edge(BrdClk);
        assert bPulseOut = '0'
          report "Pulse generation did not stop!" & LF &
                 "Expected: "  & Image('0') & LF &
                 "Received: "  & Image(bPulseOut)
          severity error;
      end loop;


      --Start pulse train generation
      bNumPulses <= '1' & std_logic_vector((to_unsigned(NumPulses,bNumPulses'length-1)));
      --Send data valid
      bDataValidHs <= true;
      ClkWait(1, BrdClk);
      bDataValidHS <= false;
      --Wait for Ack
      wait until bDataAck;
      --Check pulse stays low for kTimeBetweenAcq BrdClk cycles
      for i in 1 to NumPulses - 1 loop
        wait until  falling_edge(bPulseOut);
        StartTime := now;
        wait until  rising_edge(bPulseOut);

        assert (now - StartTime) = kTimeBetweenAcq * kBrdClkPeriod
          report "Pulse did not stay low for kTimeBetweenAcq * kBrdClkPeriods!"
                  & LF &
                 "Expected: "  & time'Image(kTimeBetweenAcq * kBrdClkPeriod) & LF &
                 "Received: "  & time'Image(now - StartTime)
          severity error;
      end loop;

      wait for ((kTimeBetweenAcq * kBrdClkPeriod) + kBrdClkPeriod);

      --Start pulse train generation
      bNumPulses <= '1' & std_logic_vector((to_unsigned(NumPulses,bNumPulses'length-1)));
      --Send data valid
      bDataValidHs <= true;
      ClkWait(1, BrdClk);
      bDataValidHS <= false;

      wait until bPulseOut = '1';

      --Stop pulse train generation
      bNumPulses <= '0' & std_logic_vector((to_unsigned(NumPulses,bNumPulses'length-1)));
      --Send data valid
      bDataValidHs <= true;
      ClkWait(1, BrdClk);
      bDataValidHS <= false;
      --Wait for Ack
      wait until bDataAck;
      --Checking pulse generation is off
      for i in 1 to kTimeBetweenAcq * 2 loop
       wait until rising_edge(BrdClk);
        assert bPulseOut = '0'
          report "Pulse generation did not stop!" & LF &
                 "Expected: "  & Image('0') & LF &
                 "Received: "  & Image(bPulseOut)
          severity error;
      end loop;
    end procedure NumberOfPulses;

    -----------------------------------------------------------------------------
    -- Procedure to Request Packets
    -----------------------------------------------------------------------------
    procedure  RequestPacket (NumSamples: integer := 1) is
      variable AdcDataLow  : std_logic_vector(bAdcData'range);
      variable AdcDataHigh : std_logic_vector(bAdcData'range);
    begin
      --Send start signal to enable the PacketGenerator FSM
      --Send number of samples to be acquired
      bNumPulses <= '1' & std_logic_vector((to_unsigned(NumSamples,bNumPulses'length-1)));
      --Send data valid
      bDataValidHs <= true;
      ClkWait(1, BrdClk);
      bDataValidHS <= false;
      --Check if NumSamples is odd or even.
      for i in 1 to NumSamples loop
          --Check if NumSamples is odd and if we are on the last packet.
        if ((NumSamples mod 2 = 1) and i = NumSamples) then
          --Wait for the Adc Conversion time
          wait for kTimeBetweenConv;
          --Send ADC Data
          --AdcDataLow := Rand.GetStdLogicVector(bAdcData'length);
          AdcDataLow := std_logic_vector(to_unsigned(i,bAdcData'length));
          bAdcData   <= AdcDataLow;
          --Send data valid from AdcSpiInterface
          bDataValidAdc <= true;
          ClkWait(1, BrdClk);
          bDataValidAdc <= false;
          --Check last packet with zeros in the top 16 bits
          wait until bAiWriteFifo for 5 us;
          assert bPktOut = zeros(16) & AdcDataLow
            report "Packet data is wrong!" & LF &
                  "Expected: "  & work.PkgTbSimUtilities.Image(zeros(16) & AdcDataLow) & LF &
                  "Received: "  & work.PkgTbSimUtilities.Image(bPktOut)
            severity error;


        --Even number of samples or not last odd packet
        else
          --Check in which part of the packet the data must be
          if (i mod 2 = 1) then
            --Wait for the Adc Conversion time
            wait for kTimeBetweenConv;
            --Send ADC Data
            --AdcDataLow := Rand.GetStdLogicVector(bAdcData'length);
            AdcDataLow := std_logic_vector(to_unsigned(i,bAdcData'length));
            bAdcData   <= AdcDataLow;
            --Send data valid from AdcSpiInterface
            bDataValidAdc <= true, false after kBrdClkPeriod + 1 ps;

          else
            --Second sample
            --Wait for the Adc Conversion time
            wait for kTimeBetweenConv;
            --Send ADC Data
            --AdcDataHigh := Rand.GetStdLogicVector(bAdcData'length);
            AdcDataHigh := std_logic_vector(to_unsigned(i,bAdcData'length));
            bAdcData    <= AdcDataHigh;
            --Send data valid from AdcSpiInterface
            bDataValidAdc <= true;
            ClkWait(1, BrdClk);
            bDataValidAdc <= false;

            --Check packet every 2 samples if there is no Overflow
            if (bFifoOverflow = '0') then
              wait on bAiWriteFifo, bFifoOverflow;
              if (bFifoOverflow = '0') then
                assert (bPktOut = AdcDataHigh & AdcDataLow)
                  report "Packet data is wrong!" & LF &
                        "Expected: "  & work.PkgTbSimUtilities.Image(AdcDataHigh)&" "
                        & work.PkgTbSimUtilities.Image(AdcDataLow)
                        & LF &
                        "Received: "  & work.PkgTbSimUtilities.Image(bPktOut)
                  severity error;
              end if;
            end if;
          end if;
        end if;
      end loop;
    end procedure RequestPacket;

    -----------------------------------------------------------------------------
    -- Packet Generator test
    -----------------------------------------------------------------------------
    procedure  PacketGeneratorTest (NumSamples: integer := 1) is
      variable AdcDataLow  : std_logic_vector(bAdcData'range);
      variable AdcDataHigh : std_logic_vector(bAdcData'range);
    begin
     --Set Valid values to PacketGenerator lines to avoid conflicts
      bDataValidAdc              <= false;
      bFifoOvrflwClr             <= '0';
      TestingOverflow            <= false;
      --Obtain the total number of packets and check if it is odd or even.
      if((NumSamples mod 2) = 0) then
        NumPkts <= NumSamples / 2;
      else
        NumPkts <= (NumSamples / 2) + 1;
      end if;

      ClkWait(1, BrdClk);
      RequestPacket(NumSamples);
      ClkWait(10, BrdClk);
    end procedure PacketGeneratorTest;
  begin
    aReset <= true, false after 25 ns;
    test_runner_setup(runner, runner_cfg);

    -- Run each test in a separate simulation
    while test_suite loop
      if run("PulseCountGenTest0") then
        NumberOfPulses(2);
      elsif run("PulseCountGenTest1") then
        NumberOfPulses(3);
      elsif run("PulseCountGenTest2") then
        NumberOfPulses(10);
      elsif run("PulseCountGenTest3") then
        NumberOfPulses(RandNatural(1,30));
      elsif run("PulseCountGenTest4") then
        NumberOfPulses(RandNatural(1,30));
      elsif run("PulseCountGenTest5") then
        NumberOfPulses(RandNatural(1,30));
      elsif run("PulseCountGenTest6") then
        NumberOfPulses(RandNatural(1,30));
      elsif run("PulseCountGenTest7") then
        NumberOfPulses(RandNatural(1,30));
      elsif run("PulseCountGenTest8") then
        NumberOfPulses(RandNatural(1,30));
      elsif run("PulseCountGenTest9") then
        NumberOfPulses(25);

      elsif run("PacketGenTest0") then
        PacketGeneratorTest(1);
      elsif run("PacketGenTest1") then
        PacketGeneratorTest(3);
      elsif run("PacketGenTest2") then
        PacketGeneratorTest(4);
      elsif run("PacketGenTest3") then
        PacketGeneratorTest(10);
      elsif run("PacketGenTest4") then
        PacketGeneratorTest(7);
      elsif run("PacketGenTest5") then
        PacketGeneratorTest(2000);
      elsif run("PacketGenTest6") then
        PacketGeneratorTest(101);
      elsif run("PacketGenTest7") then
        PacketGeneratorTest(2001);
      elsif run("PacketGenTest8") then
        PacketGeneratorTest(257);
      elsif run("PacketGenTest9") then
        PacketGeneratorTest(100);
      end if;
    end loop;
    test_runner_cleanup(runner); -- Simulation ends here
    StopSim <= true;
    wait;
  end process;

  test_runner_watchdog(runner, 10 ms);

  -----------------------------------------------------------------------------
    -- FIFO Write and Overflow Checker
  -----------------------------------------------------------------------------
  --This process checks the FIFO is not written when there is an overflow flag
  --It checks it on every edge of BrdClk
  FIFO_Checker: process(BrdClk)
  begin
    if bFifoOverflow = '1' then
      assert bAiWriteFifo = false
        report "FIFO was written with overflow flag set!" & LF &
                    "Expected: "  & boolean'Image(false) & LF &
                    "Received: "  & boolean'Image(bAiWriteFifo)
        severity error;
    end if;
  end process FIFO_Checker;

  -----------------------------------------------------------------------------
    -- FIFO Data Ready flag checker
  -----------------------------------------------------------------------------
  --This process checks the FIFO half full flag is set when the FIFO is half
  --full, or when the acquisition is done and there is still data to be collected.
  FifoHalfFull_Checker: process (BrdClk)
  begin
    if falling_edge(BrdClk) then
      if (bAiFifoNumSpacesAvailable <= 2**(kAiFifoFullSize - 2) or
          (NumPkts = 2**(kAiFifoFullSize - 1) - bAiFifoNumSpacesAvailable) or
           NumPkts = PktCount) then
          assert bFifoDataReady = '1'
            report "FIFO Half full flag was not set!" & LF &
                        "Expected: "  & Image('1') & LF &
                        "Received: "  & Image(bFifoDataReady)
            severity error;

      elsif (now /= 0 ps) and (not TestingOverflow) then
        assert bFifoDataReady = '0'
          report "FIFO Half full flag was not cleared!" & LF &
                      "Expected: "  & Image('0') & LF &
                      "Received: "  & Image(bFifoDataReady)
          severity error;
      end if;
    end if;
  end process FifoHalfFull_Checker;

  -----------------------------------------------------------------------------
    -- FIFO Simulator
  -----------------------------------------------------------------------------
  --This process simulates a FIFO. It decrements the bAiFifoNumSpacesAvailable when
  --the bAiWriteFifo line is strobed. It flushes the FIFO when the bFifoDataReady
  --flag is set.
  FifoSim: process(BrdClk, aReset)
  begin
    if aReset then
        bAiFifoNumSpacesAvailable <= x"80";
    elsif falling_edge(BrdClk) then
      --"Fill the FIFO" when testing overflow
      if TestingOverflow then
        bAiFifoNumSpacesAvailable  <= (others => '0');
      else
        if bFifoDataReady = '1' then
          --"Retrieve data from the FIFO" (leave 128 free spaces)
          bAiFifoNumSpacesAvailable <= x"80";
        elsif bAiWriteFifo then
          --"Push an element into the FIFO"
          bAiFifoNumSpacesAvailable <= bAiFifoNumSpacesAvailable - 1;
        end if;
      end if;
    end if;
  end process FifoSim;

  -----------------------------------------------------------------------------
    -- Packet Counter
  -----------------------------------------------------------------------------
  --This process counts the number of bAiWriteFifo pulses, simulating how many
  --elements have been pushed into the FIFO. When the the number of elements
  --matches the number of packets, the counter is reset.
  PacketCounter: process(BrdClk, aReset)
  begin
    if aReset then
      PktCount <= 0;
    elsif falling_edge(BrdClk) then
      if bAiWriteFifo then
        PktCount <= PktCount + 1;
      end if;
      if PktCount = NumPkts then
        PktCount <= 0;
      end if;
    end if;
  end process PacketCounter;
end test;