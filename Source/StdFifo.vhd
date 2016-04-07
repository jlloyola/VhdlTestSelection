-------------------------------------------------------------------------------
--
-- File: StdFifo.vhd
-- This file implements a simple FIFO module.
-- The code was obtained from:
-- http://www.deathbylogic.com/2013/07/vhdl-standard-fifo/
--
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity StdFifo is
  generic (
    kDataWidth  : positive := 8;
    kFifoDepth  : positive := 256
  );
  Port (
    Clk      : in  std_logic;
    aReset   : in  std_logic;
    cWriteEn : in  std_logic;
    cDataIn  : in  std_logic_vector (kDataWidth - 1 downto 0);
    cReadEn  : in  std_logic;
    cDataOut : out std_logic_vector (kDataWidth - 1 downto 0);
    cEmpty   : out std_logic;
    cFull    : out std_logic
  );
end StdFifo;

architecture Behavioral of StdFifo is

begin
  -- Memory Pointer Process
  fifo_proc : process (Clk)
    type FifoMemory_t is array (0 to kFifoDepth - 1) of std_logic_vector(
      kDataWidth - 1 downto 0);
    variable Memory : FifoMemory_t;

    variable Head : natural range 0 to kFifoDepth - 1 := 0;
    variable Tail : natural range 0 to kFifoDepth - 1 := 0;

    variable Looped : boolean;
  begin
    if aReset = '1' then
        Head := 0;
        Tail := 0;
        Looped := false;
        cFull  <= '0';
        cEmpty <= '1';
        cDataOut <= (others => '0');
    elsif rising_edge(Clk) then
      if (cReadEn = '1') then
        if ((Looped = true) or (Head /= Tail)) then
          -- Update data output
          cDataOut <= Memory(Tail);

          -- Update Tail pointer as needed
          if (Tail = kFifoDepth - 1) then
            Tail := 0;

            Looped := false;
          else
            Tail := Tail + 1;
          end if;


        end if;
      end if;

      if (cWriteEn = '1') then
        if ((Looped = false) or (Head /= Tail)) then
          -- Write Data to Memory
          Memory(Head) := cDataIn;

          -- Increment Head pointer as needed
          if (Head = kFifoDepth - 1) then
            Head := 0;

            Looped := true;
          else
            Head := Head + 1;
          end if;
        end if;
      end if;

      -- Update cEmpty and cFull flags
      if (Head = Tail) then
        if Looped then
          cFull <= '1';
        else
          cEmpty <= '1';
        end if;
      else
        cEmpty <= '0';
        cFull  <= '0';
      end if;
    end if;
  end process;

end Behavioral;

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Component Level Testbench
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library vunit_lib;
  context vunit_lib.vunit_context;

entity tb_StdFifo is
  generic (
    runner_cfg  : runner_cfg_t;
    kDataWidth  : positive := 8;
    kFifoDepth  : positive := 256
    );
end tb_StdFifo;

architecture behavior of tb_StdFifo is

  signal StopSim : boolean := false;

  --Inputs
  signal Clk      : std_logic := '0';
  signal aReset   : std_logic := '0';
  signal cDataIn  : std_logic_vector(kDataWidth-1 downto 0) := (others => '0');
  signal cReadEn  : std_logic := '0';
  signal cWriteEn : std_logic := '0';

  --Outputs
  signal cDataOut : std_logic_vector(kDataWidth-1 downto 0);
  signal cEmpty   : std_logic;
  signal cFull    : std_logic;

  -- Clock period definitions
  constant kClkPeriod : time := 10 ns;

  procedure ClkWait(
    X : integer := 1;
    signal Clk : std_logic) is
  begin -- procedure ClkWait
    for i in 1 to X loop
      wait until rising_edge(Clk);
    end loop;
  end procedure ClkWait;

begin

  -- Instantiate the Design Under Test (DUT)
  DUT: entity work.StdFifo(Behavioral)
    generic map(
      kDataWidth  => kDataWidth,
      kFifoDepth  => kFifoDepth)
    port map (
      Clk      => Clk,
      aReset   => aReset,
      cWriteEn => cWriteEn,
      cDataIn  => cDataIn,
      cReadEn  => cReadEn,
      cDataOut => cDataOut,
      cEmpty   => cEmpty,
      cFull    => cFull
    );

  Clk <= not Clk after kClkPeriod / 2 when not StopSim else
    '0';

  MainTest: process

    procedure PushDataIntoFifo(
      DataIn : std_logic_vector(kDataWidth-1 downto 0);
      PushBackToBack : boolean := false) is
    begin -- procedure PushDataIntoFifo
      if not cFull then
        cDataIn  <= DataIn;
        cWriteEn <= '1';
        ClkWait(1,Clk);
        wait for 0 ns;
      else
        cWriteEn <= '0';
        ClkWait(1,Clk);
      end if;
      if not PushBackToBack then
        cWriteEn <= '0';
        ClkWait(1,Clk);
      end if;
    end procedure PushDataIntoFifo;

    procedure ReadDataFromFifo(
      DataOut : out std_logic_vector(kDataWidth-1 downto 0);
      ReadBackToBack : boolean := false) is
    begin -- procedure ReadDataFromFifo
      if not cEmpty then
        cReadEn <= '1';
        ClkWait(1,Clk);
        wait for 0 ns;
        DataOut := cDataOut;
      else
        cReadEn <= '0';
        ClkWait(1,Clk);
      end if;
      if not ReadBackToBack then
        cReadEn <= '0';
        ClkWait(1,Clk);
      end if;
    -- wait for 0 ns;
    end procedure ReadDataFromFifo;

  variable ReadData : std_logic_vector(kDataWidth-1 downto 0);
  begin -- process MainTest
    test_runner_setup(runner, runner_cfg);

    aReset <= '1';
    wait for kClkPeriod * 2;
    aReset <= '0';
    wait for kClkPeriod * 2;

    -- Run each test in a separate simulation
    while test_suite loop
      if run("SimpleFifoTest") then
        for i in 0 to 9 loop
          PushDataIntoFifo(std_logic_vector(to_unsigned(i,kDataWidth)));
        end loop;

        for i in 0 to 9 loop
          ReadDataFromFifo(ReadData);
          assert ReadData = std_logic_vector(to_unsigned(i,kDataWidth))
            report "ReadData is different from the expected result" & LF &
            "Expected: " & Image(std_logic_vector(std_logic_vector(to_unsigned(
              i,kDataWidth)))) & LF &
            "Received: " & Image(std_logic_vector(ReadData))
            severity error;
        end loop;


      elsif run("FullAndEmptyTest") then
        assert cEmpty = '1'
          report "FIFO was not empty at the beginning of the test." & LF &
          "Expected: 1" & LF &
          "Received: " & std_logic'Image(cEmpty)
          severity error;

        for i in 0 to kFifoDepth-1 loop
          PushDataIntoFifo(
            DataIn         => std_logic_vector(to_unsigned(i,kDataWidth)),
            PushBackToBack => i /= kFifoDepth-1);
        end loop;

        assert cFull = '1'
          report "FIFO is not full." & LF &
          "Expected: '1'"& LF &
          "Received: " & std_logic'Image(cFull)
          severity error;

        assert cEmpty = '0'
          report "FIFO Empty flag was asserted with a full FIFO." & LF &
          "Expected: '0'" & LF &
          "Received: " & std_logic'Image(cEmpty)
          severity error;

        for i in 0 to kFifoDepth-1 loop
          ReadDataFromFifo(
            DataOut        => ReadData,
            ReadBackToBack => i /= kFifoDepth-1);
          assert ReadData = std_logic_vector(to_unsigned(i,kDataWidth))
            report "ReadData is different from the expected result" & LF &
            "Expected: " & Image(std_logic_vector(std_logic_vector(to_unsigned(
              i,kDataWidth)))) & LF &
            "Received: " & Image(std_logic_vector(ReadData))
            severity error;
        end loop;

        assert cEmpty = '1'
          report "FIFO was not empty at the end of the test." & LF &
          "Expected: '1'"& LF &
          "Received: " & std_logic'Image(cEmpty)
          severity error;

        assert cFull = '0'
          report "FIFO Full flag was asserted with an empty FIFO." & LF &
          "Expected: '0'"& LF &
          "Received: " & std_logic'Image(cFull)
          severity error;

      elsif run("ReadAndWriteTest") then
        for i in 0 to (kFifoDepth/2)-1 loop
          PushDataIntoFifo(
            DataIn         => std_logic_vector(to_unsigned(i,kDataWidth)),
            PushBackToBack => false);
        end loop;

        for i in 0 to (kFifoDepth/2)-1 loop
          PushDataIntoFifo(
            DataIn         => std_logic_vector(to_unsigned(kFifoDepth/2 + i,
              kDataWidth)),
            PushBackToBack => false);
          ReadDataFromFifo(
            DataOut        => ReadData,
            ReadBackToBack => false);
          assert ReadData = std_logic_vector(to_unsigned(i,kDataWidth))
            report "ReadData is different from the expected result" & LF &
            "Expected: " & Image(std_logic_vector(std_logic_vector(to_unsigned(
              i,kDataWidth)))) & LF &
            "Received: " & Image(std_logic_vector(ReadData))
            severity error;
        end loop;

        assert cEmpty = '0'
          report "FIFO Empty flag was asserted with a full FIFO." & LF &
          "Expected: '0'" & LF &
          "Received: " & std_logic'Image(cEmpty)
          severity error;

        for i in 0 to (kFifoDepth/2)-1 loop
          ReadDataFromFifo(
            DataOut        => ReadData,
            ReadBackToBack => false);
          assert ReadData = std_logic_vector(to_unsigned((kFifoDepth/2 + i),
            kDataWidth))
            report "ReadData is different from the expected result" & LF &
            "Expected: " & Image(std_logic_vector(std_logic_vector(to_unsigned(
              (kFifoDepth/2 + i),kDataWidth)))) & LF &
            "Received: " & Image(std_logic_vector(ReadData))
            severity error;
        end loop;

        assert cEmpty = '1'
          report "FIFO was not empty at the end of the test." & LF &
          "Expected: '1'"& LF &
          "Received: " & std_logic'Image(cEmpty)
          severity error;
      end if;
    end loop;

    test_runner_cleanup(runner); -- Simulation ends here
    StopSim <= true;
    wait;
  end process MainTest;

end;
