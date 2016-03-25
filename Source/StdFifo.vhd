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
    if rising_edge(Clk) then
      if aReset = '1' then
        Head := 0;
        Tail := 0;
        Looped := false;
        cFull  <= '0';
        cEmpty <= '1';
      else
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
          cEmpty  <= '0';
          cFull  <= '0';
        end if;
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
    runner_cfg  : runner_cfg_t
    );
end tb_StdFifo;

architecture behavior of tb_StdFifo is

  -- Component Declaration for the Unit Under Test (UUT)
  component StdFifo
    Generic (
      constant kDataWidth  : positive := 8;
      constant kFifoDepth  : positive := 256
    );
    port (
      Clk      : in  std_logic;
      aReset   : in  std_logic;
      cWriteEn : in  std_logic;
      cDataIn  : in  std_logic_vector (kDataWidth - 1 downto 0);
      cReadEn  : in  std_logic;
      cDataOut : out std_logic_vector (kDataWidth - 1 downto 0);
      cEmpty   : out std_logic;
      cFull    : out std_logic
    );
  end component;

  signal StopSim : boolean := false;

  --Inputs
  signal Clk    : std_logic := '0';
  signal aReset    : std_logic := '0';
  signal cDataIn  : std_logic_vector(7 downto 0) := (others => '0');
  signal cReadEn  : std_logic := '0';
  signal cWriteEn  : std_logic := '0';

  --Outputs
  signal cDataOut  : std_logic_vector(7 downto 0);
  signal cEmpty  : std_logic;
  signal cFull    : std_logic;

  -- Clock period definitions
  constant kClkPeriod : time := 10 ns;

begin

  -- Instantiate the Unit Under Test (UUT)
  uut: StdFifo
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
  begin -- process MainTest
    test_runner_setup(runner, runner_cfg);

    -- Run each test in a separate simulation
    -- while test_suite loop
      -- if run("SimpleAddTest") then
        -- TestAluOp(
          -- Operation => "00", -- Sum
          -- DataA     => to_unsigned(3, kDataLength),
          -- DataB     => to_unsigned(2, kDataLength));

      -- elsif run("SimpleSubtractTest") then
        -- TestAluOp(
          -- Operation => "01", -- Subtract
          -- DataA     => to_unsigned(3, kDataLength),
          -- DataB     => to_unsigned(2, kDataLength));

      -- elsif run("SimpleAndTest") then
        -- TestAluOp(
          -- Operation => "10", -- And
          -- DataA     => to_unsigned(10, kDataLength),
          -- DataB     => to_unsigned(3, kDataLength));

      -- elsif run("SimpleOrTest") then
        -- TestAluOp(
          -- Operation => "11", -- Or
          -- DataA     => to_unsigned(10, kDataLength),
          -- DataB     => to_unsigned(5, kDataLength));
      -- end if;
    -- end loop;

    test_runner_cleanup(runner); -- Simulation ends here
    StopSim <= true;
    wait;
  end process MainTest;

  -- Reset process
  aReset_proc : process
  begin
  wait for kClkPeriod * 5;

    aReset <= '1';

    wait for kClkPeriod * 5;

    aReset <= '0';

    wait;
  end process;

  -- Write process
  wr_proc : process
    variable counter : unsigned (7 downto 0) := (others => '0');
  begin
    wait for kClkPeriod * 20;

    for i in 1 to 32 loop
      counter := counter + 1;

      cDataIn <= std_logic_vector(counter);

      wait for kClkPeriod * 1;

      cWriteEn <= '1';

      wait for kClkPeriod * 1;

      cWriteEn <= '0';
    end loop;

    wait for kClkPeriod * 20;

    for i in 1 to 32 loop
      counter := counter + 1;

      cDataIn <= std_logic_vector(counter);

      wait for kClkPeriod * 1;

      cWriteEn <= '1';

      wait for kClkPeriod * 1;

      cWriteEn <= '0';
    end loop;

    wait;
  end process;

  -- Read process
  rd_proc : process
  begin
    wait for kClkPeriod * 20;

    wait for kClkPeriod * 40;

    cReadEn <= '1';

    wait for kClkPeriod * 60;

    cReadEn <= '0';

    wait for kClkPeriod * 256 * 2;

    cReadEn <= '1';

    wait;
  end process;

END;
