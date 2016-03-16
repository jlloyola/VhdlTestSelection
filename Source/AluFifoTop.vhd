-------------------------------------------------------------------------------
--
-- File: AluFifoTop.vhd
--
-------------------------------------------------------------------------------
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity AluFifoTop is
  generic (
    kDataLength : positive := 8;
    kFifoDepth  : positive := 8
  );
  port (
    Clk          : in  std_logic;
    aReset       : in  std_logic;
    -- Asserts if the result of the last operation was 0
    cZeroFlag    : in  std_logic;
    -- Asserts if the result of the last operation has carry
    cCarry       : in  std_logic;
    cOverflow    : out boolean;
    cResultValid : out boolean;
    -- FIFO interface
    cWriteEn     : in  std_logic;
    cDataIn      : in  std_logic_vector (kDataLength - 1 downto 0);
    cReadEn      : in  std_logic;
    cDataOut     : out std_logic_vector (kDataLength - 1 downto 0);
    cEmpty       : out std_logic;
    cFull        : out std_logic
  );

end entity AluFifoTop;

architecture rtl of AluFifoTop is

  signal cOpCode : std_logic_vector(1 downto 0);
  signal cDataA  : unsigned(kDataLength - 1 downto 0);
  signal cDataB  : unsigned(kDataLength - 1 downto 0);
  signal cPushData : boolean;
  signal cResult : unsigned(kDataLength - 1 downto 0);

  constant kFifoDataLength : positive := (kDataLength*2 + cOpCode'length);

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
      cPushData    => cPushData,
      cResult      => cResult,
      cResultValid => cResultValid,
      cOverflow    => cOverflow);

  Fifo: entity work.StdFifo(Behavioral)
    generic map(
      kDataWidth  => kFifoDataLength,
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

end architecture rtl;