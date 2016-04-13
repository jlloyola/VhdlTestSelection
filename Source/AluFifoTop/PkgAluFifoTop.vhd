-------------------------------------------------------------------------------
--
-- File: PkgAluFifoTop.vhd
-- This package contains the configuration constants for the AluFifoTop module
--
-------------------------------------------------------------------------------

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

library lib;
  use lib.PkgSimpleAlu.all;

package PkgAluFifoTop is

  -- Defines the size depth of the FIFO
  constant kFifoDepth  : natural := 8;
  -- Defines the size of the ALU data
  constant kDataLength : natural := 8;
  constant kFifoDataLength : positive := (kDataLength*2 + kOpCodeSize);
  -- Defines how the data is ordered in a FIFO word
  constant kDataB_Lsb : natural := 0;
  constant kDataB_Msb : natural := kDataLength-1;
  constant kDataA_Lsb : natural := kDataB_Msb+1;
  constant kDataA_Msb : natural := kDataA_Lsb+kDataLength-1;
  constant kOpCodeLsb : natural := kDataA_Msb+1;
  constant kOpCodeMsb : natural := kOpCodeLsb+kOpCodeSize-1;

  constant kResultZero : unsigned(kDataLength - 1 downto 0) := (others => '0');

  function BuildFifoPacket(
    OpCode : std_logic_vector(kOpCodeSize-1 downto 0);
    DataA  : std_logic_vector(kDataLength-1 downto 0);
    DataB  : std_logic_vector(kDataLength-1 downto 0))
  return std_logic_vector;


end package PkgAluFifoTop;

package body PkgAluFifoTop is

  function BuildFifoPacket(
    OpCode : std_logic_vector(kOpCodeSize-1 downto 0);
    DataA  : std_logic_vector(kDataLength-1 downto 0);
    DataB  : std_logic_vector(kDataLength-1 downto 0))
  return std_logic_vector is
    variable Packet : std_logic_vector(kFifoDataLength-1 downto 0);
  begin -- function BuildFifoPacket
    Packet(kDataA_Msb downto kDataA_Lsb) := DataA;
    Packet(kDataB_Msb downto kDataB_Lsb) := DataB;
    Packet(kOpCodeMsb downto kOpCodeLsb) := OpCode;
    return Packet;
  end function BuildFifoPacket;

end package body;