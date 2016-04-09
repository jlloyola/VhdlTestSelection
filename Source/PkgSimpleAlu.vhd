-------------------------------------------------------------------------------
--
-- File: PkgSimpleAlu.vhd
-- This package contains the values for the ALU opcodes
--
-------------------------------------------------------------------------------

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

package PkgSimpleAlu is

  -- Defines the size of the OpCode vector. The number of operations is
  -- 2**kOpCodeSize.
  constant kOpCodeSize : natural := 2;
  -- Define the value of each OpCode
  constant kSumOp     : std_logic_vector(kOpCodeSize-1 downto 0) := "00";
  constant kSubractOp : std_logic_vector(kOpCodeSize-1 downto 0) := "01";
  constant kAndOp     : std_logic_vector(kOpCodeSize-1 downto 0) := "10";
  constant kOrOp      : std_logic_vector(kOpCodeSize-1 downto 0) := "11";

end package PkgSimpleAlu;