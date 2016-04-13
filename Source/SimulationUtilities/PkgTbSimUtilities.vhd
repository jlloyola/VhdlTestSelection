-------------------------------------------------------------------------------
--
-- File: PkgTbSimUtilities.vhd
-- This package contains different functions and procedures that are useful
-- for testbenches.
--
-------------------------------------------------------------------------------

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;
  use ieee.math_real.all;
  use std.textio.all;

package PkgTbSimUtilities is

  procedure ClkWait(
    X : integer := 1;
    signal Clk : std_logic);

  function Image(Arg : std_logic_vector) return string;
  function Image(Arg : std_logic) return string;
  function Image(Arg : unsigned) return string;

  function to_Boolean(s: std_logic) return boolean;
  function to_StdLogic(b : boolean) return std_logic;

  function RandReal(UpperLim : real := 1.0) return real;
  function RandNatural(LowerLim, UpperLim : natural) return natural;
  function RandSlv(Size : positive) return std_logic_vector;
  function RandUnsigned(Size : positive) return unsigned;

  function Zeros(Size : positive) return std_logic_vector;

end package PkgTbSimUtilities;

package body PkgTbSimUtilities is

  procedure ClkWait(
    X : integer := 1;
    signal Clk : std_logic) is
  begin -- procedure ClkWait
    for i in 1 to X loop
      wait until rising_edge(Clk);
    end loop;
  end procedure ClkWait;

  function Image(Arg : std_logic_vector) return string is
    variable x : line;
    variable rval : string(1 to Arg'length);
  begin
    rval := x.all;
    deallocate(x);
    return rval;
  end Image;

  function Image(Arg : unsigned) return string is
  begin
    return Image(std_logic_vector(Arg));
  end Image;

  function Image(Arg : std_logic) return string is
    variable x : line;
    variable rval : string(1 to 1);
  begin
    rval := x.all;
    deallocate(x);
    return rval;
  end Image;

  function to_StdLogic(b : boolean) return std_logic is
  begin
    if b then
      return '1';
    else
      return '0';
    end if;
  end to_StdLogic;

  function to_Boolean(s: std_logic) return boolean is
  begin
    return to_x01(s) = '1';
  end function to_Boolean;

  function RandReal(UpperLim : real := 1.0) return real is
    variable seed1, seed2 : positive;
    variable randVal : real;
  begin -- function RandReal
    uniform(seed1, seed2, randVal);
    return randVal * UpperLim;
  end function RandReal;

  function RandNatural(LowerLim, UpperLim : natural) return natural is
  begin -- function RandNatural
    return integer(RandReal(real(UpperLim - LowerLim + 1)) + real(LowerLim));
  end function;

  function RandSlv(Size : positive) return std_logic_vector is
  begin -- function RandSlv
    return std_logic_vector(to_unsigned(RandNatural(0, ((2**Size)-1)), Size));
  end function;

  function RandUnsigned(Size : positive) return unsigned is
  begin -- function RandUnsigned
    return unsigned(RandSlv(Size));
  end function;

  function Zeros(Size : positive) return std_logic_vector is
    variable RetVal : std_logic_vector(Size-1 downto 0) := (others => '0');
  begin -- function Zeros
    return RetVal;
  end function Zeros;

end package body PkgTbSimUtilities;