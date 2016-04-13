-------------------------------------------------------------------------------
-- Purpose:
-- This file contains the time constants used in the AD7685 model.
-------------------------------------------------------------------------------
library ieee;
  use ieee.std_logic_1164.all;

package PkgAD7685TimeConstants is
  -----------------------------------------------------------------------------
  -- Timing Constants (VDD > 4.5)
  -----------------------------------------------------------------------------
  constant kCNVPulseWidth        : time := 10  ns;
  constant kMaxConvTime          : time := 2.2 us;
  constant kMinConvTime          : time := 0.5 us;
  constant kCNV_LowToMSB         : time := 15  ns;
  constant kSDO_ValidAfterFE     : time := 5   ns;
  constant kSCK_FE_toDataValid   : time := 14  ns;
  constant kSCK_LowTime          : time := 7   ns;
  constant kSCK_HighTime         : time := 7   ns;
  constant kSCK_MinPeriod        : time := 15  ns;
  constant kSDO_toHighZ          : time := 25  ns;
  constant kTimeBetweenConv      : time := 4   us;
  constant kAcquisitionTime      : time := 1.8 us;
  constant kBrdClkPeriod         : time := 25  ns;
  constant kHalfBrdClkPeriod     : time := kBrdClkPeriod / 2;


  -----------------------------------------------------------------------------
  -- Timing Constants as type positive
  -----------------------------------------------------------------------------
  -- All constants are in ns
  constant kCNVPulseWidthPositive_t        : positive := 10;   --ns
  constant kMaxConvTimePositive_t          : positive := 2200; --ns
  constant kMinConvTimePositive_t          : positive := 500;  --ns
  constant kCNV_LowToMSBPositive_t         : positive := 15;   --ns
  constant kSDO_ValidAfterFEPositive_t     : positive := 5;    --ns
  constant kSCK_FE_toDataValidPositive_t   : positive := 14;   --ns
  constant kSCK_LowTimePositive_t          : positive := 7;    --ns
  constant kSCK_HighTimePositive_t         : positive := 7;    --ns
  constant kSCK_MinPeriodPositive_t        : positive := 15;   --ns
  constant kSDO_toHighZPositive_t          : positive := 25;   --ns
  constant kBrdClkPeriodPositive_t         : positive := 25;   --ns
  constant kTimeBetweenConvPositive_t      : positive := 4000; --ns
  constant kAcquisitionTimePositive_t      : positive := 1800; --ns

end PkgAD7685TimeConstants;
