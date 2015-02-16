/*
 * Copyright (C) 2015 Jan Marvin Garbuszus and Sebastian Jeworutzki
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef STATADEFINES
#define STATADEFINES


/* Test for a little-endian machine */
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define sbyteorder "LSF"
#define SBYTEORDER 2
#else
#define sbyteorder "MSF"
#define SBYTEORDER 1
#endif

/*Define missings*/
#define STATA_BYTE_NA_MIN -127
#define STATA_BYTE_NA_MAX +100
#define STATA_BYTE_NA +101
#define STATA_SHORTINT_NA_MIN -32767
#define STATA_SHORTINT_NA_MAX +32740
#define STATA_SHORTINT_NA +32741
#define STATA_INT_NA_MIN -2147483647
#define STATA_INT_NA_MAX +2147483620
#define STATA_INT_NA +2147483621
#define STATA_FLOAT_NA_MAX (1+15/pow(16,1)+15/pow(16,2)+15/pow(16,3)+15/pow(16,4)+15/pow(16,5)+14/pow(16,6))*pow(2,126)
#define STATA_FLOAT_NA_MIN -STATA_FLOAT_NA_MAX
#define STATA_FLOAT_NA pow(2,126)
#define STATA_DOUBLE_NA_MAX (1+15/pow(16,1)+15/pow(16,2)+15/pow(16,3)+15/pow(16,4)+15/pow(16,5)+15/pow(16,6)+15/pow(16,7)+15/pow(16,8)+15/pow(16,9)+15/pow(16,10)+15/pow(16,11)+15/pow(16,12)+15/pow(16,13))*pow(2,1022)
#define STATA_DOUBLE_NA_MIN -1*(1+15/pow(16,1)+15/pow(16,2)+15/pow(16,3)+15/pow(16,4)+15/pow(16,5)+15/pow(16,6)+15/pow(16,7)+15/pow(16,8)+15/pow(16,9)+15/pow(16,10)+15/pow(16,11)+15/pow(16,12)+15/pow(16,13))*pow(2,1023)
#define STATA_DOUBLE_NA pow(2,1023)

#define STATA_BYTE 65530
#define STATA_SHORTINT 65529
#define STATA_INT 65528
#define STATA_FLOAT 65527
#define STATA_DOUBLE 65526

#define STATA_STRL 32768


#endif
