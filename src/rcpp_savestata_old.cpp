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

#include <Rcpp.h>
#include <string>
#include <fstream>
#include <stdint.h>
#include "statadefines.h"
#include "swap_endian.h"

using namespace Rcpp;
using namespace std;

template <typename T>
static void writebin(T t, fstream& dta, bool swapit)
{
  if (swapit==1){
    T t_s = swap_endian(t);
    dta.write((char*)&t_s, sizeof(t_s));
  } else {
    dta.write((char*)&t, sizeof(t));
  }
}

//' Writes the binary Stata file
//'
//' @param filePath The full systempath to the dta file you want to export.
//' @param dat an R-Object of class data.frame.
//' @export
// [[Rcpp::export]]
int stataWriteOld(const char * filePath, Rcpp::DataFrame dat)
{

  // This bool was inteded to do a swap if you want to create a MSF-File on a
  // LSF-machine. By default it should be 0 (no byteswap).
  // bool swapit = strcmp(byteorder, SBYTEORDER);
  bool swapit = 0;

  uint16_t k = dat.size();
  uint32_t n = dat.nrows();
  int8_t byteorder = 2;

  string timestamp = dat.attr("timestamp");
  timestamp.resize(18);
  string datalabel = dat.attr("datalabel");
  datalabel.resize(81);

  CharacterVector valLabels = dat.attr("vallabels");
  CharacterVector nvarnames = dat.attr("names");

  List chs = dat.attr("expansion.fields");
  List formats = dat.attr("formats");
  List labeltable = dat.attr("label.table");
  List varLabels = dat.attr("var.labels");
  List vartypes = dat.attr("types");


  int8_t version = as<int>(dat.attr("version"));


  fstream dta (filePath, ios::out | ios::binary);
  if (dta.is_open())
  {

    int formatsize = 49;
    int varnamesize = 32;
    int maxdatalabelsize = 82;
    int vallabelsize = 32;
    int varlabelsize = 81;

    switch(version)
    {
    case 105:
      maxdatalabelsize = 32;
      formatsize = 12;
      varnamesize = 8;
      vallabelsize = 8;
      varlabelsize = 32;
      break;
    case 108:
      formatsize = 12;
      varnamesize = 8;
      vallabelsize = 8;
    case 110:
      formatsize = 12;
      break;
    case 111:
      formatsize = 12;
      break;
    case 113:
      formatsize = 12;
      break;
    }

    writebin(version, dta, swapit);
    writebin(byteorder, dta, swapit); // LSF
    int8_t ft = 1;
    writebin(ft, dta, swapit);
    int8_t unused = 0;
    writebin(unused, dta, swapit);
    writebin(k, dta, swapit);
    writebin(n, dta, swapit);

    /* write a datalabel */
    if (datalabel.size()>maxdatalabelsize)
      datalabel.resize(maxdatalabelsize);

    dta.write(datalabel.c_str(),datalabel.size());


    /* timestamp size is 17 */
    dta.write(timestamp.c_str(),timestamp.size());

    /* <variable_types> ... </variable_types> */
    uint8_t  nvartype;
    for (uint16_t i = 0; i < k; ++i)
    {
      nvartype = as<uint8_t>(vartypes[i]);
      if(version<111)
      {
        char c[2];

        switch(nvartype)
        {
        case 255:
          strcpy(c,"d");
          c[1] = '\0';
          dta.write(c, 1);
          break;
        case 254:
          strcpy(c,"f");
          c[1] = '\0';
          dta.write(c, 1);
          break;
        case 253:
          strcpy(c,"l");
          c[1] = '\0';
          dta.write(c, 1);
          break;
        case 252:
          strcpy(c,"i");
          c[1] = '\0';
          dta.write(c, 1);
          break;
        case 251:
          strcpy(c,"b");
          c[1] = '\0';
          dta.write(c, 1);
          break;
        default:
          char d = char(nvartype+127);
        dta.write(&d, 1);
        break;
        }
      }
      else
        writebin(nvartype, dta, swapit);
    }

    /* <varnames> ... </varnames> */
    for (uint16_t i = 0; i < k; ++i )
    {
      string nvarname = as<string>(nvarnames[i]);
      nvarname.resize(varnamesize);

      dta.write(nvarname.c_str(),nvarname.size()+1);
    }

    /* <sortlist> ... </sortlist> */
    uint32_t big_k = k+1;

    for (uint32_t i = 0; i < big_k; ++i)
    {
      uint16_t nsortlist = 0;
      writebin(nsortlist, dta, swapit);
    }

    /* <formats> ... </formats> */
    for (uint16_t i = 0; i < k; ++i )
    {
      string nformats = as<string>(formats[i]);
      nformats.resize(formatsize);

      dta.write(nformats.c_str(),nformats.size());
    }

    /* <value_label_names> ... </value_label_names> */
    for (uint16_t i = 0; i < k; ++i )
    {
      string nvalLabels = as<string>(valLabels[i]);
      nvalLabels.resize(vallabelsize);

      dta.write(nvalLabels.c_str(),nvalLabels.size()+1);
    }

    /* <variable_labels> ... </variable_labels> */
    for (uint16_t i = 0; i < k; ++i)
    {
      string nvarLabels = "";
      if (!Rf_isNull(varLabels) && Rf_length(varLabels) > 1)
      {
        nvarLabels = as<std::string>(varLabels[i]);
      }
      dta.write(nvarLabels.c_str(),varlabelsize);
    }


    /* <characteristics> ... </characteristics> */

    int8_t datatype = 0;
    uint32_t len = 0;

    if (chs.size()>0){
      for (int32_t i = 0; i<chs.size(); ++i){

        CharacterVector ch = as<CharacterVector>(chs[i]);

        string ch1 = as<string>(ch[0]);
        ch1[ch1.size()] = '\0';
        string ch2 = as<string>(ch[1]);
        ch2[ch2.size()] = '\0';
        string ch3 = as<string>(ch[2]);
        ch3[ch3.size()] = '\0';

        len = 33 + 33 + ch3.size()+1;
        datatype = 1;

        writebin(datatype, dta, swapit);
        if(version<=108)
          writebin((int16_t)len, dta, swapit);
        else
          writebin(len, dta, swapit);

        dta.write(ch1.c_str(),33);
        dta.write(ch2.c_str(),33);
        dta.write(ch3.c_str(),ch3.size()+1);

      }
    }

    // five bytes of zero end characteristics
    datatype = 0;
    len = 0;
    writebin(datatype, dta, swapit);
    if (version<=108)
      writebin((int16_t)len, dta, swapit);
    else
      writebin(len, dta, swapit);

    /* <data> ... </data> */

    for(uint32_t j = 0; j < n; ++j)
    {
      for (uint16_t i = 0; i < k; ++i)
      {
        int const type = vartypes[i];
        switch(type)
        {
          // store numeric as Stata double (double)
        case 255:
        {
          double val_d = as<NumericVector>(dat[i])[j];

          if ( (val_d == NA_REAL) | R_IsNA(val_d) )
            val_d = STATA_DOUBLE_NA;

          writebin(val_d, dta, swapit);

          break;
        }
          // float
        case 254:
        {
          double val_d = as<NumericVector>(dat[i])[j];
          float val_f = (float)(val_d);

          if ((val_d == NA_REAL) | (R_IsNA(val_d)) )
            val_f = STATA_FLOAT_NA;

          writebin(val_f, dta, swapit);

          break;
        }
          // store integer as Stata long (int32_t)
        case 253:
        {
          int32_t val_l = as<IntegerVector>(dat[i])[j];

          if ( (val_l == NA_INTEGER) | (R_IsNA(val_l)) )
          {
            if(version>111)
              val_l = STATA_INT_NA;
            else
              val_l = STATA_INT_NA_108;
          }

          writebin(val_l, dta, swapit);

          break;
        }
          // int
        case 252:
        {
          union v {
            int32_t   l;
            int16_t   i;
          } val;

          val.l = as<IntegerVector>(dat[i])[j];

          int16_t val_i = val.i;

          if (val.l == NA_INTEGER)
            val_i = STATA_SHORTINT_NA;

          writebin(val_i, dta, swapit);

          break;
        }
          // byte
        case 251:
        {
          union v {
            int32_t   l;
            int8_t    b;
          } val;

          val.l = as<IntegerVector>(dat[i])[j];

          int8_t val_b = val.b;

          if (val.l == NA_INTEGER)
            val_b = STATA_BYTE_NA;

          writebin(val_b, dta, swapit);

          break;
        }
        default:
        {
          int32_t len = vartypes[i];
          /* FixMe: Storing the vector in b for each string. */
          CharacterVector b = as<CharacterVector>(dat[i]);
          string val_s = as<string>(b[j]);
          // Stata 6-12 can only store 244 byte strings
          if(val_s.size()>244)
          {
            val_s.resize(244);
            len = 244;
          }
          dta.write(val_s.c_str(),len);
          break;
        }

        }
      }
    }


    /* <value_labels> ... </value_labels> */
    if (labeltable.size()>0 & version>105)
    {

      CharacterVector labnames = labeltable.attr("names");
      int8_t padding = 0;

      for (int32_t i=0; i < labnames.size(); ++i)
      {
        int32_t txtlen = 0;

        string labname = as<string>(labnames[i]);
        IntegerVector labvalue = labeltable[labname];
        int32_t N = labvalue.size();
        CharacterVector labelText = labvalue.attr("names");
        IntegerVector off;

        /*
        * Fill off with offset position and create txtlen
        */

        for (int32_t i = 0; i < labelText.size(); ++i)
        {
          string label = as<string>(labelText[i]);
          int32_t labellen = label.size()+1;
          txtlen += labellen;
          off.push_back ( txtlen-labellen );
        }

        int32_t offI, labvalueI;

        int32_t nlen = sizeof(N) + sizeof(txtlen) + sizeof(offI)*N + sizeof(labvalueI)*N + txtlen;

        writebin(nlen, dta, swapit);

        if (version>108)
          labname.resize(32);
        else
          labname.resize(8);

        dta.write(labname.c_str(),labname.size()+1);
        dta.write((char*)&padding,3);
        writebin(N, dta, swapit);
        writebin(txtlen, dta, swapit);

        for (int32_t i = 0; i < N; ++i)
        {
          offI = off[i];
          writebin(offI, dta, swapit);
        }

        for (int32_t i = 0; i < N; ++i)
        {
          labvalueI = labvalue[i];
          writebin(labvalueI, dta, swapit);
        }

        for (int32_t i = 0; i < N; ++i)
        {
          string labtext = as<string>(labelText[i]);
          labtext[labtext.size()] = '\0';
          dta.write(labtext.c_str(),labtext.size()+1);
        }
      }

    }

    dta.close();
    return 0;
  }
  else {
    throw std::range_error("Unable to open file.");
    return -1;
  }
}
