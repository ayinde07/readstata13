/*
 * Copyright (C) 2014-2015 Jan Marvin Garbuszus and Sebastian Jeworutzki
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
#include <string.h>
#include <stdint.h>
#include "statadefines.h"
#include "swap_endian.h"
#include "readstata.h"

using namespace Rcpp;
using namespace std;


// Reads the binary Stata file
//
// @param filePath The full systempath to the dta file you want to import.
// @param missing logical if missings should be converted outside of Rcpp.
// @import Rcpp
// @export
// [[Rcpp::export]]
List stata(const char * filePath, const bool missing)
{
  FILE *file = NULL;    // File pointer

  /*
  * Open the file in binary mode using the "rb" format string
  * This also checks if the file exists and/or can be opened for reading correctly
  */

  if ((file = fopen(filePath, "rb")) == NULL)
    Rcpp::stop("Could not open specified file.");

  /*
  * check the first byte. continue if "<"
  */

  char fbit[2];
  readstr(fbit, file, sizeof(fbit));

  char expfbit[2] = "<";
  expfbit[1] = '\0';

  int8_t stataversion = 117;

  if (strcmp(fbit,expfbit)!=0)
  {
    // restart from the beginning. Maybe a Stata 12, 10 or 8 file.
    rewind(file);
    stataversion = readbin(stataversion, file, 0);

    if (stataversion==117)
      Rcpp::stop("First byte: Not a file we can read.");
  }

  char version [4];

  IntegerVector versionIV(1);
  if (stataversion==117)
  {
    fseek(file, 18, SEEK_CUR);// stata_dta><header>
    test("<release>", file);

    /*
    * release is a 4 byte character e.g. "117"
    */

    int8_t gversion = 117L; //g = good

    readstr(version, file, sizeof(version));

    versionIV(0) = atoi(version);

    // check the release version. continue if "117"
    if (gversion!=atoi(version))
      Rcpp::stop("Version: Not a version 13 dta-file.");

    fseek(file, 10, SEEK_CUR); // </release>
    test("<byteorder>", file);
  } else {
    if (stataversion<102 | stataversion>115)
      Rcpp::stop("File appears to be of unsupported Stata format.");
    versionIV(0) = stataversion;
  }

  /*
  * byteorder is a 4 byte character e.g. "LSF". MSF referes to big-memory data.
  */


  bool swapit;

  CharacterVector byteorderC(1);
  IntegerVector byteorderI(1);

  switch (stataversion)
  {
  case 117:
  {
    char byteorder [4];
    readstr(byteorder,file, sizeof(byteorder));

    fseek(file, 12, SEEK_CUR); // </byteorder>
    test("<K>", file);

    swapit = strcmp(byteorder, sbyteorder);
    byteorderC(0) = byteorder;
    break;
  }

  default:
  {
    int8_t byteorder = 0;
    byteorder = readbin(byteorder, file, 0);
    // 1 = MSF 2 = LSF
    swapit = std::abs(SBYTEORDER-byteorder);
    byteorderI(0) = byteorder;

    // filetype: unnown?
    int8_t ft = 0;
    ft = readbin(ft, file, swapit);

    int8_t unused = 0;
    unused = readbin(unused, file, swapit);
    break;
  }
  }

  /*
  * Number of Variables
  */

  uint16_t k = 0;
  switch (stataversion)
  {

  case 117:
    k = readbin(k, file, swapit);
    fseek(file, 4, SEEK_CUR); //</K>
    test("<N>", file);
    break;

  default:
    k = readbin(k, file, swapit);
  break;
  }

  /*
  * Number of Observations
  */

  uint32_t n = 0;

  switch (stataversion)
  {

  case 117:
    n = readbin(n, file, swapit);
    fseek(file, 4, SEEK_CUR); //</N>
    test("<label>", file);
    break;

  default:
    n = readbin(n, file, swapit);
  break;
  }

  /*
  * A dataset may have a label e.g. "Written by R".
  * First we read its length (ndlabel), later the actual label (datalabel).
  * ndlabel:   length of datalabel (excl. binary 0)
  * datalabel: string max length 80
  */

  //   if (stataversion==stataversion)
  //     throw std::range_error("Debug");

  uint8_t ndlabel = 0;
  CharacterVector datalabelCV(1);

  switch (stataversion)
  {

  case 102:
  {
    char datalabel[31];
    readstr(datalabel, file, 31);
    datalabelCV(0) = datalabel;
    break;
  }

  case 103:
  case 104:
  case 105:
  case 106:
  {
    char datalabel[33];
    readstr(datalabel, file, 33);
    datalabelCV(0) = datalabel;
    break;
  }

  case 107:
  case 108:
  case 110:
  case 111:
  case 112:
  case 113:
  case 114:
  case 115:
  {
    char datalabel[82];
    readstr(datalabel, file, 82);
    datalabelCV(0) = datalabel;
    break;
  }

  case 117:
  {
    ndlabel = readbin(ndlabel, file, swapit);
    // FixMe: 117 ndlabel max length is 80. Use 113/115 default char length?
    char *datalabel = new char[ndlabel+1];
    if (ndlabel>0)
    {
      readstr(datalabel, file, ndlabel+1);
    } else {
      datalabel[0] = '\0';
    }

    datalabelCV(0) = datalabel;
    delete[] datalabel;

    fseek(file, 8, SEEK_CUR); //</label>
    test("<timestamp>", file);
    break;
  }
  }

  CharacterVector timestampCV(1);
  char timestamp[18];
  switch (stataversion)
  {

  case 102:
  case 103:
  case 104:
    timestamp[0] = '\0';
    break;

  case 117:
    {
      /*
      * A dataset may have a timestamp. If it has a timestamp the length of the
      * timestamp (ntimestamp) is 17. Else it is zero.
      * ntimestamp: 0 or 17
      * timestamp: empty or 17 byte string
      */
      uint8_t ntimestamp = 0;
      ntimestamp = readbin(ntimestamp, file, swapit);

      if (ntimestamp == 17) // ntimestap is 0 or 17
      {
        readstr(timestamp, file, ntimestamp+1);
      } else {
        timestamp[0] = '\0';
      }
      break;
    }

  default:
    {
      readstr(timestamp, file, 19);
      break;
    }
  }

  timestampCV(0) = timestamp;

  if (stataversion==117)
  {
    fseek(file, 21, SEEK_CUR); //</timestamp></header>
    test("<map>", file);

    /*
    * Stata stores the byteposition of certain areas of the file here. Currently
    * this is of no use to us.
    * 1.  <stata_data>
    * 2.  <map>
    * 3.  <variable_types>
    * 4.  <varnames>
    * 5.  <sortlist>
    * 6.  <formats>
    * 7.  <value_label_names>
    * 8.  <variable_labels>
    * 9.  <characteristics>
    * 10. <data>
    * 11. <strls>
    * 12. <value_labels>
    * 13. </stata_data>
    * 14. end-of-file
    */

    IntegerVector map(14);
    for (int i=0; i <14; ++i)
    {
      uint64_t nmap = 0;
      nmap = readbin(nmap, file, swapit);
      map[i] = nmap;
    }

    fseek(file, 6, SEEK_CUR); //</map>
    test("<variable_types>", file);
  }

  /*
  * vartypes.
  * 0-2045: strf (String: Max length 2045)
  * 32768:  strL (long String: Max length 2 billion)
  * 65526:  double
  * 65527:  float
  * 65528:  long
  * 65529:  int
  * 65530:  byte
  */

  IntegerVector vartype(k);

  switch (stataversion)
  {

  case 102:
  case 103:
  case 104:
  case 105:
  case 106:
  case 107:
  case 108:
  case 110:
  case 112:
  {
    char nvartypec [1];

    for (uint16_t i=0; i<k; ++i)
    {
      readstr(nvartypec, file, 1+1);

      switch(nvartypec[0])
      {
      case 'd':
        vartype[i] = 255;
        break;
      case 'f':
        vartype[i] = 254;
        break;
      case 'l':
        vartype[i] = 253;
        break;
      case 'i':
        vartype[i] = 252;
        break;
      case 'b':
        vartype[i] = 251;
        break;
      default:
        // 127 is Statas offset
        vartype[i] = *reinterpret_cast<int16_t*>(nvartypec) -127;
      break;
      }
    }
    break;
  }

  case 111:
  case 113:
  case 114:
  case 115:
  {
    uint8_t nvartype = 0;

    for (uint16_t i=0; i<k; ++i)
    {
      nvartype = readbin(nvartype, file, swapit);
      vartype[i] = nvartype;
    }
    break;
  }

  case 117:
  {
    uint16_t nvartype = 0;
    for (uint16_t i=0; i<k; ++i)
    {
      nvartype = readbin(nvartype, file, swapit);
      vartype[i] = nvartype;
    }
    break;
  }
  }


  // FixMe: Needs clone otherwise missing.type would not work
  IntegerVector types = clone(vartype);

  if (stataversion==117)
  {
    fseek(file, 17, SEEK_CUR); //</variable_types>
    test("<varnames>", file);
  }

  /*
  * varnames. Max length 33.
  */

  CharacterVector varnames(k);
  switch(stataversion)
  {
  case 102:
  case 103:
  case 104:
  case 105:
  case 106:
  case 107:
  case 108:
  {
    for (uint16_t i=0; i<k; ++i)
  {
    char nvarnames [10];
    readstr(nvarnames, file, sizeof(nvarnames));
    varnames[i] = nvarnames;
  }
    break;
  }

  default:
  {
    for (uint16_t i=0; i<k; ++i)
  {
    char nvarnames [34];
    readstr(nvarnames, file, sizeof(nvarnames));
    varnames[i] = nvarnames;
  }
    break;
  }
  }

  if (stataversion==117)
  {
    fseek(file, 11, SEEK_CUR); //</varnames>
    test("<sortlist>", file);
  }
  /*
  * sortlist. Stata stores the information which variable of a dataset was
  * sorted. Depending on byteorder sortlist is written different. Currently we
  * do not use this information.
  * Vector size is k+1.
  */

  uint32_t big_k = k+1;

  IntegerVector sortlist(big_k);
  for (uint32_t i=0; i<big_k; ++i)
  {
    uint16_t nsortlist = 0;
    nsortlist = readbin(nsortlist, file, swapit);
    sortlist[i] = nsortlist;
  }

  if (stataversion==117)
  {
    fseek(file, 11, SEEK_CUR); //</sortlist>
    test("<formats>", file);
  }
  /*
  * formats handle how Stata prints a variable. Currently we do not use this
  * information.
  */

  CharacterVector formats(k);

  // FixMe: Same problem. Again only the charlength is different
  switch (stataversion)
  {

  case 102:
  case 103:
  case 104:
  {
    for (uint16_t i=0; i<k; ++i)
  {
    char nformats[8];
    readstr(nformats, file, sizeof(nformats));
    formats[i] = nformats;
  }
    break;
  }
  case 105:
  case 106:
  case 107:
  case 108:
  case 110:
  case 111:
  case 112:
  case 113:
  {
    for (uint16_t i=0; i<k; ++i)
  {
    char nformats[13];
    readstr(nformats, file, sizeof(nformats));
    formats[i] = nformats;
  }
    break;
  }

  case 114:
  case 115:
  case 117:
  {
    for (uint16_t i=0; i<k; ++i)
  {
    char nformats[50];
    readstr(nformats, file, sizeof(nformats));
    formats[i] = nformats;
  }
  }
    break;
  }

  if (stataversion==117)
  {
    fseek(file, 10, SEEK_CUR); //</formats>
    test("<value_label_names>",file);
  }
  /*
  * value_label_names. Stata stores variable labels by names.
  * nvalLabels: length of the value_label_name
  * valLabels:  Char of max length 33
  */

  CharacterVector valLabels(k);
  switch(stataversion)
  {

  case 102:
  case 103:
  case 104:
  case 105:
  case 106:
  case 107:
  case 108:
  {
    for (uint16_t i=0; i<k; ++i)
  {
    char nvalLabels[10];
    readstr(nvalLabels, file, sizeof(nvalLabels));
    valLabels[i] = nvalLabels;
  }
    break;
  }

  default:
  {
    for (uint16_t i=0; i<k; ++i)
  {
    char nvalLabels[34];
    readstr(nvalLabels, file, sizeof(nvalLabels));
    valLabels[i] = nvalLabels;
  }
    break;
  }
  }

  if (stataversion==117)
  {
    fseek(file, 20, SEEK_CUR); //</value_label_names>
    test("<variable_labels>", file);
  }
  /*
  * variabel_labels
  */

  CharacterVector varLabels(k);
  if (stataversion<=106)
  {
    for (uint16_t i=0; i<k; ++i)
    {
      char nvarLabels[33];
      readstr(nvarLabels, file, sizeof(nvarLabels));
      varLabels[i] = nvarLabels;
    }
  }
  else
  {
    for (uint16_t i=0; i<k; ++i)
    {
      char nvarLabels[82];
      readstr(nvarLabels, file, sizeof(nvarLabels));
      varLabels[i] = nvarLabels;
    }
  }

  List ch = List();
  switch (stataversion)
  {

  case 105:
  case 106:
  case 107:
  case 108:
  {
    int8_t datatype = 0;
    uint16_t len = 0;

    datatype = readbin(datatype, file, swapit);
    len = readbin(len, file, swapit);

    while (!(datatype==0) && !(len==0))
    {

      char chvarname[34];
      char chcharact[34];
      char *contents = new char[len-65]; // we need more memory here

      readstr(chvarname, file, sizeof(chvarname));
      readstr(chcharact, file, sizeof(chcharact));
      readstr(contents, file, len-66+1);

      CharacterVector chs(3);
      chs[0] = chvarname;
      chs[1] = chcharact;
      chs[2] = contents;

      delete[] contents;

      ch.push_back(chs);

      datatype = readbin(datatype, file, swapit);
      len = readbin(len, file, swapit);
    }
    break;
  }

  case 110:
  case 111:
  case 112:
  case 113:
  case 114:
  case 115:
  {
    int8_t datatype = 0;
    uint32_t len = 0;

    datatype = readbin(datatype, file, swapit);
    len = readbin(len, file, swapit);

    while (!(datatype==0) && !(len==0))
    {

      char chvarname[34];
      char chcharact[34];
      char *contents = new char[len-65]; // we need more memory here

      readstr(chvarname, file, sizeof(chvarname));
      readstr(chcharact, file, sizeof(chcharact));
      readstr(contents, file, len-66+1);

      CharacterVector chs(3);
      chs[0] = chvarname;
      chs[1] = chcharact;
      chs[2] = contents;

      delete[] contents;

      ch.push_back(chs);

      datatype = readbin(datatype, file, swapit);
      len = readbin(len, file, swapit);
    }
    break;
  }

  case 117:
  {
    fseek(file, 18, SEEK_CUR); //</variable_labels>
    test("<characteristics>", file);
    /*
    * characteristics. Stata can store additional information this way. It may
    * contain notes (for the dataset or a variable) or about label language sets.
    * Characteristics are not documented. We export them as attribute:
    * expansion.fields. Characteristics are seperated by <ch> tags. Each <ch> has:
    * nocharacter:  length of the characteristics
    * chvarname:    varname (binary 0 terminated)
    * chcharact:    characteristicsname (binary 0 terminated)
    * nnocharacter: contes (binary 0 terminated)
    */

    char chtag[5] = "<ch>";
    chtag[4] = '\0';

    CharacterVector chs(3);

    char tago[5];
    readstr(tago, file, sizeof(tago));

    while (strcmp(tago,chtag)==0)
    {
      uint32_t nocharacter = 0;
      nocharacter = readbin(nocharacter, file, swapit);

      char chvarname[34];
      char chcharact[34];
      char *nnocharacter = new char[nocharacter-65]; // we need more memory here

      readstr(chvarname, file, sizeof(chvarname));
      readstr(chcharact, file, sizeof(chcharact));
      readstr(nnocharacter, file, nocharacter-66+1);

      // chs vector
      CharacterVector chs(3);
      chs[0] = chvarname;
      chs[1] = chcharact;
      chs[2] = nnocharacter;

      delete[] nnocharacter;

      // add characteristics to the list
      ch.push_front( chs );

      //fseek(file, 5, SEEK_CUR); // </ch>
      test("</ch>", file);

      // read next tag
      readstr(tago, file, sizeof(tago));
    }

    fseek(file, 14, SEEK_CUR); //[</ch]aracteristics>
    test("<data>", file);

    break;
  }

  }


  /*
  * data. First a list is created with vectors. The vector type is defined by
  * vartype. Stata stores data columnwise so we loop over it and store the
  * data in the list of the first step. Third variable- and row-names are
  * attatched and the list type is changed to data.frame.
  */

  /* replace vartypes of Stata 8 - 12 with Stata 13 values. */
  if (stataversion != 117)
  {
    // 117 contains new variable types (longer strings and strL)
    std::replace (vartype.begin(), vartype.end(), 251, STATA_BYTE);
    std::replace (vartype.begin(), vartype.end(), 252, STATA_SHORTINT);
    std::replace (vartype.begin(), vartype.end(), 253, STATA_INT);
    std::replace (vartype.begin(), vartype.end(), 254, STATA_FLOAT);
    std::replace (vartype.begin(), vartype.end(), 255, STATA_DOUBLE);
  }

  // 1. create the list
  List df(k);
  for (uint16_t i=0; i<k; ++i)
  {
    int const type = vartype[i];
    switch(type)
    {
    case STATA_FLOAT:
    case STATA_DOUBLE:
      SET_VECTOR_ELT(df, i, NumericVector(no_init(n)));
      break;

    case STATA_INT:
    case STATA_SHORTINT:
    case STATA_BYTE:
      SET_VECTOR_ELT(df, i, IntegerVector(no_init(n)));
      break;

    default:
      SET_VECTOR_ELT(df, i, CharacterVector(no_init(n)));
    break;
    }
  }

  // 2. fill it with data

  for(uint32_t j=0; j<n; ++j)
  {
    for (uint16_t i=0; i<k; ++i)
    {
      int32_t const type = vartype[i];
      switch(type)
      {
        // double
      case STATA_DOUBLE:
      {
        double val_d = 0;
        val_d = readbin(val_d, file, swapit);

        if ((missing == FALSE) & ((val_d<STATA_DOUBLE_NA_MIN) | (val_d>STATA_DOUBLE_NA_MAX)) )
          REAL(VECTOR_ELT(df,i))[j] = NA_REAL;
        else
          REAL(VECTOR_ELT(df,i))[j] = val_d;
        break;
      }
        // float
      case STATA_FLOAT:
      {
        float val_f = 0;
        val_f = readbin(val_f, file, swapit);

        if ((missing == FALSE) & ((val_f<STATA_FLOAT_NA_MIN) | (val_f>STATA_FLOAT_NA_MAX)) )
          REAL(VECTOR_ELT(df,i))[j] = NA_REAL;
        else
          REAL(VECTOR_ELT(df,i))[j] = val_f;
        break;
      }
        //long
      case STATA_INT:
      {
        int32_t val_l = 0;
        val_l = readbin(val_l, file, swapit);

        if ((missing == FALSE) & ((val_l<STATA_INT_NA_MIN) | (val_l>STATA_INT_NA_MAX)) )
          INTEGER(VECTOR_ELT(df,i))[j]  = NA_INTEGER;
        else
          INTEGER(VECTOR_ELT(df,i))[j] = val_l;
        break;
      }
        // int
      case STATA_SHORTINT:
      {
        int16_t val_i = 0;
        val_i = readbin(val_i, file, swapit);

        if ((missing == FALSE) & ((val_i<STATA_SHORTINT_NA_MIN) | (val_i>STATA_SHORTINT_NA_MAX)) )
          INTEGER(VECTOR_ELT(df,i))[j] = NA_INTEGER;
        else
          INTEGER(VECTOR_ELT(df,i))[j] = val_i;
        break;
      }
        // byte
      case STATA_BYTE:
      {
        int8_t val_b = 0;
        val_b = readbin(val_b, file, swapit);

        if ((missing == FALSE) & ( (val_b<STATA_BYTE_NA_MIN) | (val_b>STATA_BYTE_NA_MAX)) )
          INTEGER(VECTOR_ELT(df,i))[j] = NA_INTEGER;
        else
          INTEGER(VECTOR_ELT(df,i))[j] = val_b;
        break;
      }
        // string of any length
      case STATA_STRL:
      {// strL 2 4bit
        int32_t v = 0, o = 0;
        v = readbin(v, file, swapit);
        o = readbin(o, file, swapit);

        char val_strl[22];
        sprintf(val_strl, "%010d%010d", v, o);
        as<CharacterVector>(df[i])[j] = val_strl;
        break;
      }
        // strings with 2045 or fewer characters
      default:
      {
        int32_t len = 0;
        len = vartype[i];

        char *val_s = new char[len+1];
        readstr(val_s, file, len+1);
        as<CharacterVector>(df[i])[j] = val_s;
        delete[] val_s;
        break;
      }
      }
    }
  }

  // 3. Create a data.frame
  R_xlen_t nrows = Rf_length(df[0]);
  df.attr("row.names") = IntegerVector::create(NA_INTEGER, nrows);
  df.attr("names") = varnames;
  df.attr("class") = "data.frame";


  List strlstable = List(); //put strLs into this list
  if (stataversion==117)
  {
    fseek(file, 7, SEEK_CUR); //</data>
    test("<strls>", file);

    /*
    * strL. Stata 13 introduced long strings up to 2 billon characters. strLs are
    * sperated by "GSO".
    * (v,o): Position in the data.frame.
    * t:     129/130 defines whether or not the strL is stored with a binary 0.
    * len:   length of the strL.
    * strl:  long string.
    */


    char tags[4];
    readstr(tags, file, sizeof(tags));

    char gso[4] = "GSO";
    gso[3] = '\0';

    while(strcmp(gso,tags)==0)
    {
      CharacterVector strls(2);

      // 2x4 bit (strl[vo1,vo2])
      int32_t v = 0, o = 0;
      v = readbin(v, file, swapit);
      o = readbin(o, file, swapit);
      char erg[22];
      sprintf(erg, "%010d%010d", v, o);

      strls(0) = erg;

      // (129 = binary) | (130 = ascii)
      uint8_t t = 0;
      t = readbin(t, file, swapit);

      uint32_t len = 0;
      len = readbin(len, file, swapit);

      if (t==129)
      {
        char *strl = new char[len];
        readstr(strl, file, len);
        strls(1) = strl;
        delete[] strl;
      } else
      {
        if (t==130)
        {
          char *strl = new char[len+1];
          readstr(strl, file, len+1);
          strls(1) = strl;
          delete[] strl;
        }
      }

      strlstable.push_back( strls );

      readstr(tags, file, sizeof(tags));
    }

    // after strls
    fseek(file, 5, SEEK_CUR); //[</s]trls>
    test("<value_labels>", file);
  }

  /*
  * labels are seperated by <lbl>-tags. Labels may appear in any order e.g.
  * 2 "female" 1 "male 9 "missing". They are stored as tables.
  * nlen:     length of label.
  * nlabname: label name.
  * labn:     number of labels in this set (e.g. "male" "female" = 2)
  * txtlen:   length of the label text.
  * off:      offset defines where to read a new label in txtlen.
  */

  List labelList = List(); //put labels into this list

  // FixMe: the while statement differs and the final check
  switch (stataversion)
  {

    // case 105:
  case 106:
  case 107:
  case 108:
  {

    int32_t nlen = 0, labn = 0, txtlen = 0, noff = 0, val = 0;

    // length of value_label_table
    nlen = readbin(nlen, file, swapit);

    while(!feof(file)||ferror(file))
    {
      // name of this label set
      char nlabname[10];
      readstr(nlabname, file, sizeof(nlabname));

      //padding
      fseek(file, 3, SEEK_CUR);

      // value_label_table for actual label set
      labn = readbin(labn, file, swapit);

      txtlen = readbin(txtlen, file, swapit);

      // offset for each label
      // off0 : label 0 starts at off0
      // off1 : label 1 starts at off1 ...
      IntegerVector off(labn);
      for (int i=0; i < labn; ++i) {
        noff = readbin(noff, file, swapit);
        off[i] = noff;
      }

      // needed for match
      IntegerVector laborder = clone(off);
      //laborder.erase(labn+1);
      IntegerVector labordersort = clone(off);
      //labordersort.erase(labn+1);
      std::sort(labordersort.begin(), labordersort.end());

      // needs txtlen for loop
      off.push_back(txtlen);

      // sort offsets so we can read labels sequentially
      std::sort(off.begin(), off.end());

      // create an index to sort lables along the code values
      // this is done while factor creation
      IntegerVector indx(labn);
      indx = match(laborder,labordersort);

      // code for each label
      IntegerVector code(labn);
      for (int i=0; i < labn; ++i) {
        val = readbin(val, file, swapit);
        code[i] = val;
      }

      // label text
      CharacterVector label(labn);
      for (int i=0; i < labn; ++i) {
        int const lablen = off[i+1]-off[i];
        char *lab = new char[lablen+1]; //add + 1
        readstr(lab, file, lablen+1);
        label[i] = lab;
        delete[] lab;
      }

      // sort labels according to indx
      CharacterVector labelo(labn);
      for (int i=0; i < labn; ++i) {
        labelo[i] = label[indx[i]-1];
      }
      // create table for actual label set
      string const labset = nlabname;
      code.attr("names") = labelo;

      // add this set to output list
      labelList.push_front( code, labset);

      // EOF reached?
      nlen = readbin(nlen, file, swapit);
    }
    break;
  }

  case 110:
  case 111:
  case 112:
  case 113:
  case 114:
  case 115:
  {

    int32_t nlen = 0, labn = 0, txtlen = 0, noff = 0, val = 0;

    // length of value_label_table
    nlen = readbin(nlen, file, swapit);

    while(!feof(file)||ferror(file))
    {
      // name of this label set
      char nlabname[34];
      readstr(nlabname, file, sizeof(nlabname));

      //padding
      fseek(file, 3, SEEK_CUR);

      // value_label_table for actual label set
      labn = readbin(labn, file, swapit);

      txtlen = readbin(txtlen, file, swapit);

      // offset for each label
      // off0 : label 0 starts at off0
      // off1 : label 1 starts at off1 ...
      IntegerVector off(labn);
      for (int i=0; i < labn; ++i) {
        noff = readbin(noff, file, swapit);
        off[i] = noff;
      }

      // needed for match
      IntegerVector laborder = clone(off);
      //laborder.erase(labn+1);
      IntegerVector labordersort = clone(off);
      //labordersort.erase(labn+1);
      std::sort(labordersort.begin(), labordersort.end());

      // needs txtlen for loop
      off.push_back(txtlen);

      // sort offsets so we can read labels sequentially
      std::sort(off.begin(), off.end());

      // create an index to sort lables along the code values
      // this is done while factor creation
      IntegerVector indx(labn);
      indx = match(laborder,labordersort);

      // code for each label
      IntegerVector code(labn);
      for (int i=0; i < labn; ++i) {
        val = readbin(val, file, swapit);
        code[i] = val;
      }

      // label text
      CharacterVector label(labn);
      for (int i=0; i < labn; ++i) {
        int const lablen = off[i+1]-off[i];
        char *lab = new char[lablen+1]; //add + 1
        readstr(lab, file, lablen+1);
        label[i] = lab;
        delete[] lab;
      }

      // sort labels according to indx
      CharacterVector labelo(labn);
      for (int i=0; i < labn; ++i) {
        labelo[i] = label[indx[i]-1];
      }
      // create table for actual label set
      string const labset = nlabname;
      code.attr("names") = labelo;

      // add this set to output list
      labelList.push_front( code, labset);

      // EOF reached?
      nlen = readbin(nlen, file, swapit);
    }
    break;
  }

  case 117:
  {
    char tag[6];
    readstr(tag, file, sizeof(tag));

    char lbltag[6] = "<lbl>";
    lbltag[5] = '\0';

    while(strcmp(lbltag,tag)==0)
    {
      int32_t nlen = 0, labn = 0, txtlen = 0, noff = 0, val = 0;

      // length of value_label_table
      nlen = readbin(nlen, file, swapit);

      // name of this label set
      char nlabname[34];
      readstr(nlabname, file, sizeof(nlabname));

      //padding
      fseek(file, 3, SEEK_CUR);

      // value_label_table for actual label set
      labn = readbin(labn, file, swapit);
      txtlen = readbin(txtlen, file, swapit);

      // offset for each label
      // off0 : label 0 starts at off0
      // off1 : label 1 starts at off1 ...
      IntegerVector off(labn);
      for (int i=0; i < labn; ++i) {
        noff = readbin(noff, file, swapit);
        off[i] = noff;
      }

      // needed for match
      IntegerVector laborder = clone(off);
      //laborder.erase(labn+1);
      IntegerVector labordersort = clone(off);
      //labordersort.erase(labn+1);
      std::sort(labordersort.begin(), labordersort.end());

      // needs txtlen for loop
      off.push_back(txtlen);

      // sort offsets so we can read labels sequentially
      std::sort(off.begin(), off.end());

      // create an index to sort lables along the code values
      // this is done while factor creation
      IntegerVector indx(labn);
      indx = match(laborder,labordersort);

      // code for each label
      IntegerVector code(labn);
      for (int i=0; i < labn; ++i) {
        val = readbin(val, file, swapit);
        code[i] = val;
      }

      // label text
      CharacterVector label(labn);
      for (int i=0; i < labn; ++i) {
        int const lablen = off[i+1]-off[i];
        char *lab = new char[lablen+1]; //add + 1
        readstr(lab, file, lablen+1);
        label[i] = lab;
        delete[] lab;
      }

      // sort labels according to indx
      CharacterVector labelo(labn);
      for (int i=0; i < labn; ++i) {
        labelo[i] = label[indx[i]-1];
      }
      // create table for actual label set
      string const labset = nlabname;
      code.attr("names") = labelo;

      // add this set to output list
      labelList.push_front( code, labset);

      fseek(file, 6, SEEK_CUR); //</lbl>

      readstr(tag, file, sizeof(tag));
    }
    break;
  }

  }

  if (stataversion==117)
  {
    /*
    * Final test if we reached the end of the file
    * close the file
    */


    fseek(file, 10, SEEK_CUR); // [</val]ue_labels>
    test("</stata_dta>", file);
  }

  fclose(file);

  /*
   * assign attributes to the resulting data.frame
   */

  df.attr("datalabel") = datalabelCV;
  df.attr("time.stamp") = timestampCV;
  df.attr("formats") = formats;
  df.attr("types") = types;
  df.attr("val.labels") = valLabels;
  df.attr("var.labels") = varLabels;
  df.attr("version") = versionIV;
  df.attr("label.table") = labelList;
  df.attr("expansion.fields") = ch;
  if (stataversion == 117)
  {
    df.attr("strl") = strlstable;
    df.attr("byteorder") = byteorderC;
  } else {
    df.attr("byteorder") = byteorderI;
  }
  return df;
}
