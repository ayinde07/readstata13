#ifndef READSTATA_H
#define READSTATA_H


template <typename T>
T readbin( T t , FILE * file, bool swapit)
{
  if (fread(&t, sizeof(t), 1, file) != 1) {
    if (feof(file))
      return 0; // this is expected after reading the labeltable
  } else if (ferror(file)){
    perror("A binary read error occurred.");
  }
  if (swapit==0)
    return(t);
  else
    return(swap_endian(t));
}

static void readstr(char *var, FILE * fp, int nchar)
{
  nchar = nchar-1;
  if (!fread(var, nchar, 1, fp))
    perror("a binary read error occurred");
  var[nchar] = '\0';
}

void test(std::string testme, FILE * file)
{
  const char *testMe = testme.c_str();
  char *test = new char[1+testme.size()];
  readstr(test,file, 1+testme.size());
  if (strcmp(testMe,test)!=0)
  {
    REprintf("When attempting to read %s:", testme.c_str());
    throw std::range_error("Something went wrong!");
  }
  delete[] test;
}

#endif
