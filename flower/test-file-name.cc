#include "file-name.hh"

#include "yaffut-parameters.hh"

using namespace std;

string slashify (string file_name);

TEST_STRING (File_name, Mingw_slashify, "foe.ly")
{
  string s = slashify (to_string ());
  EQUAL (parameter_one_, s);
}

TEST_STRING (File_name, Mingw_slashify_2, "/tmp/x.ly")
{
  string s = slashify (to_string ());
  EQUAL (parameter_one_, s);
}

TEST_STRING (File_name, Mingw_slashify_3, "c:/tmp/x.ly")
{
  string s = slashify (to_string ());
  EQUAL (parameter_one_, s);
}

TEST_STRING (File_name, Mingw_slashify_4, "\\tmp\\x.ly")
{
  string s = slashify (to_string ());
  EQUAL ("/tmp/x.ly", s);
}
