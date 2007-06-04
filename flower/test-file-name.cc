#include "file-name.hh"

#include "yaffut-parameters.hh"

using namespace std;

string slashify (string file_name);

struct CaseB: public yaffut::Test<File_name, CaseB>
{
  CaseB(): File_name("123"){}
};

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

TEST_STRING (File_name, Canonicalize, "foo//bar/..//bla//z.ly")
{
  string s = canonicalized ().to_string ();
  EQUAL ("foo/bla/z.ly", s);
}

