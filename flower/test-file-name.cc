#include "file-name.hh"

#include "yaffut-parameters.hh"

using namespace std;
using std::string;

string slashify (string file_name);

struct CaseB: public yaffut::Test<File_name, CaseB>
{
  CaseB (): File_name ("123") {}
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

TEST_STRING (File_name, Canonicalize_1, "foo//bar/./..//bla//z.ly")
{
  string s = canonicalized ().to_string ();
  EQUAL ("foo/bla/z.ly", s);
}

TEST_STRING (File_name, Canonicalize_2, "./foo")
{
  string s = canonicalized ().to_string ();
  EQUAL (parameter_one_, s);
}

TEST_STRING (File_name, Canonicalize_3, "/")
{
  string s = canonicalized ().to_string ();
  EQUAL (parameter_one_, s);
}

TEST_STRING (File_name, Canonicalize_4, "/foo")
{
  string s = canonicalized ().to_string ();
  EQUAL (parameter_one_, s);
}

TEST_STRING (File_name, Canonicalize_5, "foo/")
{
  string s = canonicalized ().to_string ();
  EQUAL ("foo", s);
}

TEST_STRING (File_name, Canonicalize_6, "foo/./..")
{
  string s = canonicalized ().to_string ();
  EQUAL (".", s);
}

TEST_STRING (File_name, Canonicalize_7, "../.")
{
  string s = canonicalized ().to_string ();
  EQUAL ("..", s);
}

TEST_STRING (File_name, Canonicalize_8, "./../")
{
  string s = canonicalized ().to_string ();
  EQUAL ("..", s);
}
