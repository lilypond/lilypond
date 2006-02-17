#define HAVE_BOOST_LAMBDA 1
#include "std-vector.hh"

#include <unistd.h>
#include <iostream>

#include <boost/test/auto_unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

using namespace std;
using boost::unit_test::test_suite;

#include "file-name.hh"
string slashify (string file_name);

// FIXME
//BOOST_AUTO_UNIT_TEST (mingw_slashify)
void mingw_slashify ()
{
  File_name f = string ("foe.ly");
  string s = slashify (f.to_string ());
  cout << s << endl;
  BOOST_CHECK_EQUAL (s, "foe.ly");
  f = string ("/tmp/x.ly");
  s = slashify (f.to_string ());
  cout << s << endl;
  BOOST_CHECK_EQUAL (s, "/tmp/x.ly");
  f = string ("c:/tmp/x.ly");
  s = slashify (f.to_string ());
  cout << s << endl;
  BOOST_CHECK_EQUAL (s, "c:/tmp/x.ly");
  f = string ("\\tmp\\x.ly");
  s = slashify (f.to_string ());
  cout << s << endl;
  BOOST_CHECK_EQUAL (s, "/tmp/x.ly");
}

#include "config.hh"
#include "file-path.hh"

// FIXME
//BOOST_AUTO_UNIT_TEST (mingw_slashify)
void file_find ()
{
  char const *extensions[] = {"ly", "", 0};
  string file = "init";
  File_path path;
  char cwd[PATH_MAX];
  getcwd (cwd, PATH_MAX);
  string ly_dir = string (getenv ("LILYPONDPREFIX")) + "/ly";
  cout << ly_dir << endl;
  path.parse_path (string (1, PATHSEP) + ly_dir);
  string file_name = path.find (file, extensions);
  cout << file_name << endl;
  BOOST_CHECK_EQUAL (file_name.substr (file_name.rfind ('/')), "/init.ly");
  file = "init.ly";
  file_name = path.find (file, extensions);
  cout << file_name << endl;
  BOOST_CHECK_EQUAL (file_name, ly_dir + "/init.ly");
}
