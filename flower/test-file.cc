#include "std-vector.hh"

#include <unistd.h>
#include <iostream>

using namespace std;

#include "file-name.hh"
string slashify (string file_name);

#include "yaffut.hh"

// FIXME: split into file-name, file-path unit fixture tests
FUNC (mingw_slashify)
{
  File_name f = string ("foe.ly");
  string s = slashify (f.to_string ());
  cout << s << endl;
  EQUAL ("foe.ly", s);
  f = string ("/tmp/x.ly");
  s = slashify (f.to_string ());
  cout << s << endl;
  EQUAL ("/tmp/x.ly", s);
  f = string ("c:/tmp/x.ly");
  s = slashify (f.to_string ());
  cout << s << endl;
  EQUAL ("c:/tmp/x.ly", s);
  f = string ("\\tmp\\x.ly");
  s = slashify (f.to_string ());
  cout << s << endl;
  EQUAL ("/tmp/x.ly", s);
}

#include "config.hh"
#include "file-path.hh"

FUNC (file_find)
{
  char const *extensions[] = {"ly", "", 0};
  string file = "init";
  File_path path;
  char cwd[PATH_MAX];
  getcwd (cwd, PATH_MAX);
  string ly_dir = string (getenv ("top-src-dir")) + "/ly";
  cout << ly_dir << endl;
  path.parse_path (string (1, PATHSEP) + ly_dir);
  string file_name = path.find (file, extensions);
  cout << file_name << endl;
  EQUAL (file_name.substr (file_name.rfind ('/')), "/init.ly");
  file = "init.ly";
  file_name = path.find (file, extensions);
  cout << file_name << endl;
  EQUAL (file_name, ly_dir + "/init.ly");
  
}
