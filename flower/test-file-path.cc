#include "file-path.hh"

#include <limits.h>

#include "yaffut.hh"
#include "config.hh"

TEST (File_path, Find)
{
  char const *extensions[] = {"ly", "", 0};
  string file = "init";
  char cwd[PATH_MAX];
  getcwd (cwd, PATH_MAX);
  string ly_dir = string (getenv ("top-src-dir")) + "/ly";
  parse_path (string (1, PATHSEP) + ly_dir);
  string file_name = find (file, extensions);
  EQUAL (file_name.substr (file_name.rfind ('/')), "/init.ly");
  file = "init.ly";
  file_name = find (file, extensions);
  EQUAL (file_name, ly_dir + "/init.ly");
}
