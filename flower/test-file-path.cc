#include "file-path.hh"
#include "file-name.hh"

#include <limits.h>
#include <unistd.h>

#include "yaffut.hh"
#include "config.hh"

using std::string;

TEST (File_path, Find)
{
  char const *extensions[] = {"ly", "", 0};
  string file = "init";
  if (get_working_directory ().empty ())
    {
      FAIL ("Could not get current work directory\n");
    }
  char *top_src_dir = getenv ("top-src-dir");
  if (!top_src_dir)
    {
      FAIL ("Could not get top source directory\n");
    }
  string ly_dir = string (top_src_dir) + "/ly";
  parse_path (string (1, PATHSEP) + ly_dir);
  string file_name = find (file, extensions);
  EQUAL (file_name.substr (file_name.rfind ('/')), "/init.ly");
  file = "init.ly";
  file_name = find (file, extensions);
  EQUAL (file_name, ly_dir + "/init.ly");
}
