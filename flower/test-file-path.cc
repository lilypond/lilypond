#include "file-name.hh"
#include "file-path.hh"

#include <limits.h>
#include <unistd.h>

#include "config.hh"
#include "yaffut.hh"

using std::string;

TEST (File_path, Find)
{
  char const *extensions[] = {"ly", "", 0};
  string file = "init";
  if (get_working_directory ().empty ())
    {
      std::cerr << "Could not get current work directory\n";
      exit (1);
    }
  string ly_dir = string (getenv ("top-src-dir")) + "/ly";
  parse_path (string (1, PATHSEP) + ly_dir);
  string file_name = find (file, extensions);
  EQUAL (file_name.substr (file_name.rfind ('/')), "/init.ly");
  file = "init.ly";
  file_name = find (file, extensions);
  EQUAL (file_name, ly_dir + "/init.ly");
}
