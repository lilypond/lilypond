/*
  file-name-map.cc --  implement map_file_name ()

  source file of the GNU LilyPond music typesetter

  (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include <map>
using namespace std;

#include "file-name-map.hh"
#include "lily-guile.hh"

map<string, string> file_name_map_global;

string
map_file_name (string s)
{
  if (file_name_map_global.find (s) != file_name_map_global.end ())
    s = file_name_map_global[s];
  return s;
}

LY_DEFINE (ly_add_file_name_alist, "ly:add-file-name-alist",
	   1, 0, 0, (SCM alist),
	   "Add mappings for error messages from @var{alist}.")
{
  for (SCM s = alist; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM key = scm_caar (s);
      SCM val = scm_cdar (s);

      file_name_map_global[ly_scm2string (key)] = ly_scm2string (val);
    }
  return SCM_UNSPECIFIED;
}

