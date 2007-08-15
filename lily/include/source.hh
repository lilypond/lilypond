/*
  source.hh -- part of LilyPond

  (c) 1997--2007 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef SOURCE_HH
#define SOURCE_HH

#include "flower-proto.hh"
#include "std-vector.hh"

/*   A set of sourcefiles.  */
class Sources
{
  Sources (Sources const &);
  vector<Source_file*> sourcefiles_;

public:
  Sources ();
  ~Sources ();

  Source_file *get_file (string *file_name);
  Source_file *get_sourcefile (char const *);
  void add (Source_file *sourcefile);
  void set_path (File_path *);

  const File_path *path_;
};

#endif /* SOURCE_HH */
