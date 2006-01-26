/*
  source.hh -- part of LilyPond

  (c) 1997--2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef SOURCE_HH
#define SOURCE_HH

#include "cons.hh"
#include "flower-proto.hh"
#include "std-string.hh"

/*   A set of sourcefiles.  */
class Sources
{
  Sources (Sources const &) {}
  Cons<Source_file> *sourcefile_list_;
  bool is_binary_;

public:
  Sources ();
  ~Sources ();

  Source_file *get_file (std::string &file_name);
  Source_file *get_sourcefile (char const *);
  void add (Source_file *sourcefile);
  void set_path (File_path *);
  void set_binary (bool);

  const File_path *path_;
};

#endif /* SOURCE_HH */
