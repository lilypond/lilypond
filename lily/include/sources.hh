/*
  sources.hh -- part of LilyPond

  (c) 1997--2009 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef SOURCES_HH
#define SOURCES_HH

#include "lily-proto.hh"
#include "std-vector.hh"

class Sources
{
  Sources (Sources const &);
  vector<Source_file*> sourcefiles_;

public:
  Sources ();
  ~Sources ();

  Source_file *get_file (string file_name, string const& currentpath);
  void add (Source_file *sourcefile);
  void set_path (File_path *);

  const File_path *path_;
};

#endif /* SOURCE_HH */
