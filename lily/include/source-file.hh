/*
  source-file.hh -- declare Source_file

  source file of the GNU LilyPond music typesetter

  (c) 1999--2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef SOURCE_FILE_HH
#define SOURCE_FILE_HH

#include <iostream>
using namespace std;

#include "string.hh"
#include "protected-scm.hh"
#include "parray.hh"

/**
   class for reading and mapping a file.

   duplicates a lot of Data_file and Text_stream.
   should look at including Data_file's functionality:
   get_line (), get_word () here.
*/

class Source_file
{
public:
  Source_file (String fn);
  Source_file (String, String);

  virtual ~Source_file ();

  char const *c_str () const;
  virtual String quote_input (char const *pos_str0) const;
  istream *get_istream ();
  bool contains (char const *pos_str0) const;
  int length () const;
  virtual int get_line (char const *pos_str0) const;
  String name_string () const;
  String file_line_column_string (char const *str0) const;

  // return start + n
  char const *seek_str0 (int n);

  int tell () const;
  // return here + n bytes
  char const *forward_str0 (int n);
  char const *pos_str0 () { return pos_str0_; }
  String get_string (int n);
  void set_pos (char const *pos_str0);
public:
  Slice line_slice (char const *pos_str0) const;
  String line_string (char const *pos_str0) const;
  void get_counts (char const *pos_str0, int *, int *, int *) const;

  /*
    JUNKME.

    This thing doubles as a file-storage/file-iterator object.
  */
  char const *pos_str0_;

  SCM get_port () const;
  String name_;

private:
  Link_array<char> newline_locations_;
  istream *istream_;
  char *contents_str0_;
  int length_;
  void load_stdin ();
  void init_port ();

  Protected_scm str_port_;
};

char *gulp_file (String fn, int *len);

#endif /* SOURCE_FILE_HH */

