/*
  source-file.hh -- declare Source_file

  source file of the GNU LilyPond music typesetter

  (c) 1999--2008 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef SOURCE_FILE_HH
#define SOURCE_FILE_HH

#include "std-vector.hh"
#include "lily-proto.hh"
#include "smobs.hh"

#include <iostream>
using namespace std;

/**
   class for reading and mapping a file.

   duplicates a lot of Data_file and Text_stream.
   should look at including Data_file's functionality:
   get_line (), get_word () here.
*/

class Source_file
{
  vector<char const*> newline_locations_;
  istream *istream_;
  vector<char> characters_;
  SCM str_port_;

  void load_stdin ();
  void init_port ();
  void init ();
  
  DECLARE_SMOBS (Source_file);
public:
  Source_file (string fn);
  Source_file (string, string);

  char const *c_str () const;
  virtual string quote_input (char const *pos_str0) const;
  istream *get_istream ();
  bool contains (char const *pos_str0) const;
  int length () const;
  virtual int get_line (char const *pos_str0) const;
  void set_line (char const *pos_str0, int i);
  string name_string () const;
  string file_line_column_string (char const *str0) const;

public:
  Slice line_slice (char const *pos_str0) const;
  string line_string (char const *pos_str0) const;
  void get_counts (char const *pos_str0, int *, int *, int *) const;
  
  SCM get_port () const;
  string name_;

protected:
  int line_offset_;
};

vector<char> gulp_file (string fn, int desired);

#endif /* SOURCE_FILE_HH */

