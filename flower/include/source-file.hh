
#ifndef SOURCE_FILE_HH
#define SOURCE_FILE_HH

#include <iostream>

#include "flower-proto.hh"
#include "string.hh"
#include "interval.hh"

/**
  class for reading and mapping a file. 

  duplicates a lot of Data_file and Text_stream.
  should look at including Data_file's functionality:
  get_line (), get_word () here.
*/

class Source_file
{
public:
  /** Ugh! filename gets changed! The path to the opened file may
    change, since it might be searched in multiple directories.  */
  Source_file (String filename_string_r );

  Source_file (String name_string, String data_string);
  virtual ~Source_file ();

  char const* to_str0 () const;
  virtual String error_string (char const* pos_str0 ) const;
  std::istream * get_istream ();
  bool in_b (char const* pos_str0 ) const;
  int length () const;
  virtual int get_line (char const* pos_str0 ) const;
  String name_string () const;
  String file_line_column_string (char const* str0 ) const;

  // return start + n
  char const* seek_str0 (int n);
  // return here + n bytes
  char const* forward_str0 (int n);
  char const* pos_str0 () { return pos_str0_; }
  String get_string (int n);
  void set_pos (char const * pos_str0);
public:
  Slice line_slice (char const* pos_str0) const;
  String line_string (char const* pos_str0) const;
  int get_column (char const* pos_str0) const;
  int get_char (char const* pos_str0) const;

  /*
    DOCUMENT-ME
   */
  char const* pos_str0_;

private:
  String name_string_;
  std::istream* istream_;
  File_storage * storage_;
};

#endif // SOURCE_FILE_HH //

