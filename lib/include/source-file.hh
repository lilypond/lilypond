//
//  source-file.hh -- declare Source_file
//
//  copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

#ifndef SOURCE_FILE_HH
#define SOURCE_FILE_HH

#include "proto.hh"
#include "string.hh"
#include "interval.hh"

class istream;


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
  Source_file (String filename_str_r );

  Source_file (String name_str, String data_str);
  virtual ~Source_file ();

  char const* ch_C () const;
  virtual String error_str (char const* pos_ch_C ) const;
  istream * istream_l ();
  bool in_b (char const* pos_ch_C ) const;
  int length_i () const;
  virtual int line_i (char const* pos_ch_C ) const;
  String name_str () const;
  String file_line_column_str (char const* ch_C ) const;

  // return start + n
  char const* seek_ch_C (int n);
  // return here + n bytes
  char const* forward_ch_C (int n);
  char const* pos_ch_C () { return pos_ch_C_; }
  String get_str (int n);
  void set_pos (char const * pos_ch_C);
  
  // tbd
  // String get_line ();
  // String get_word ();
  // only used in binary-source-file, currently


protected:
  Slice line_slice (char const* pos_ch_C) const;
  String line_str (char const* pos_ch_C) const;
  int column_i (char const* pos_ch_C) const;
  int char_i (char const* pos_ch_C) const;

  char const* pos_ch_C_;

private:
  String name_str_;
  istream* istream_p_;
  File_storage * storage_p_;
};

#endif // SOURCE_FILE_HH //

