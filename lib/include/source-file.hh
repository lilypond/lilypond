//
//  source-file.hh -- declare Source_file
//
//  copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#ifndef SOURCE_FILE_HH
#define SOURCE_FILE_HH

#include "proto.hh"
#include "string.hh"

class istream;


/// class for reading and mapping a file. 
class Source_file
{
public:
  /** Ugh! filename gets changed! The path to the opened file may
    change, since it might be searched in multiple directories.  */
  Source_file (String filename_str_r );
  virtual ~Source_file ();

  char const* ch_C () const;
  virtual String error_str (char const* pos_ch_c_l ) const;
  istream * istream_l ();
  bool in_b (char const* pos_ch_c_l ) const;
  int length_i () const;
  virtual int line_i (char const* pos_ch_c_l ) const;
  String name_str () const;
  String file_line_no_str (char const* ch_c_l ) const;

private:
  String name_str_;
  istream* istream_p_;
  File_storage * storage_p_;
};

#endif // SOURCE_FILE_HH //
