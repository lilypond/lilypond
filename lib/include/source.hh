//
//  source.hh -- part of LilyPond
//
//  copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#ifndef SOURCE_HH
#define SOURCE_HH

#include "proto.hh"
#include "plist.hh"

class Sources 
{
public:
  Sources ();

  Source_file * get_file_l (String &filename );
  Source_file* sourcefile_l (char const* ch_C );
  void set_path (File_path*p_C);
  void set_binary (bool);

private:
  const File_path * path_C_;
  void add (Source_file* sourcefile_p );
  Pointer_list<Source_file*> sourcefile_p_list_;
  bool binary_b_ ;
};



#endif // SOURCE_HH //
