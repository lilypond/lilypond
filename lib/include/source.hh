//
//  source.hh -- part of LilyPond
//
//  copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#ifndef SOURCE_HH
#define SOURCE_HH
#include "source-file.hh"
#include "plist.hh"
#include "path.hh"
class Sources 
{
public:
    Source_file * get_file_l( String &filename );
    Source_file* sourcefile_l( char const* ch_c_l );
    void set_path(File_path*p_C);
    Sources();
    void set_binary(bool);
private:
    const File_path * path_C_;
    void add( Source_file* sourcefile_p );
    IPointerList<Source_file*> sourcefile_p_iplist_;
    bool binary_b_ ;
};



#endif // SOURCE_HH //
