//
//  source-file.hh -- declare Source_file
//
//  copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#ifndef SOURCE_FILE_HH
#define SOURCE_FILE_HH
#include "fproto.hh"
#include "string.hh"
class istream;
/// class for reading and mapping a file. 
class Source_file
{
public:
    /** Ugh! filename gets changed! The path to the opened file may
       change, since it might be searched in multiple directories.  */
    Source_file( String filename_str_r );
    virtual ~Source_file();

    char const* ch_C();
    virtual String error_str( char const* pos_ch_c_l );
    istream * istream_l();
    bool in_b( char const* pos_ch_c_l );
    off_t length_off();
    virtual int line_i( char const* pos_ch_c_l );
    String name_str();
    String file_line_no_str( char const* ch_c_l );

private:
    void close();
    void map();
    void open();
    void unmap();

    istream* istream_p_;
    int fildes_i_;
    String name_str_;
    off_t size_off_;
    caddr_t data_caddr_;
};

#endif // SOURCE_FILE_HH //
