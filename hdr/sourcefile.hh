//
//  sourcefile.hh -- declare Source_file
//
//  copyright 1997 Jan Nieuwenhuizen <jan@digicash.nl>

#ifndef SOURCE_FILE_HH
#define SOURCE_FILE_HH

/// class for reading and mapping a file. 
class Source_file
{
public:
    /**
      RETURN path to opened file.
     */
    Source_file( String &filename_str );
    ~Source_file();
    char const* ch_c_l();
    String error_str( char const* pos_ch_c_l );
    istream* istream_l();
    bool in_b( char const* pos_ch_c_l );
    int line_i( char const* pos_ch_c_l );
    String name_str();
    String file_line_no_str(const char *ch_c_l );

private:
    void close();
    void map();
    void open();
    void unmap();

    istream* istream_p_m;
    int fildes_i_m;
    String name_str_m;
    off_t size_off_m;
    caddr_t data_caddr_m;
};

#endif // SOURCE_FILE_HH //
