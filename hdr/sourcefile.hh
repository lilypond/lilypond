//
//  sourcefile.hh -- part of LilyPond
//
//  copyright 1997 Jan Nieuwenhuizen <jan@digicash.nl>

#ifndef SOURCE_FILE_HH
#define SOURCE_FILE_HH

class Source_file
{
public:
    ///
    Source_file( String &filename_str );
    /**
      RETURN path to opened file.
     */
    ~Source_file();
    char const* ch_c_l();
    String error_str( char const* pos_ch_c_l );
    istream* istream_l();
    bool in_b( char const* pos_ch_c_l );
    int line_i( char const* pos_ch_c_l );
    String name_str();

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
