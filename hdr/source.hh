//
//  source.hh -- part of LilyPond
//
//  copyright 1997 Jan Nieuwenhuizen <jan@digicash.nl>

#ifndef SOURCE_HH
#define SOURCE_HH

class Source 
{
public:
    Source();
    ~Source();

    void add( Source_file* sourcefile_p );
    Source_file* sourcefile_l( char const* ch_c_l );

private:
    IPointerList<Source_file*> sourcefile_p_iplist_m;
};

#endif // SOURCE_HH //
