/*
  input.hh -- declare Input

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef INPUT_HH
#define INPUT_HH

/**
  Base class for anything that records its poisition in the parse file.
 */
class Input {
    char const *defined_ch_C_ ;
    Sources * sources_l_;
public:
    
    void warning(String)const; // should use member func?
    void error(String)const;
    void message(String)const;
    void set_spot(Input const &);
    void set_sources(Sources *);
    
    String location_str()const;
    Input(Sources *,char const*);
    Input();
};

#endif // INPUT_HH
