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

public:
    set_spot(char const *);
    Input(char const *);
    Input(Input const &);
};

#endif // INPUT_HH
