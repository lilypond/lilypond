/*
  textdef.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef TEXTDEF_HH
#define TEXTDEF_HH

#include "string.hh"

struct Text_def {
    int align;
    String text;
    String style;

    /*****************/
    
    Text_def();
    void print()const;
    Atom create(Paperdef*)const;
};
#endif // TEXTDEF_HH

