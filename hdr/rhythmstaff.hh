/*
  rhythmstaf.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef RHYTHMSTAF_HH
#define RHYTHMSTAF_HH

#include "simplestaff.hh"

/// all notes on one line
struct Rhythmic_staff : public Simple_staff
{

    /****************/
    
    virtual Item *get_TYPESET_item(Command*);    
    virtual Stem *get_stem(Stem_req *rq);
    virtual Notehead * get_notehead(Note_req *rq, int b);   
    virtual void set_output(PScore *);
};

#endif // RHYTHMSTAF_HH


