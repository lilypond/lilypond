/*
  rhythmstaf.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef MELODICSTAFF_HH
#define MELODICSTAFF_HH

#include "simplestaff.hh"

/// five line staff, no multiple voices
struct Melodic_staff : public Simple_staff
{
    
    /****************/

    virtual Rest *get_rest(Rest_req *rq);
    virtual void set_output(PScore *);

    virtual Item* get_TYPESET_item(Command*);
    virtual Stem * get_stem(Stem_req *rq);
    virtual Notehead * get_notehead(Note_req *rq, int bot);
    virtual Local_key_item* get_local_key_item();
};

#endif // MELODICSTAFF_HH


