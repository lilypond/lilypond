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
    Staff_column * create_col(Score_column*);
    virtual void set_output(PScore *);
    virtual Melodic_staff*clone()const;
};

struct Melodic_column : public Simple_column {
    virtual void typeset_req(Request *rq);
    virtual void typeset_stem(Stem_req *rq);
    virtual void typeset_command(Command *, int brs);
  //    virtual void typeset_item(Item*, int=1);
    Melodic_column(Score_column*s,Simple_staff*rs) :
	Simple_column(s,rs) { }
//    virtual Melodic_column*clone()const;
};

#endif // MELODICSTAFF_HH


