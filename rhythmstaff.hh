/*
  rhythmstaf.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef RHYTHMSTAF_HH
#define RHYTHMSTAF_HH

#include "simplestaff.hh"

/// all notes on one line
struct Rhythmic_staff : public Simple_staff
{
    //  Rhythmic_staff();
    Staff_column * create_col(Score_column*);
    virtual void set_output(PScore *);
    virtual Rhythmic_staff*clone()const;
};

/// this does the typesetting
struct Rhythmic_column : public Simple_column {
    virtual void typeset_req(Request *rq);
    virtual void typeset_command(Command *, int brs);
  

//    virtual void typeset_item(Item *rq, int=1);
    Rhythmic_column(Score_column*s,Simple_staff*rs) :
	Simple_column(s,rs) { }
//    virtual Rhythmic_column*clone()const;
};

#endif // RHYTHMSTAF_HH


