/*
  stem-engraver.hh -- declare Stem_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef STEM_GRAV_HH
#define STEM_GRAV_HH

#include "engraver.hh"

/**
  Make stems upon receiving noteheads.
 */
class Stem_engraver : public Engraver
{
  int default_abbrev_i_;
  Stem *stem_p_;
  Abbreviation *abbrev_p_;
  Rhythmic_req *rhythmic_req_l_;
  Abbreviation_req* abbrev_req_l_;
protected:
  virtual void do_creation_processing ();
  virtual void acknowledge_element (Score_element_info);
  virtual void do_pre_move_processing ();
  virtual bool do_try_music (Music*);

public:
  VIRTUAL_COPY_CONS(Translator);
  Stem_engraver();
  
};

#endif // STEM_GRAV_HH
