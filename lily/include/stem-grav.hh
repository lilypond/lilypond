/*
  stem-grav.hh -- declare Stem_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef STEM_GRAV_HH
#define STEM_GRAV_HH

#include "engraver.hh"

/**
  Make stems upon receiving noteheads.
 */
class Stem_engraver : public Engraver
{
  Direction dir_;
  Stem *stem_p_;
  Rhythmic_req *rhythmic_req_l_;
protected:
  virtual void acknowledge_element (Score_elem_info);
  virtual void do_pre_move_processing ();
  virtual void set_feature (Feature dir_i_);

public:
  Stem_engraver();
  DECLARE_MY_RUNTIME_TYPEINFO;
};

#endif // STEM_GRAV_HH
