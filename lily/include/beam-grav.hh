/*
  beam-grav.hh -- declare Beam_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef BEAM_GRAV_HH
#define BEAM_GRAV_HH

#include "engraver.hh"
#include "drul-array.hh"

/**
  Generate a beam. Eats stems.
 */
class Beam_engraver : public Engraver
{
  Drul_array<Beam_req *> span_reqs_drul_;
  Beam *beam_p_;
  Rhythmic_grouping *current_grouping_p_;

public:
  TRANSLATOR_CLONE(Beam_engraver);
  DECLARE_MY_RUNTIME_TYPEINFO;
  Beam_engraver();
protected:
  virtual void do_removal_processing();
  virtual void do_process_requests();
  virtual bool do_try_request (Request*);
  virtual void acknowledge_element (Score_elem_info);
  virtual void do_pre_move_processing();
};

#endif // BEAM_GRAV_HH
