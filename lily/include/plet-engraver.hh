/*
  plet-engraver.hh -- declare Plet_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>
*/

#ifndef PLET_ENGRAVER_HH
#define PLET_ENGRAVER_HH

#include "engraver.hh"
#include "drul-array.hh"
#include "plet-spanner.hh"

/**
  Generate a plet.
  Should make a Plet_spanner that typesets a nice bracket.
 */
class Plet_engraver : public Engraver
{
public:
  TRANSLATOR_CLONE(Plet_engraver);
  DECLARE_MY_RUNTIME_TYPEINFO;
  Plet_engraver ();

protected:
  virtual void acknowledge_element (Score_elem_info);
  virtual void do_removal_processing();
  virtual void do_process_requests();
  virtual bool do_try_request (Request*);
  virtual void do_pre_move_processing();
  
private:
  Drul_array<Plet_req*> span_reqs_drul_;
  Drul_array<Moment> beam_mom_drul_;
  Drul_array<Moment> span_mom_drul_;
  Plet_spanner* plet_spanner_p_;
};

#endif // PLET_ENGRAVER_HH
