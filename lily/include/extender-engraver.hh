/*
  extender-engraver.hh -- declare Extender_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef EXTENDER_ENGRAVER_HH
#define EXTENDER_ENGRAVER_HH

#include "engraver.hh"
#include "drul-array.hh"
#include "extender-spanner.hh"

/**
  Generate an extender.
  Should make an Extender_spanner that typesets a nice extender line.
 */
class Extender_engraver : public Engraver
{
public:
  Extender_engraver ();
  VIRTUAL_COPY_CONS (Translator);

protected:
  virtual void acknowledge_element (Score_element_info);
  virtual void do_removal_processing();
  virtual void do_process_requests();
  virtual bool do_try_music (Music*);
  virtual void do_pre_move_processing();
  
private:
  Drul_array<Extender_req*> span_reqs_drul_;
  Drul_array<Moment> span_mom_drul_;
  Extender_spanner* extender_spanner_p_;
};

#endif // EXTENDER_ENGRAVER_HH
