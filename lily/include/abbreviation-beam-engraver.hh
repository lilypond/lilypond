/*
  abbreviation-beam-engraver.hh -- declare Abbreviation_beam_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
	   Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef ABBREVIATION_BEAM_ENGRAVER_HH
#define ABBREVIATION_BEAM_ENGRAVER_HH

#include "engraver.hh"
#include "drul-array.hh"

/**
  Generate an abbreviation beam.  Eat stems.
 */
class Abbreviation_beam_engraver : public Engraver
{
public:
  VIRTUAL_COPY_CONS(Translator);
  

  Abbreviation_beam_engraver();

protected:
  virtual void do_removal_processing();
  virtual void do_process_requests();
  virtual bool do_try_music (Music*);
  virtual void acknowledge_element (Score_element_info);
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();

private:
  void typeset_beam ();
  Drul_array<Abbreviation_beam_req*> reqs_drul_;
  Abbreviation_beam_req* prev_start_req_;
  Abbreviation_beam* abeam_p_;
  Abbreviation_beam* finished_abeam_p_;
};

#endif // ABBREVIATION_BEAM_ENGRAVER_HH
