/*
  abbreviation-beam-engraver.hh -- declare Abbreviation_beam_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
	   Jan Nieuwenhuizen <jan@digicash.com>
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
  TRANSLATOR_CLONE(Abbreviation_beam_engraver);
  DECLARE_MY_RUNTIME_TYPEINFO;

  Abbreviation_beam_engraver();

protected:
  virtual void do_removal_processing();
  virtual void do_process_requests();
  virtual bool do_try_request (Request*);
  virtual void acknowledge_element (Score_elem_info);
  virtual void do_pre_move_processing();

private:
  Drul_array<Abbreviation_beam_req *> span_reqs_drul_;
  Abbreviation_beam* abeam_p_;
};

#endif // ABBREVIATION_BEAM_ENGRAVER_HH
