/*
  chord-tremolo-engraver.hh -- declare Chord_tremolo_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
	   Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef Chord_tremolo_ENGRAVER_HH
#define Chord_tremolo_ENGRAVER_HH

#include "engraver.hh"
#include "drul-array.hh"

/**
  Generate an abbreviation beam.  Eat stems.
 */
class Chord_tremolo_engraver : public Engraver
{
public:
  VIRTUAL_COPY_CONS(Translator);
  

  Chord_tremolo_engraver();

protected:
  virtual void do_removal_processing();
  virtual void do_process_requests();
  virtual bool do_try_music (Music*);
  virtual void acknowledge_element (Score_element_info);
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();

private:
  void typeset_beam ();
  Drul_array<Chord_tremolo_req*> reqs_drul_;
  Chord_tremolo_req* prev_start_req_;
  Chord_tremolo* abeam_p_;
  Chord_tremolo* finished_abeam_p_;
};

#endif // Chord_tremolo_ENGRAVER_HH
