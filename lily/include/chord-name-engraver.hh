/*
  chord-name-engraver.hh -- declare Chord_name_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef CHORD_NAME_ENGRAVER_HH
#define CHORD_NAME_ENGRAVER_HH

#include "engraver.hh"
#include "array.hh"
#include "musical-pitch.hh"
#include "lily-proto.hh"

class Chord_name_engraver : public Engraver 
{
public:
  Chord_name_engraver ();
  VIRTUAL_COPY_CONS (Translator);

protected:
  virtual void do_pre_move_processing ();
  virtual void acknowledge_element (Score_element_info i);
  virtual void do_process_requests ();
  virtual bool do_try_music (Music* m);

private:
  Array<Musical_pitch> pitch_arr_;
  Chord* chord_p_;
  Tonic_req* tonic_req_;
  Inversion_req* inversion_req_;
  Bass_req* bass_req_;
};

#endif // CHORD_NAME_ENGRAVER_HH
