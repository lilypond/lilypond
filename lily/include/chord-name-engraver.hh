/*
  chord-name-engraver.hh -- declare Chord_name_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef CHORD_NAME_ENGRAVER_HH
#define CHORD_NAME_ENGRAVER_HH

#include "engraver.hh"
#include "array.hh"
#include "musical-pitch.hh"

#include "lily-proto.hh"

class Chord_name_engraver : public Engraver 
{
protected:
  virtual void do_pre_move_processing ();
  virtual void acknowledge_element (Score_element_info i);
  virtual void do_process_requests ();
  virtual bool do_try_music (Music* m);

public:
  Chord_name_engraver ();
  VIRTUAL_COPY_CONS (Translator);

private:
  Array<Musical_pitch> pitch_arr_;
  Link_array<Text_item> text_p_arr_;
};

#endif // CHORD_NAME_ENGRAVER_HH
