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

Array<Musical_pitch> rebuild_from_base_pitch_arr (Array<Musical_pitch> pitch_arr, int base_i);
Array<Musical_pitch> rebuild_insert_inversion_pitch_arr (Array<Musical_pitch> pitch_arr, int tonic_i);
Array<Musical_pitch> rebuild_with_bass_pitch_arr (Array<Musical_pitch> pitch_arr, int bass_i);


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
  Link_array<Item> text_p_arr_;

  String banter_str (Array<Musical_pitch> pitch_arr, Musical_pitch* inversion) const;
  int find_tonic_i () const;
  Array<Musical_pitch> rebuild_pitch_arr (int tonic_i) const;
};

#endif // CHORD_NAME_ENGRAVER_HH
