/*
  chord.hh -- declare Chord

  source file of the GNU LilyPond music typesetter

  (c) 1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef CHORD_HH
#define CHORD_HH

#include "array.hh"
#include "musical-pitch.hh"
#include "lily-proto.hh"

class Chord
{
public:
  Chord (Array<Musical_pitch> pitch_arr);
  Chord (Musical_pitch tonic, Array<Musical_pitch>* add_arr_p, Array<Musical_pitch>* sub_arr_p, Musical_pitch* inversion_p);

  void rebuild_from_base (int base_i);
  void rebuild_insert_inversion (int tonic_i);
  void rebuild_with_bass (int bass_i);

  String banter_str (Musical_pitch* inversion) const;
  int find_tonic_i () const;
  int find_pitch_i (Musical_pitch p) const;
  int find_notename_i (Musical_pitch p) const;
  void find_additions_and_subtractions(Array<Musical_pitch>* add_arr_p, Array<Musical_pitch>* sub_arr_p) const;

  Array<Musical_pitch> pitch_arr_;
};

#endif // CHORD_HH
