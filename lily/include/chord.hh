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
  static int find_tonic_i (Array<Musical_pitch> const*);
  static int find_pitch_i (Array<Musical_pitch> const*, Musical_pitch p);
  static int find_notename_i (Array<Musical_pitch> const*, Musical_pitch p);
  static Array<Musical_pitch> missing_thirds_pitch_arr (Array<Musical_pitch> const* pitch_arr_p);
  static void rebuild_from_base (Array<Musical_pitch>*, int base_i);
  static void rebuild_insert_inversion (Array<Musical_pitch>*, int tonic_i);
  static void rebuild_transpose (Array<Musical_pitch>*, Musical_pitch tonic);
  static void rebuild_with_bass (Array<Musical_pitch>*, int bass_i);
  static int step_i (Musical_pitch tonic, Musical_pitch p);

  Chord (Array<Musical_pitch> pitch_arr, Musical_pitch* inversion_p, Musical_pitch* bass_p);
  Chord (Chord const& chord);
  ~Chord ();


  Array<Musical_pitch> to_pitch_arr () const;

  String banter_str () const;
  void find_additions_and_subtractions(Array<Musical_pitch>* add_arr_p, Array<Musical_pitch>* sub_arr_p) const;

  Array<Musical_pitch> pitch_arr_;
  Musical_pitch* inversion_p_;
  Musical_pitch* bass_p_;
};

Chord to_chord (Musical_pitch tonic, Array<Musical_pitch>* add_arr_p, Array<Musical_pitch>* sub_arr_p, Musical_pitch* inversion_p, Musical_pitch* bass_p);

Chord to_chord (Array<Musical_pitch> pitch_arr, Tonic_req* tonic_req, Inversion_req* inversion_req, Bass_req* bass_req, bool find_inversion_b);

#endif // CHORD_HH
