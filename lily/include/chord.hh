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
#include "item.hh"
#include "molecule.hh"

class Chord_name
{
public:
  Molecule tonic_mol;
  Molecule modifier_mol;
  Molecule addition_mol;
  Molecule inversion_mol;
  Molecule bass_mol;
};

class Chord : public Item
{
public:
  VIRTUAL_COPY_CONS (Score_element);
  static Array<Musical_pitch> base_arr (Musical_pitch p);
  static int find_tonic_i (Array<Musical_pitch> const*);
  static int find_pitch_i (Array<Musical_pitch> const*, Musical_pitch p);
  static int find_notename_i (Array<Musical_pitch> const*, Musical_pitch p);
  static Array<Musical_pitch> missing_thirds_pitch_arr (Array<Musical_pitch> const* pitch_arr_p);
  static void rebuild_from_base (Array<Musical_pitch>*, int base_i);
  static void rebuild_insert_inversion (Array<Musical_pitch>*, int tonic_i);
  static void rebuild_transpose (Array<Musical_pitch>*, Musical_pitch tonic, bool fix7_b);
  static void rebuild_with_bass (Array<Musical_pitch>*, int bass_i);
  static int step_i (Musical_pitch tonic, Musical_pitch p);

  Chord (Array<Musical_pitch> pitch_arr, Musical_pitch* inversion_p, Musical_pitch* bass_p);
  Chord (Chord const&);
  virtual ~Chord ();


  Array<Musical_pitch> to_pitch_arr () const;

  void find_additions_and_subtractions(Array<Musical_pitch> pitch_arr, Array<Musical_pitch>* add_arr_p, Array<Musical_pitch>* sub_arr_p) const;

  Molecule ly_word2molecule (SCM scm) const;
  Molecule ly_text2molecule (SCM scm) const;
  Molecule pitch2molecule (Musical_pitch p) const;
  bool user_chord_name (Array<Musical_pitch> pitch_arr, Chord_name* name_p) const;
  void banter (Array<Musical_pitch> pitch_arr, Chord_name* name_p) const;

  Array<Musical_pitch> pitch_arr_;
  Musical_pitch* inversion_p_;
  Musical_pitch* bass_p_;

protected:
  virtual Molecule* do_brew_molecule_p () const;
  virtual void do_print () const;
};

Chord to_chord (Musical_pitch tonic, Array<Musical_pitch>* add_arr_p, Array<Musical_pitch>* sub_arr_p, Musical_pitch* inversion_p, Musical_pitch* bass_p);

Chord to_chord (Array<Musical_pitch> pitch_arr, Tonic_req* tonic_req, Inversion_req* inversion_req, Bass_req* bass_req, bool find_inversion_b);

#endif // CHORD_HH
