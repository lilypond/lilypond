/*
  chord-name.hh -- declare Chord_name

  source file of the GNU LilyPond music typesetter

  (c) 1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef CHORD_NAME_HH
#define CHORD_NAME_HH

#include "chord.hh"
#include "item.hh"
#include "molecule.hh"

class Chord_mol
{
public:
  Molecule tonic_mol;
  Molecule modifier_mol;
  Molecule addition_mol;
  Molecule inversion_mol;
  Molecule bass_mol;
};

class Chord_name : public Item
{
public:
  VIRTUAL_COPY_CONS (Score_element);
  Molecule ly_word2molecule (SCM scm) const;
  Molecule ly_text2molecule (SCM scm) const;
  Molecule pitch2molecule (Musical_pitch p) const;
  bool user_chord_name (Array<Musical_pitch> pitch_arr, Chord_mol* name_p) const;
  void banter (Array<Musical_pitch> pitch_arr, Chord_mol* name_p) const;

  Chord_name (Chord const& c);
  Chord chord_;

protected:
  virtual Molecule* do_brew_molecule_p () const;
};

#endif // CHORD_NAME_HH
