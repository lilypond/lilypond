/*
  chord-name.cc -- implement Chord_name

  source file of the GNU LilyPond music typesetter

  (c)  1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "chord-name.hh"
#include "musical-request.hh"
#include "warn.hh"
#include "debug.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"

SCM
pitch2scm (Musical_pitch p)
{
  return gh_cons (gh_int2scm (p.notename_i_), gh_int2scm (p.accidental_i_));
}

Chord_name::Chord_name (Chord const& c)
{
  chord_ = c;
}

/*
  word is roman text or styled text:
   "text"
   ("style" . "text")
 */
Molecule
Chord_name::ly_word2molecule (SCM scm) const
{
  String style;
  if (gh_pair_p (scm))
    {
      style = ly_scm2string (gh_car (scm));
      scm = gh_cdr (scm);
    }
  String text = ly_scm2string (scm);
  return lookup_l ()->text (style, text, paper_l ());
}

/*
 scm is word or list of words:
   word
   (word word)
 */
Molecule
Chord_name::ly_text2molecule (SCM scm) const
{
  Molecule mol;
  if (gh_list_p (scm))
    {
      while (gh_cdr (scm) != SCM_EOL)
        {
	  mol.add_at_edge (X_AXIS, RIGHT, 
            ly_word2molecule (gh_car (scm)), 0);
	  scm = gh_cdr (scm);
	}
      scm = gh_car (scm);
    }  
  mol.add_at_edge (X_AXIS, RIGHT, 
    ly_word2molecule (scm), 0);
  return mol;
}

Molecule
Chord_name::pitch2molecule (Musical_pitch p) const
{
  SCM name = scm_eval (gh_list (ly_symbol2scm ("user-pitch-name"), ly_quote_scm (pitch2scm (p)), SCM_UNDEFINED));

  if (name != SCM_UNSPECIFIED)
    {
      return ly_text2molecule (name);
    }

  Molecule mol = lookup_l ()->text ("", p.str ().left_str (1).upper_str (), paper_l ());

  /*
    We want the smaller size, even if we're big ourselves.
   */
  if (p.accidental_i_)
    mol.add_at_edge (X_AXIS, RIGHT, 
		     
		     paper_l ()->lookup_l (-2)->afm_find (String ("accidentals-") + to_str (p.accidental_i_)), 0.0);
  return mol;
}

Musical_pitch
diff_pitch (Musical_pitch tonic, Musical_pitch  p)
{
  Musical_pitch diff (p.notename_i_ - tonic.notename_i_, 
    p.accidental_i_ - tonic.accidental_i_, 
    p.octave_i_ - tonic.octave_i_);

  while  (diff.notename_i_ >= 7)
    {
      diff.notename_i_ -= 7;
      diff.octave_i_ ++;
    }
  while  (diff.notename_i_ < 0)
    {
      diff.notename_i_ += 7;
      diff.octave_i_ --;
    }

  diff.accidental_i_ -= (tonic.semitone_pitch () + diff.semitone_pitch ())
    - p.semitone_pitch ();

  return diff;
}

bool
Chord_name::user_chord_name (Array<Musical_pitch> pitch_arr, Chord_mol* name_p) const
{
  SCM chord = SCM_EOL;
  Array<Musical_pitch> chord_type = pitch_arr;
  Chord::rebuild_transpose (&chord_type, diff_pitch (pitch_arr[0], Musical_pitch (0)), false);

  for (int i= chord_type.size (); i--; )
    chord = gh_cons (pitch2scm (chord_type[i]), chord);

  SCM name = scm_eval (gh_list (ly_symbol2scm ("user-chord-name"), ly_quote_scm (chord), SCM_UNDEFINED));
  if (gh_pair_p (name))
    {
      name_p->modifier_mol = ly_text2molecule (gh_car (name));
      name_p->addition_mol = ly_text2molecule (gh_cdr (name));
      return true;
    }
  return false;
}

void
Chord_name::banter (Array<Musical_pitch> pitch_arr, Chord_mol* name_p) const
{
  Array<Musical_pitch> add_arr;
  Array<Musical_pitch> sub_arr;
  Chord::find_additions_and_subtractions (pitch_arr, &add_arr, &sub_arr);
			   
  Array<Musical_pitch> scale;
  for (int i=0; i < 7; i++)
    scale.push (Musical_pitch (i));

  Musical_pitch tonic = pitch_arr[0];
  chord_.rebuild_transpose (&scale, tonic, true);
  
  /*
    Does chord include this step?  -1 if flat
   */
  int has[16];
  for (int i=0; i<16; i++)
    has[i] = 0;

  String mod_str;
  String add_str;
  String sep_str;
  for (int i = 0; i < add_arr.size (); i++)
    {
      Musical_pitch p = add_arr[i];
      int step = Chord::step_i (tonic, p);
      int accidental = p.accidental_i_ - scale[(step - 1) % 7].accidental_i_;
      if ((step < 16) && (has[step] != -1))
        has[step] = accidental == -1 ? -1 : 1;
      // only from guile table ?
      if ((step == 3) && (accidental == -1))
	{
	  mod_str = "m";
	}
      else if (accidental
	       || (!(step % 2) 
	       || ((i == add_arr.size () - 1) && (step > 5))))
        {
	  add_str += sep_str;
	  sep_str = "/";
          if ((step == 7) && (accidental == 1))
	    {
              add_str += "maj7";
	    }
	  else
	    {
	      add_str += to_str (step);
	      if (accidental)
		add_str += accidental < 0 ? "-" : "+";
	    }
	}
    }

  for (int i = 0; i < sub_arr.size (); i++)
    {
      Musical_pitch p = sub_arr[i];
      int step = Chord::step_i (tonic, p);
      /*
	if additions include 2 or 4, assume sus2/4 and don't display 'no3'
      */
      if (!((step == 3) && (has[2] || has[4])))
	{
	  add_str += sep_str + "no" + to_str (step);
	  sep_str = "/";
	}
    }

  if (mod_str.length_i ())
    name_p->modifier_mol.add_at_edge (X_AXIS, RIGHT, 
      lookup_l ()->text ("roman", mod_str, paper_l ()), 0);
  if (add_str.length_i ())
    {
      if (!name_p->addition_mol.empty_b ())
        add_str = "/" + add_str;
      name_p->addition_mol.add_at_edge (X_AXIS, RIGHT,
       lookup_l ()->text ("script", add_str, paper_l ()), 0);
    }
}

Molecule*
Chord_name::do_brew_molecule_p () const
{
  Musical_pitch tonic = chord_.pitch_arr_[0];
  
  Chord_mol name;
  name.tonic_mol = pitch2molecule (tonic);

  /*
    if user has explicitely listed chord name, use that
    
    TODO
    urg
    maybe we should check all sub-lists of pitches, not
    just full list and base triad?
   */
  if (!user_chord_name (chord_.pitch_arr_, &name))
    {
      /*
        else, check if user has listed base triad
	use user base name and add banter for remaining part
       */
      if ((chord_.pitch_arr_.size () > 2)
	  && user_chord_name (chord_.pitch_arr_.slice (0, 3), &name))
        {
	  Array<Musical_pitch> base = Chord::base_arr (tonic);
	  base.concat (chord_.pitch_arr_.slice (3, chord_.pitch_arr_.size ()));
	  banter (base, &name);
	}
      /*
        else, use pure banter
       */
      else
	{
	  banter (chord_.pitch_arr_, &name);
	}
    }

  if (chord_.inversion_b_)
    {
      name.inversion_mol = lookup_l ()->text ("", "/", paper_l ());
      // zucht  const&
      Molecule mol = pitch2molecule (chord_.inversion_pitch_);
      name.inversion_mol.add_at_edge (X_AXIS, RIGHT, mol, 0);
    }

  if (chord_.bass_b_)
    {
      name.bass_mol = lookup_l ()->text ("", "/", paper_l ());
      Molecule mol = pitch2molecule (chord_.bass_pitch_);
      name.bass_mol.add_at_edge (X_AXIS, RIGHT, mol, 0);
    }

  // urg, howto get a good superscript_y?
  Real super_y = lookup_l ()->text ("", "x", paper_l ()).dim_.y ().length ()/2;
  if (!name.addition_mol.empty_b ())
    name.addition_mol.translate (Offset (0, super_y));

  Molecule* mol_p = new Molecule;
  mol_p->add_at_edge (X_AXIS, RIGHT, name.tonic_mol, 0);
  // huh?
  if (!name.modifier_mol.empty_b ())
    mol_p->add_at_edge (X_AXIS, RIGHT, name.modifier_mol, 0);
  if (!name.addition_mol.empty_b ())
    mol_p->add_at_edge (X_AXIS, RIGHT, name.addition_mol, 0);
  if (!name.inversion_mol.empty_b ())
    mol_p->add_at_edge (X_AXIS, RIGHT, name.inversion_mol, 0);
  if (!name.bass_mol.empty_b ())
    mol_p->add_at_edge (X_AXIS, RIGHT, name.bass_mol, 0);
  return mol_p;
}
