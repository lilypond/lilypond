/*
  chord.cc -- implement Chord

  source file of the GNU LilyPond music typesetter

  (c)  1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "chord.hh"
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

/*
  construct from parser output
*/
Chord
to_chord (Musical_pitch tonic, Array<Musical_pitch>* add_arr_p, Array<Musical_pitch>* sub_arr_p, Musical_pitch* inversion_p, Musical_pitch* bass_p)
{
  // urg: catch dim modifier: 3rd, 5th, 7th, .. should be lowered
  bool dim_b = false;
  for (int i=0; i < add_arr_p->size (); i++)
    {
      Musical_pitch* p = &(*add_arr_p)[i];
      if (p->octave_i_ == -100)
        {
          p->octave_i_ = 0;
	  dim_b = true;
	}
    }
  Chord::rebuild_transpose (add_arr_p, tonic, true);
  Chord::rebuild_transpose (sub_arr_p, tonic, true);

  Musical_pitch fifth = Chord::base_arr (tonic).top ();

  /*
    remove double adds (urg: sus4)
   */
  for (int i = add_arr_p->size () - 1; i >= 0 ; i--)
    {
      int j = Chord::find_pitch_i (add_arr_p, (*add_arr_p)[i]);
      if ((j != -1) && (i != j))
        {
	    add_arr_p->get (i);
	} 
    }

  /*
    default chord includes upto 5: <1, 3, 5>
   */
  add_arr_p->insert (tonic, 0);
  Array<Musical_pitch> tmp = *add_arr_p;
  int highest_step = Chord::step_i (tonic, tmp.top ());
  if (highest_step < 5)
    tmp.push (fifth);
  else if (dim_b)
    {
      Musical_pitch* p = &add_arr_p->top ();
      p->accidental_i_--;
    }

  /*
    find missing thirds
   */
  Array<Musical_pitch> missing_arr = Chord::missing_thirds_pitch_arr (&tmp);
  if (highest_step < 5)
    missing_arr.push (fifth);

  /*
    if dim modifier is given: lower all missing
   */
  if (dim_b)
    {
      for (int i=0; i < missing_arr.size (); i++)
        {
	  missing_arr[i].accidental_i_--;
	}
    }

  /*
    if additions include some 3, don't add third
   */
  Musical_pitch third = Chord::base_arr (tonic)[1];
  if (Chord::find_notename_i (add_arr_p, third) != -1)
    {
      int i = Chord::find_pitch_i (&missing_arr, third);
      if (i != -1)
	missing_arr.get (i);
    }
  
  /*
    if additions include 4, assume sus4 and don't add third implicitely
     C-sus (4) = c f g (1 4 5)
   */
  Musical_pitch sus = tonic;
  sus.transpose (Musical_pitch (3));
  if (Chord::find_pitch_i (add_arr_p, sus) != -1)
    {
      int i = Chord::find_pitch_i (&missing_arr, third);
      if (i != -1)
	missing_arr.get (i);
    }

  /*
    if additions include some 5, don't add fifth
   */
  if (Chord::find_notename_i (add_arr_p, fifth) != -1)
    {
      int i = Chord::find_pitch_i (&missing_arr, fifth);
      if (i != -1)
	missing_arr.get (i);
    }
  
  
  /*
    complete the list of thirds to be added
   */
  add_arr_p->concat (missing_arr);
  add_arr_p->sort (Musical_pitch::compare);

  Array<Musical_pitch> pitch_arr;
  /*
   add all that aren't subtracted
   */
  for (int i = 0; i < add_arr_p->size (); i++)
    {
      Musical_pitch p = (*add_arr_p)[i];
      int j = 0;
      for (; j < sub_arr_p->size (); j++)
	if (p == (*sub_arr_p)[j])
	  {
	    sub_arr_p->del (j);
	    j = -1;
	    break;
	  }
      if (j == sub_arr_p->size ())
	pitch_arr.push (p);
    }

  pitch_arr.sort (Musical_pitch::compare);

  for (int i = 0; i < sub_arr_p->size (); i++)
    warning (_f ("invalid subtraction: not part of chord: %s",
		 (*sub_arr_p)[i].str ()));
 
  return Chord (pitch_arr, inversion_p, bass_p);
}

/*
  Construct from list of pitches and requests
 */
Chord
to_chord (Array<Musical_pitch> pitch_arr, Tonic_req* tonic_req, Inversion_req* inversion_req, Bass_req* bass_req, bool find_inversion_b)
{
  Musical_pitch* inversion_p = 0;
  Musical_pitch* bass_p = 0;

  if (bass_req)
    {
      assert (pitch_arr[0].notename_i_ == bass_req->pitch_.notename_i_);
      bass_p = new Musical_pitch (pitch_arr.get (0));
    }
    
  if (inversion_req)
    {
      assert (pitch_arr[0].notename_i_ == inversion_req->pitch_.notename_i_);
      inversion_p = new Musical_pitch (inversion_req->pitch_);
      assert (tonic_req);
      int tonic_i = Chord::find_notename_i (&pitch_arr, tonic_req->pitch_);
      if (tonic_i)
	Chord::rebuild_insert_inversion (&pitch_arr, tonic_i);
    }
    
  if (find_inversion_b && !inversion_p)
    {
      int tonic_i = tonic_req
	? Chord::find_notename_i (&pitch_arr, tonic_req->pitch_) 
	: Chord::find_tonic_i (&pitch_arr);
	
      if (tonic_i)
	{
	  inversion_p = &pitch_arr[0];
	  Chord::rebuild_insert_inversion (&pitch_arr, tonic_i);
	}
    }

  if (tonic_req)
    {
      assert (pitch_arr[0].notename_i_ == tonic_req->pitch_.notename_i_);
    }

  return Chord (pitch_arr, inversion_p, bass_p);
}

Chord::Chord (Array<Musical_pitch> pitch_arr, Musical_pitch* inversion_p, Musical_pitch* bass_p)
{
  pitch_arr_ = pitch_arr;
  inversion_p_ = inversion_p;
  bass_p_ = bass_p;
}

Chord::Chord (Chord const& chord)
  : Item (chord)
{
  pitch_arr_ = chord.pitch_arr_;
  inversion_p_ = chord.inversion_p_ ? new Musical_pitch (*chord.inversion_p_) : 0;
  bass_p_ = chord.bass_p_ ? new Musical_pitch (*chord.bass_p_) : 0;
}

Chord::~Chord ()
{
  delete inversion_p_;
  delete bass_p_;
  // AAARGH, why doesn't Score_elt do this?
  unsmobify_self ();
}

Array<Musical_pitch>
Chord::base_arr (Musical_pitch p)
{
  Array<Musical_pitch> base;
  base.push (p);
  p.transpose (Musical_pitch (2));
  base.push (p);
  p.transpose (Musical_pitch (2, -1));
  base.push (p);
  return base;
}

void
Chord::rebuild_transpose (Array<Musical_pitch>* pitch_arr_p, Musical_pitch tonic, bool fix7_b)
{
  for (int i = 0; i < pitch_arr_p->size (); i++)
    {
      Musical_pitch p = tonic;
      Musical_pitch q = (*pitch_arr_p)[i];
      p.transpose (q);
      // duh, c7 should mean <c bes>
      if (fix7_b && (step_i (tonic, p) == 7))
        p.accidental_i_--;
      (*pitch_arr_p)[i] = p;
    }
  pitch_arr_p->sort (Musical_pitch::compare);
}

int
Chord::find_pitch_i (Array<Musical_pitch> const* pitch_arr_p, Musical_pitch p)
{
  for (int i = 0; i < pitch_arr_p->size (); i++)
    if (p == (*pitch_arr_p)[i])
      return i;
  return -1;
}

int
Chord::find_notename_i (Array<Musical_pitch> const* pitch_arr_p, Musical_pitch p)
{
  int i = find_pitch_i (pitch_arr_p, p);
  if (i == -1)
    {
      for (int i = 0; i < pitch_arr_p->size (); i++)
	{
	  p.octave_i_ = (*pitch_arr_p)[i].octave_i_;
	  if (p == (*pitch_arr_p)[i])
	    return i;
	}
    }
  return i;
}

int
Chord::step_i (Musical_pitch tonic, Musical_pitch p)
{
  int i = p.notename_i_ - tonic.notename_i_
    + (p.octave_i_ - tonic.octave_i_) * 7;
  while (i < 0)
    i += 7;
  i++;
  return i;
}

Array<Musical_pitch>
Chord::missing_thirds_pitch_arr (Array<Musical_pitch> const* pitch_arr_p)
{
  Array<Musical_pitch> thirds;

  /* is the third c-e, d-f, etc. small or large? */
  int minormajor_a[] = {0, -1, -1, 0,0,-1,-1};
  for (int i=0; i < 7; i++)
    thirds.push (Musical_pitch( 2, minormajor_a[i]));

  Musical_pitch tonic = (*pitch_arr_p)[0];
  Musical_pitch last = tonic;
  Array<Musical_pitch> missing_arr;

  for (int i = 0; i < pitch_arr_p->size ();)
    {
      Musical_pitch p = (*pitch_arr_p)[i];
      int step = step_i (tonic, p);
      if (last.notename_i_ == p.notename_i_)
	last.transpose (thirds[(last.notename_i_ - tonic.notename_i_ + 7) % 7]);
      if (step > step_i (tonic, last))
	{
	  while (step > step_i (tonic, last))
	    {
	      if ((last.notename_i_ - tonic.notename_i_ + 7) % 7 == 6)
		{
		  Musical_pitch special_seven = last;
		  Musical_pitch lower (0, -1);
		  special_seven.transpose (lower);
		  missing_arr.push (special_seven);
		}
	      else
		{
		  missing_arr.push (last);
		}
	      last.transpose (thirds[(last.notename_i_ - tonic.notename_i_ + 7) % 7]);
	    }
	}
      else
	{
	  i++;
	}
    }
  return missing_arr;
}


/*
 Mangle into list of pitches.
 For normal chord entry, inversion and bass pitches are retained in
 specific *_requests
 */
Array<Musical_pitch>
Chord::to_pitch_arr () const
{
  Array<Musical_pitch> pitch_arr = pitch_arr_;
  if (inversion_p_)
    {
      int i = 0;
      for (; i < pitch_arr.size (); i++)
	{
	  if ((pitch_arr[i].notename_i_ == inversion_p_->notename_i_)
	      && (pitch_arr[i].accidental_i_ == inversion_p_->accidental_i_))
	    break;
	}
      if (i == pitch_arr.size ())
	{
	  warning (_f ("invalid inversion pitch: not part of chord: %s",
		       inversion_p_->str ()));
	}
      else
	rebuild_with_bass (&pitch_arr, i);
    }

  if (bass_p_)
    {
      pitch_arr.insert (*bass_p_, 0);
      rebuild_with_bass (&pitch_arr, 0);
    }
  return pitch_arr;
}

void
Chord::find_additions_and_subtractions (Array<Musical_pitch> pitch_arr, Array<Musical_pitch>* add_arr_p, Array<Musical_pitch>* sub_arr_p) const
{
  Musical_pitch tonic = pitch_arr[0];
  /*
    construct an array of thirds for a normal chord
   */
  Array<Musical_pitch> all_arr;
  all_arr.push (tonic);
  if (step_i (tonic, pitch_arr.top ()) >= 5)
    all_arr.push (pitch_arr.top ());
  else
    all_arr.push (base_arr (tonic).top ());
  all_arr.concat (missing_thirds_pitch_arr (&all_arr));
  all_arr.sort (Musical_pitch::compare);
  
  int i = 0;
  int j = 0;
  Musical_pitch last_extra = tonic;
  while ((i < all_arr.size ()) || (j < pitch_arr.size ()))
    {
      Musical_pitch a = all_arr [i <? all_arr.size () - 1];
      Musical_pitch p = pitch_arr[j <? pitch_arr.size () - 1];
      /*
        this pitch is present: do nothing, check next
       */
      if (a == p)
	{
	  i++;
	  j++;
	  last_extra = tonic;
	}
      /*
        found an extra pitch: chord addition
       */
      else if ((p < a) || (p.notename_i_ == a.notename_i_))
	{
	  add_arr_p->push (p);
	  last_extra = p;
	  (j < pitch_arr.size ()) ? j++ : i++;
	}
      /*
        a third is missing: chord subtraction
       */
      else
	{
	  if (last_extra.notename_i_ != a.notename_i_)
	    sub_arr_p->push (a);
	  (i < all_arr.size ()) ? i++ : j++;
	  last_extra = tonic;
	}
    }
      
  /* add missing basic steps */
  if (step_i (tonic, pitch_arr.top ()) < 3)
    sub_arr_p->push (base_arr (tonic)[1]);
  if (step_i (tonic, pitch_arr.top ()) < 5)
    sub_arr_p->push (base_arr (tonic).top ());

  /*
    add highest addition, because it names chord, if greater than 5
    or non-standard
    (1, 3 and) 5 not additions: part of normal chord
   */
  if ((step_i (tonic, pitch_arr.top ()) > 5)
       || pitch_arr.top ().accidental_i_)
    add_arr_p->push (pitch_arr.top ());
}


/*
  word is roman text or styled text:
   "text"
   ("style" . "text")
 */
Molecule
Chord::ly_word2molecule (SCM scm) const
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
Chord::ly_text2molecule (SCM scm) const
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
Chord::pitch2molecule (Musical_pitch p) const
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
Chord::user_chord_name (Array<Musical_pitch> pitch_arr, Chord_name* name_p) const
{
  SCM chord = SCM_EOL;
  Array<Musical_pitch> chord_type = pitch_arr;
  rebuild_transpose (&chord_type, diff_pitch (pitch_arr[0], Musical_pitch (0)), false);

  for (int i= chord_type.size (); i--; )
    chord = gh_cons (pitch2scm (chord_type[i]), chord);

  SCM name = scm_eval (gh_list (ly_symbol2scm ("user-chord-name"), ly_quote_scm (chord), SCM_UNDEFINED));
  if (name != SCM_UNSPECIFIED)
    {
      name_p->modifier_mol = ly_text2molecule (gh_car (name));
      name_p->addition_mol = ly_text2molecule (gh_cdr (name));
      return true;
    }
  return false;
}

void
Chord::banter (Array<Musical_pitch> pitch_arr, Chord_name* name_p) const
{
  Array<Musical_pitch> add_arr;
  Array<Musical_pitch> sub_arr;
  find_additions_and_subtractions (pitch_arr, &add_arr, &sub_arr);
			   
  Array<Musical_pitch> scale;
  for (int i=0; i < 7; i++)
    scale.push (Musical_pitch (i));

  Musical_pitch tonic = pitch_arr[0];
  rebuild_transpose (&scale, tonic, true);
  
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
      int step = step_i (tonic, p);
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
      int step = step_i (tonic, p);
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

/*
  This routine tries to guess tonic in a possibly inversed chord, ie
  <e g c'> should produce: C.
  This is only used for chords that are entered as simultaneous notes,
  chords entered in \chord mode are fully defined.
 */
int
Chord::find_tonic_i (Array<Musical_pitch> const* pitch_arr_p)
{
  /*
    find tonic
    
    first try: base of longest line of thirds
   */
  int tonic_i = 0;
  int longest_i = 0;
  for (int i = 0; i < pitch_arr_p->size (); i++)
    {
      int no_third_i = 0;
      int last_i = (*pitch_arr_p)[i % pitch_arr_p->size ()].notename_i_;
      int j = 0;
      for (; j < pitch_arr_p->size (); j++)
	{
	  int cur_i = (*pitch_arr_p)[(i + j + 1) % pitch_arr_p->size ()].notename_i_;
	  int gap = cur_i - last_i;
	  while (gap < 0)
	    gap += 7;
	  gap %= 7;
	  if (gap == 2)
	    last_i = cur_i;
	  else
	    no_third_i++;
	}
      if (j - no_third_i > longest_i)
	{
	  longest_i = j - no_third_i;
	  tonic_i = i;
	}
    }

  /*
    second try: note after biggest gap
   */
  int biggest_i = 0;
  //  if (longest_i)
  if (longest_i <= 1)
    for (int i = 0; i < pitch_arr_p->size (); i++)
      {
	int gap = (*pitch_arr_p)[i].notename_i_
	  - (*pitch_arr_p)[(i - 1 + pitch_arr_p->size ()) 
	  % pitch_arr_p->size ()].notename_i_;
	while (gap < 0)
	  gap += 7;
	gap %= 7;
	if (gap > biggest_i)
	  {
	    biggest_i = gap;
	    tonic_i = i;
	  }
      }
  return tonic_i;
}

void
Chord::rebuild_from_base (Array<Musical_pitch>* pitch_arr_p, int base_i)
{
  assert (base_i >= 0);
  Musical_pitch last (0, 0, -5);
  Array<Musical_pitch> new_arr;
  for (int i = 0; i < pitch_arr_p->size (); i++)
    {
      Musical_pitch p = (*pitch_arr_p)[(base_i + i) % pitch_arr_p->size ()];
      if (p < last)
	{
	  p.octave_i_ = last.octave_i_;
	  if (p < last)
	    p.octave_i_++;
	}
      new_arr.push (p);
      last = p;
    }
  *pitch_arr_p = new_arr;
}

void
Chord::rebuild_insert_inversion (Array<Musical_pitch>* pitch_arr_p, int tonic_i)
{
  assert (tonic_i > 0);
  Musical_pitch inversion = pitch_arr_p->get (0);
  rebuild_from_base (pitch_arr_p, tonic_i - 1);
  if (pitch_arr_p->size ())
    {
      inversion.octave_i_ = (*pitch_arr_p)[0].octave_i_ - 1;
      while (inversion < (*pitch_arr_p)[0])
	inversion.octave_i_++;
    }
  for (int i = 0; i < pitch_arr_p->size (); i++)
    if ((*pitch_arr_p)[i] > inversion)
      {
	pitch_arr_p->insert (inversion, i);
	break;
      }
}

void
Chord::rebuild_with_bass (Array<Musical_pitch>* pitch_arr_p, int bass_i)
{
  assert (bass_i >= 0);
  Musical_pitch bass = pitch_arr_p->get (bass_i);
  // is lowering fine, or should others be raised?
  if (pitch_arr_p->size ())
    while (bass > (*pitch_arr_p)[0])
      bass.octave_i_--;
  pitch_arr_p->insert (bass, 0);
}

Molecule*
Chord::do_brew_molecule_p () const
{
  Musical_pitch tonic = pitch_arr_[0];
  
  Chord_name name;
  name.tonic_mol = pitch2molecule (tonic);

  /*
    if user has explicitely listed chord name, use that
    
    TODO
    urg
    maybe we should check all sub-lists of pitches, not
    just full list and base triad?
   */
  if (!user_chord_name (pitch_arr_, &name))
    {
      /*
        else, check if user has listed base triad
	use user base name and add banter for remaining part
       */
      if ((pitch_arr_.size () > 2)
	  && user_chord_name (pitch_arr_.slice (0, 3), &name))
        {
	  Array<Musical_pitch> base = base_arr (tonic);
	  base.concat (pitch_arr_.slice (3, pitch_arr_.size ()));
	  banter (base, &name);
	}
      /*
        else, use pure banter
       */
      else
	{
	  banter (pitch_arr_, &name);
	}
    }

  if (inversion_p_)
    {
      name.inversion_mol = lookup_l ()->text ("", "/", paper_l ());
      // zucht  const&
      Molecule mol = pitch2molecule (*inversion_p_);
      name.inversion_mol.add_at_edge (X_AXIS, RIGHT, mol, 0);
    }

  if (bass_p_)
    {
      name.bass_mol = lookup_l ()->text ("", "/", paper_l ());
      Molecule mol = pitch2molecule (*bass_p_);
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

void
Chord::do_print () const
{
#ifndef NPRINT
  //DEBUG_OUT <<  "chord = " ...
#endif
}
