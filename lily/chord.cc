/*
  chord.cc -- implement Chord

  source file of the GNU LilyPond music typesetter

  (c)  1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "chord.hh"
#include "warn.hh"

Chord::Chord (Array<Musical_pitch> pitch_arr)
{
  pitch_arr_ = pitch_arr;
}

// construct from parser output
// urg: should split this up into understandable chunks
Chord::Chord (Musical_pitch tonic, Array<Musical_pitch>* add_arr_p, Array<Musical_pitch>* sub_arr_p, Musical_pitch* inversion_p)
{
  for (int i = 0; i < add_arr_p->size (); i++)
    {
      Musical_pitch p = tonic;
      Musical_pitch q = (*add_arr_p)[i];
      // duh, c7 should mean <c bes>
      if (q.notename_i_ == 6)
        q.accidental_i_--;
      p.transpose (q);
      (*add_arr_p)[i] = p;
    }
  add_arr_p->sort (Musical_pitch::compare);
  for (int i = 0; i < sub_arr_p->size (); i++)
    {
      Musical_pitch p = tonic;
      Musical_pitch q = (*sub_arr_p)[i];
      // duh, c7 should mean <c bes>
      if (q.notename_i_ == 6)
        q.accidental_i_--;
      p.transpose (q);
      (*sub_arr_p)[i] = p;
    }
  sub_arr_p->sort (Musical_pitch::compare);

  Musical_pitch third (2);
  Musical_pitch mthird (2, -1);
  Musical_pitch missing;
  missing = tonic;
  missing.transpose (third);

  Musical_pitch p;
  p = tonic;
  p.transpose (third);
  p.transpose (mthird);

  /*
   must have minimum at 5 (3 is added automatically as missing)
   */
  if (!add_arr_p->size ())
    add_arr_p->push (p);
  else if ((add_arr_p->top () < p) && (add_arr_p->top ().notename_i_ != p.notename_i_))
    add_arr_p->push (p);
  add_arr_p->sort (Musical_pitch::compare);

  Array<Musical_pitch> triads;
  triads.push (third);   // c e 
  triads.push (mthird);  // d f 
  triads.push (mthird);  // e g 
  triads.push (third);   // f a 
  triads.push (third);   // g b 
  triads.push (mthird);  // a c 
  triads.push (mthird);  // b d 

  /*
    if first addition is 4, assume sus4 and don't add third implicitely
   */
  Musical_pitch sus (3);
  sus.transpose (tonic);
  if (add_arr_p->size ())
    if ((*add_arr_p)[0] == sus)
      missing.transpose (mthird);

  /*
   add missing triads
   */
  for (int i = 0; i < add_arr_p->size ();)
    {
      Musical_pitch p = (*add_arr_p)[i];
      if (p > missing)
        while (p > missing)
	  {
	    if (p.notename_i_ != missing.notename_i_)
	      {
	        if ((missing.notename_i_ - tonic.notename_i_ + 7) % 7 == 6)
		  {
		    Musical_pitch special_seven = missing;
		    Musical_pitch lower (0, -1);
		    special_seven.transpose (lower);
		    add_arr_p->insert (special_seven, i++);
		  }
		else
		  add_arr_p->insert (missing, i++);
	      }
	    missing.transpose (triads[(missing.notename_i_ - tonic.notename_i_ + 7) % 7]);
	  }
      else if (p.notename_i_ == missing.notename_i_)
        missing.transpose (triads[(missing.notename_i_ - tonic.notename_i_ + 7) % 7]);
      else
	i++;
    }

  /*
    add tonic
   */
  if (!add_arr_p->size () || ((*add_arr_p)[0] != tonic))
    add_arr_p->insert (tonic, 0);

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
        pitch_arr_.push (p);
    }

  for (int i = 0; i < sub_arr_p->size (); i++)
    warning (_f ("invalid subtraction: not part of chord: %s",
		 (*sub_arr_p)[i].str ()));

  if (inversion_p)
    {
      int i = 0;
      for (; i < pitch_arr_.size (); i++)
	if ((pitch_arr_[i].notename_i_ == inversion_p->notename_i_)
	  && (pitch_arr_[i].accidental_i_ == inversion_p->accidental_i_))
	  break;
      if (i == pitch_arr_.size ())
	warning (_f ("invalid inversion pitch: not part of chord: %s",
		      inversion_p->str ()));
      else
        {
	  /*
	    urg
	    should be run-time switchable "chordInversionPreserve", howto?

	    there are two ways commonly used to rearrange a chord with
	    an inversion:

	    1. rebuild pitch list, taking inversion as base
	      */
#if 0
	  rebuild_from_base (i);
#else
	  /*
	    or
	    2. insert inversion as lowest (at first position)
	  */
	  rebuild_with_bass (i);
#endif
	}
      delete inversion_p;
    }
}

String
Chord::banter_str (Musical_pitch* inversion) const
{
  Musical_pitch tonic = pitch_arr_[0];

  Array<Musical_pitch> scale;
  scale.push (Musical_pitch (0)); // c
  scale.push (Musical_pitch (1)); // d
  scale.push (Musical_pitch (2)); // e
  scale.push (Musical_pitch (3)); // f
  scale.push (Musical_pitch (4)); // g
  scale.push (Musical_pitch (5)); // a
  // 7 always means 7-...
  scale.push (Musical_pitch (6, -1)); // b


  for (int i = 0; i < scale.size (); i++)
    scale[i].transpose (tonic);

  //urg, should do translation in scheme.
  char const *acc[] = {"\\textflat\\textflat ", "\\textflat ", "", "\\textsharp " , "\\textsharp\\textsharp "};
  String tonic_str = tonic.str ();
  tonic_str = tonic_str.left_str (1).upper_str ()
    + acc[tonic.accidental_i_ + 2];

  String add_str;
  String sub_str;
  String sep_str;
  String sub_sep_str;
  int last_trap = 1;
  for (int i=1; i < pitch_arr_.size (); i++)
    {
      Musical_pitch p = pitch_arr_[i];
      int trap = p.notename_i_ - tonic.notename_i_
        + (p.octave_i_ - tonic.octave_i_) * 7;
      while (trap < 0)
        trap += 7;
      trap++;
      while (trap - last_trap > 2)
	{
	  last_trap += 2;
	  sub_str += sub_sep_str + "no" + to_str (last_trap);
	  sub_sep_str = "/";
	}
      last_trap = trap;
      int accidental = p.accidental_i_ - scale[(trap - 1) % 7].accidental_i_;
      if ((trap == 3) && (accidental == -1))
        tonic_str += "m"; // hmm
      else if (accidental || (!(trap % 2) || ((i + 1 == pitch_arr_.size ()) && (trap > 5))))
        {
	  add_str += sep_str;
          if ((trap == 7) && (accidental == 1))
            add_str += "maj7";
          else
            {
              add_str += to_str (trap);
              if (accidental)
                add_str += accidental < 0 ? "-" : "+";
	      // catch "C4/no3"; remove "no3"
	      if (trap == 4)
		{
		  int i = sub_str.index_i ("no3");
		  if (i != -1)
		    sub_str = sub_str.nomid_str (i, 3);
		  if (!sub_str.length_i ())
		    sub_sep_str = "";
		}
            }
	  sep_str = "/";
	}
    }

  String inversion_str;
  if (inversion)
    {
      inversion_str = inversion->str ();
      inversion_str = "/" + inversion_str.left_str (1).upper_str ()
	+ acc[tonic.accidental_i_ + 2];

    }

  if (sub_str.length_i ())
    sub_str = sep_str + sub_str;
  String str = tonic_str + "$^{" + add_str + sub_str + "}$" + inversion_str;
  return str;
}

int
Chord::find_tonic_i () const
{
  /*
    find tonic
    
    first try: base of longest line of triads
   */
  int tonic_i = 0;
  int longest_i = 0;
  for (int i = 0; i < pitch_arr_.size (); i++)
    {
      int no_triad_i = 0;
      int last_i = pitch_arr_[i % pitch_arr_.size ()].notename_i_;
      int j = 0;
      for (; j < pitch_arr_.size (); j++)
	{
	  int cur_i = pitch_arr_[(i + j + 1) % pitch_arr_.size ()].notename_i_;
	  int gap = cur_i - last_i;
	  while (gap < 0)
	    gap += 7;
	  gap %= 7;
	  if (gap == 2)
	    last_i = cur_i;
	  else
	    no_triad_i++;
	}
      if (j - no_triad_i > longest_i)
	{
	  longest_i = j - no_triad_i;
	  tonic_i = i;
	}
    }

  /*
    second try: note after biggest gap
   */
  int biggest_i = 0;
  //  if (longest_i)
  if (longest_i <= 1)
    for (int i = 0; i < pitch_arr_.size (); i++)
      {
	int gap = pitch_arr_[i].notename_i_
	  - pitch_arr_[(i - 1 + pitch_arr_.size ()) 
	  % pitch_arr_.size ()].notename_i_;
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
Chord::rebuild_from_base (int base_i)
{
  Musical_pitch last (0, 0, -5);
  Array<Musical_pitch> new_arr;
  for (int i = 0; i < pitch_arr_.size (); i++)
    {
      Musical_pitch p = pitch_arr_[(base_i + i) % pitch_arr_.size ()];
      if (p < last)
	{
	  p.octave_i_ = last.octave_i_;
	  if (p < last)
	    p.octave_i_++;
	}
      new_arr.push (p);
      last = p;
    }
  pitch_arr_ = new_arr;
}

void
Chord::rebuild_insert_inversion (int tonic_i)
{
  Musical_pitch inversion = pitch_arr_.get (0);
  rebuild_from_base (tonic_i - 1);
  if (pitch_arr_.size ())
    {
      inversion.octave_i_ = pitch_arr_[0].octave_i_ - 1;
      while (inversion < pitch_arr_[0])
	inversion.octave_i_++;
    }
  for (int i = 0; i < pitch_arr_.size (); i++)
    if (pitch_arr_[i] > inversion)
      {
	pitch_arr_.insert (inversion, i);
	break;
      }
}

void
Chord::rebuild_with_bass (int bass_i)
{
  Musical_pitch inversion = pitch_arr_.get (bass_i);
  // is lowering fine, or should others be raised?
  if (pitch_arr_.size ())
    while (inversion > pitch_arr_[0])
      inversion.octave_i_--;
  pitch_arr_.insert (inversion, 0);
}
