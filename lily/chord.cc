/*
  chord.cc -- implement Chord

  source file of the GNU LilyPond music typesetter

  (c)  1999--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "chord.hh"
#include "musical-request.hh"
#include "warn.hh"
#include "debug.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"


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

Chord::Chord ()
{
  inversion_b_ = false;
  bass_b_ = false;
}

Chord::Chord (Array<Musical_pitch> pitch_arr, Musical_pitch* inversion_p, Musical_pitch* bass_p)
{
  pitch_arr_ = pitch_arr;
  inversion_b_ = false;
  bass_b_ = false;
  if (inversion_p)
    {
      inversion_pitch_ = *inversion_p;
      inversion_b_ = true;
      delete inversion_p;
    }
  if (bass_p)
    {
      bass_pitch_ = *bass_p;
      bass_b_ = true;
      delete bass_p;
    }
}
  
Chord::Chord (Chord const& chord)
{
  pitch_arr_ = chord.pitch_arr_;
  inversion_b_ = chord.inversion_b_;
  inversion_pitch_ = chord.inversion_pitch_;
  bass_b_ = chord.bass_b_;
  bass_pitch_ = chord.bass_pitch_;
}
  

/*
  JUNKME. 
  do something smarter.
 */
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

/*
  JUNKME. 
  do something smarter.
 */
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
  if (inversion_b_)
    {
      int i = 0;
      for (; i < pitch_arr.size (); i++)
	{
	  if ((pitch_arr[i].notename_i_ == inversion_pitch_.notename_i_)
	      && (pitch_arr[i].accidental_i_ == inversion_pitch_.accidental_i_))
	    break;
	}
      if (i == pitch_arr.size ())
	{
	  warning (_f ("invalid inversion pitch: not part of chord: %s",
		       inversion_pitch_.str ()));
	}
      else
	rebuild_with_bass (&pitch_arr, i);
    }

  if (bass_b_)
    {
      pitch_arr.insert (bass_pitch_, 0);
      rebuild_with_bass (&pitch_arr, 0);
    }
  return pitch_arr;
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

