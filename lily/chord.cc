/*
  chord.cc -- implement Chord

  source file of the GNU LilyPond music typesetter

  (c)  1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "chord.hh"
#include "warn.hh"

// doesn't seem common, and we should know about this during purple hit
// #define INVERSION_ADDED_AS_BASE 1

Chord::Chord (Array<Musical_pitch> pitch_arr)
{
  pitch_arr_ = pitch_arr;
}

static void
rebuild_transpose (Musical_pitch tonic, Array<Musical_pitch>* pitch_arr_p)
{
  for (int i = 0; i < pitch_arr_p->size (); i++)
    {
      Musical_pitch p = tonic;
      Musical_pitch q = (*pitch_arr_p)[i];
      // duh, c7 should mean <c bes>
      if (q.notename_i_ == 6)
        q.accidental_i_--;
      p.transpose (q);
      (*pitch_arr_p)[i] = p;
    }
  pitch_arr_p->sort (Musical_pitch::compare);
}

static int
find_pitch_i (Array<Musical_pitch> const* pitch_arr_p, Musical_pitch p)
{
  for (int i = 0; i < pitch_arr_p->size (); i++)
    if (p == (*pitch_arr_p)[i])
      return i;
  return -1;
}

static int
find_notename_i (Array<Musical_pitch> const* pitch_arr_p, Musical_pitch p)
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

static int
trap_i (Musical_pitch tonic, Musical_pitch p)
{
  int i = p.notename_i_ - tonic.notename_i_
    + (p.octave_i_ - tonic.octave_i_) * 7;
  while (i < 0)
    i += 7;
  i++;
  return i;
}

static Array<Musical_pitch>
missing_triads_pitch_arr (Array<Musical_pitch>const* pitch_arr_p)
{
  Musical_pitch third (2);
  Musical_pitch mthird (2, -1);

  Array<Musical_pitch> triads;
  triads.push (third);   // c e 
  triads.push (mthird);  // d f 
  triads.push (mthird);  // e g 
  triads.push (third);   // f a 
  triads.push (third);   // g b 
  triads.push (mthird);  // a c 
  triads.push (mthird);  // b d 

  Musical_pitch tonic = (*pitch_arr_p)[0];
  Musical_pitch last = tonic;
  Array<Musical_pitch> missing_arr;

  for (int i = 0; i < pitch_arr_p->size ();)
    {
      Musical_pitch p = (*pitch_arr_p)[i];
      int trap = trap_i (tonic, p);
      if (last.notename_i_ == p.notename_i_)
	last.transpose (triads[(last.notename_i_ - tonic.notename_i_ + 7) % 7]);
      if (trap > trap_i (tonic, last))
	{
	  while (trap > trap_i (tonic, last))
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
	      last.transpose (triads[(last.notename_i_ - tonic.notename_i_ + 7) % 7]);
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
  construct from parser output
*/
Chord::Chord (Musical_pitch tonic, Array<Musical_pitch>* add_arr_p, Array<Musical_pitch>* sub_arr_p, Musical_pitch* inversion_p)
{
  rebuild_transpose (tonic, add_arr_p);
  rebuild_transpose (tonic, sub_arr_p);

  Musical_pitch fifth = tonic;
  fifth.transpose (Musical_pitch (2));
  fifth.transpose (Musical_pitch (2, -1));

  /*
    default chord includes upto 5: <1, 3, 5>
   */
  add_arr_p->insert (tonic, 0);
  int highest_trap = trap_i (tonic, add_arr_p->top ());
  if (highest_trap < 5)
    add_arr_p->push (fifth);

  /*
    find missing triads
   */
  Array<Musical_pitch> missing_arr = missing_triads_pitch_arr (add_arr_p);

  /*
    if additions include 4, assume sus4 and don't add third implicitely
   */
  Musical_pitch third = tonic;
  third.transpose (Musical_pitch (2));
  Musical_pitch sus = tonic;
  sus.transpose (Musical_pitch (3));
  if (::find_pitch_i (add_arr_p, sus) != -1)
    {
      int i = ::find_pitch_i (&missing_arr, third);
      if (i != -1)
	missing_arr.get (i);
    }
  
  /*
    complete the list of triads to be added
   */
  add_arr_p->concat (missing_arr);
  add_arr_p->sort (Musical_pitch::compare);

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

  pitch_arr_.sort (Musical_pitch::compare);

  for (int i = 0; i < sub_arr_p->size (); i++)
    warning (_f ("invalid subtraction: not part of chord: %s",
		 (*sub_arr_p)[i].str ()));

  if (inversion_p)
    {
      int i = 0;
      for (; i < pitch_arr_.size (); i++)
	{
	  if ((pitch_arr_[i].notename_i_ == inversion_p->notename_i_)
	      && (pitch_arr_[i].accidental_i_ == inversion_p->accidental_i_))
	    break;
	}
      if (i == pitch_arr_.size ())
	{
	  warning (_f ("invalid inversion pitch: not part of chord: %s",
		       inversion_p->str ()));
	}
      else
	{
#if INVERSION_ADDED_AS_BASE
	  pitch_arr_.insert (pitch_arr_[i], 0);
	  rebuild_with_bass (0);
#else
	  rebuild_with_bass (i);
#endif
	  
	}
      delete inversion_p;
    }
}

void
Chord::find_additions_and_subtractions(Array<Musical_pitch>* add_arr_p, Array<Musical_pitch>* sub_arr_p)
{
  Musical_pitch tonic = pitch_arr_[0];
  /*
    all the triads that should be there
   */
  Array<Musical_pitch> all_arr;
  all_arr.push (tonic);
  all_arr.push (pitch_arr_.top ());
  all_arr.concat (missing_triads_pitch_arr (&all_arr));
  all_arr.sort (Musical_pitch::compare);
  
  int i = 0;
  int j = 0;
  while ((i < all_arr.size ()) || (j < pitch_arr_.size ()))
    {
      i = i <? all_arr.size () - 1;
      j = j <? pitch_arr_.size () - 1;
      Musical_pitch a = all_arr[i];
      Musical_pitch p = pitch_arr_[j];
      if (a == p)
	{
	  i++;
	  j++;
	}
      else if ((p < a) || (p.notename_i_ == a.notename_i_))
	{
	  add_arr_p->push (p);
	  j++;
	}
      else
	{
	  sub_arr_p->push (a);
	  i++;
	}
    }
      
  /*
    add highest addition, because it names chord
   */
  if (trap_i (tonic, pitch_arr_.top () > 5))
    add_arr_p->push (pitch_arr_.top ());
}

String
Chord::banter_str (Musical_pitch* inversion) const
{
  Musical_pitch tonic = pitch_arr_[0];

  //urg, should do translation in scheme.
  char const *acc[] = {"\\textflat\\textflat ", "\\textflat ", "", "\\textsharp " , "\\textsharp\\textsharp "};
  String tonic_str = tonic.str ();
  tonic_str = tonic_str.left_str (1).upper_str ()
    + acc[tonic.accidental_i_ + 2];

  Array<Musical_pitch> add_arr;
  Array<Musical_pitch> sub_arr;
  find_additions_and_subtractions (&add_arr, &sub_arr);
			   
  Array<Musical_pitch> scale;
  scale.push (Musical_pitch (0)); // c
  scale.push (Musical_pitch (1)); // d
  scale.push (Musical_pitch (2)); // e
  scale.push (Musical_pitch (3)); // f
  scale.push (Musical_pitch (4)); // g
  scale.push (Musical_pitch (5)); // a
  scale.push (Musical_pitch (6)); // b
  // 7 always means 7-...
  //  scale.push (Musical_pitch (6, -1)); // b

  rebuild_transpose (tonic, &scale);
  
  bool has3m_b = false;
  bool has4_b = false;
  String str;
  String sep_str;
  for (int i = 0; i < add_arr.size (); i++)
    {
      Musical_pitch p = add_arr[i];
      int trap = trap_i (tonic, p);
      if (trap == 4)
	has4_b = true;
      int accidental = p.accidental_i_ - scale[(trap - 1) % 7].accidental_i_;
      if ((trap == 3) && (accidental == -1))
	{
	  tonic_str += "m";
	  has3m_b = true;
	}
      else if (accidental
	       || (!(trap % 2) || ((i + 1 == add_arr.size ()) && (trap > 5))))
        {
	  str += sep_str;
          if ((trap == 7) && (accidental == 1))
            str += "maj7";
          else
            {
              str += to_str (trap);
              if (accidental)
                str += accidental < 0 ? "-" : "+";
            }
	  sep_str = "/";
	}
    }

  for (int i = 0; i < sub_arr.size (); i++)
    {
      Musical_pitch p = sub_arr[i];
      int trap = trap_i (tonic, p);
      /*
	if chord has 3-, assume minor and don't display 'no3'
	if additions include 4, assume sus4 and don't display 'no3'
      */
      if (!((trap == 3) && (has3m_b || has4_b)))
	{
	  str += sep_str + "no" + to_str (trap);
	  sep_str = "/";
	}
    }

  String inversion_str;
  if (inversion)
    {
      inversion_str = inversion->str ();
      inversion_str = "/" + inversion_str.left_str (1).upper_str ()
	+ acc[inversion->accidental_i_ + 2];

    }

  return tonic_str + "$^{" + str + "}$" + inversion_str;
}

int
Chord::find_notename_i (Musical_pitch p) const
{
  return ::find_notename_i (&pitch_arr_, p);
}

int
Chord::find_pitch_i (Musical_pitch p) const
{
  return ::find_pitch_i (&pitch_arr_, p);
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
  assert (base_i >= 0);
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
  assert (tonic_i > 0);
#if INVERSION_ADDED_AS_BASE
  // inversion was added; don't insert
  Musical_pitch inversion = pitch_arr_.get (0);
  (void)inversion;
#else
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
#endif
}

void
Chord::rebuild_with_bass (int bass_i)
{
  assert (bass_i >= 0);
  Musical_pitch inversion = pitch_arr_.get (bass_i);
  // is lowering fine, or should others be raised?
  if (pitch_arr_.size ())
    while (inversion > pitch_arr_[0])
      inversion.octave_i_--;
  pitch_arr_.insert (inversion, 0);
}
