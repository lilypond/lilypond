/*
  chord.cc -- implement Chord

  source file of the GNU LilyPond music typesetter

  (c)  1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "chord.hh"
#include "musical-request.hh"
#include "warn.hh"


/*
  construct from parser output
*/
Chord
to_chord (Musical_pitch tonic, Array<Musical_pitch>* add_arr_p, Array<Musical_pitch>* sub_arr_p, Musical_pitch* inversion_p, Musical_pitch* bass_p)
{
  // urg: catch dim modifier: 5th and 7th should be lowered
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
  Chord::rebuild_transpose (add_arr_p, tonic);
  Chord::rebuild_transpose (sub_arr_p, tonic);

  Musical_pitch fifth = tonic;
  fifth.transpose (Musical_pitch (2));
  fifth.transpose (Musical_pitch (2, -1));

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
  Musical_pitch third = tonic;
  third.transpose (Musical_pitch (2));
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
{
  pitch_arr_ = chord.pitch_arr_;
  inversion_p_ = chord.inversion_p_ ? new Musical_pitch (*chord.inversion_p_) : 0;
  bass_p_ = chord.bass_p_ ? new Musical_pitch (*chord.bass_p_) : 0;
}

Chord::~Chord ()
{
  delete inversion_p_;
  delete bass_p_;
}

void
Chord::rebuild_transpose (Array<Musical_pitch>* pitch_arr_p, Musical_pitch tonic)
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
Chord::find_additions_and_subtractions (Array<Musical_pitch>* add_arr_p, Array<Musical_pitch>* sub_arr_p) const
{
  Musical_pitch tonic = pitch_arr_[0];
  /*
    construct an array of thirds for a normal chord
   */
  Array<Musical_pitch> all_arr;
  all_arr.push (tonic);
  all_arr.push (pitch_arr_.top ());
  all_arr.concat (missing_thirds_pitch_arr (&all_arr));
  all_arr.sort (Musical_pitch::compare);
  
  int i = 0;
  int j = 0;
  while ((i < all_arr.size ()) || (j < pitch_arr_.size ()))
    {
      Musical_pitch a = all_arr [i <? all_arr.size () - 1];
      Musical_pitch p = pitch_arr_ [j <? pitch_arr_.size () - 1];
      /*
        this pitch is present: do nothing, check next
       */
      if (a == p)
	{
	  i++;
	  j++;
	}
      /*
        found an extra pitch: chord addition
       */
      else if ((p < a) || (p.notename_i_ == a.notename_i_))
	{
	  add_arr_p->push (p);
	  (j < pitch_arr_.size ()) ? j++ : i++;
	}
      /*
        a third is missing: chord subtraction
       */
      else
	{
	  sub_arr_p->push (a);
	  (i < all_arr.size ()) ? i++ : j++;
	}
    }
      
  /*
    add highest addition, because it names chord
    (1, 3 and) 5 not an addition: part of normal chord
   */
  if (step_i (tonic, pitch_arr_.top () > 5))
    add_arr_p->push (pitch_arr_.top ());
}

/*
  TODO:
   reduce guess work: dim chord
   other naming conventions `American'?
   don't use TeX constructs
   user defined chords-names for specific chords:
      tonic, additions, subtractions, inversion, bass -> "my-chord-name"
 */
String
Chord::banter_str () const
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
  for (int i=0; i < 7; i++)
    scale.push (Musical_pitch (i));

  // 7 always means 7-...
  //  scale.push (Musical_pitch (6, -1)); // b

  rebuild_transpose (&scale, tonic);
  
  bool has3m_b = false;
  bool has4_b = false;
  bool has5m_b = false;
  String str;
  String minor_str;
  String sep_str;
  for (int i = 0; i < add_arr.size (); i++)
    {
      Musical_pitch p = add_arr[i];
      int step = step_i (tonic, p);
      if (step == 4)
	has4_b = true;
      int accidental = p.accidental_i_ - scale[(step - 1) % 7].accidental_i_;
      if ((step == 3) && (accidental == -1))
	{
	  minor_str = "m";
	  has3m_b = true;
	}
      /*
	have Cdim rather than Cm5-, even if it's a prefix
       */
      else if ((step == 5) && (accidental == -1) && has3m_b)
	{
	  minor_str = "dim";
	  has5m_b = true;
	}
      else if (accidental
	       || (!(step % 2) || ((i + 1 == add_arr.size ()) && (step > 5))))
        {
	  str += sep_str;
	  sep_str = "/";
          if ((step == 7) && (accidental == 1))
	    {
              str += "maj7";
	    }
	  else
            {
	      /* 
	        if has3m_b and has5m_b, assume dim
		don't mention dim-addition, except for chord-namer
	       */
              if (((step/2) && (accidental == -1))
	          && has3m_b && has5m_b)
		{
		  if (i == add_arr.size () - 1)
                    str += to_str (step);
		  else
		    sep_str = "";
		}
	      else
	        {
                  str += to_str (step);
                  if (accidental)
                    str += accidental < 0 ? "-" : "+";
		}
            }
	}
    }

  for (int i = 0; i < sub_arr.size (); i++)
    {
      Musical_pitch p = sub_arr[i];
      int step = step_i (tonic, p);
      /*
	if chord has 3-, assume minor and don't display 'no3'
	if additions include 4, assume sus4 and don't display 'no3'
	if has3m_b and has5m_b, assume 'dim' chord
      */
      if (!((step == 3) && (has3m_b || has4_b))
         && !((step/2) && (step !=3) && (step !=7 ) && (p.accidental_i_ == 0) && has3m_b && has5m_b)
         && !((step == 7) && (p.accidental_i_ == -1) && has3m_b && has5m_b))
	{
	  str += sep_str + "no" + to_str (step);
	  sep_str = "/";
	}
    }

  /*
   have Co rather than Cdim7
   */
  if (minor_str + str == "dim7")
    {
      minor_str = "";
      str = "o";
    }
    

  String inversion_str;
  if (inversion_p_)
    {
      inversion_str = inversion_p_->str ();
      inversion_str = "/" + inversion_str.left_str (1).upper_str ()
	+ acc[inversion_p_->accidental_i_ + 2];
    }

  String bass_str;
  if (bass_p_)
    {
      bass_str = bass_p_->str ();
      bass_str = "/" + bass_str.left_str (1).upper_str ()
	+ acc[bass_p_->accidental_i_ + 2];

    }

  return tonic_str + minor_str + "$^{" + str + "}$" + inversion_str + bass_str;
}

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
