/*
  duration-convert.cc -- implement Duration_convert

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
	   Jan Nieuwenhuizen <jan@digicash.com>
*/
#include <assert.h>
#include "duration-convert.hh"
#include "warn.hh"

// statics Duration_convert
bool const Duration_convert::midi_as_plet_b_s = true;
bool Duration_convert::no_quantify_b_s = false;
bool Duration_convert::no_double_dots_b_s = false;
bool Duration_convert::no_triplets_b_s = false;
int Duration_convert::no_smaller_than_i_s = 0;
Array<Duration> Duration_convert::dur_array_s;
	
String 
Duration_convert::dur2_str (Duration dur)
{
  if (dur.ticks_i_)
    return String ("[") + String (dur.ticks_i_) + "]";
  
  String str;
  if (dur.durlog_i_ >= 0)
    str = String ( type2_i (dur.durlog_i_) );
  else if (dur.durlog_i_ == -1)
    str = "\\breve";
  else if (dur.durlog_i_ == -2)
    str = "\\longa";
  str += String ('.', dur.dots_i_);
  if (dur.plet_b ())
    str += String ("*") + String (dur.plet_.iso_i_)
      + String ("/") + String (dur.plet_.type_i_);
  return str;
}

#if 0
int
Duration_convert::dur2_i (Duration dur, int division_1_i)
{
  return dur2_mom (dur) * Moment (division_1_i);
}
#endif

int
Duration_convert::dur2ticks_i (Duration dur)
{
  if (dur.ticks_i_)
    return dur.ticks_i_;
  return dur2_mom (dur) * Moment (Duration::division_1_i_s);
}


int
Duration_convert::i2_type (int i)
{
  int t=0;
  while (!(i & 1)) {
    i >>= 1;
    t++;
  }
  return t;
}

int
Duration_convert::type2_i (int type)
{
  if (type<0)
    return 0; 
  else
    return 1 << type;
}

Moment
Duration_convert::dur2_mom (Duration dur)
{
  if (dur.ticks_i_)
    return Moment (dur.ticks_i_, Duration::division_1_i_s);	

  // or simply assert?
  if (dur.durlog_i_<-10)
    return Moment (0);
  Moment mom;
  if (dur.durlog_i_<0)
    mom = Moment (type2_i (-dur.durlog_i_), 1);
  else
    mom = Moment (1 , type2_i (dur.durlog_i_));

  Moment delta = mom;
  while (dur.dots_i_--) 
    {
      delta /= 2.0;
      mom += delta;
    }

  return mom * plet_factor_mom (dur);    
}

#if 0
Moment
Duration_convert::i2_mom (int time_i, int division_1_i)
{
  if (!time_i)
    return Moment (0);

  if (division_1_i > 0)
    return Moment (time_i, division_1_i);
  else 
    return Moment (-division_1_i, time_i);
}
#endif

Duration
Duration_convert::mom2_dur (Moment mom)
{
  if (!mom) 
    {
      Duration dur;
      dur.set_plet (0,1);
      return dur;
    }
	

  Duration dur = mom2standardised_dur (mom);
  //	if (!dur.mom () || (dur.mom () == mom))
  if (!dur.length () || (dur.length () == mom))
    return dur;
  assert (midi_as_plet_b_s);

  //	dur.set_plet (type_mom, Duration::division_1_i_s / 4); 

  //	Moment as_plet_mom = mom / dur.mom ();
  Moment as_plet_mom = mom / dur.length ();
  as_plet_mom *= dur.plet_.mom ();
  long num = as_plet_mom.numerator ().as_long ();
  long den = as_plet_mom.denominator ().as_long ();
  dur.set_plet (num, den);
  return dur;
}

Duration
Duration_convert::mom2standardised_dur (Moment mom)
{
  //	if (!dur_array_s.length_i ())
  if (!dur_array_s.size ())
    set_array ();
  assert (dur_array_s.size ());
  for (int i = 0; i < dur_array_s.size () - 1; i++) 
    {
      Moment lower_mom = dur2_mom (dur_array_s[ i ]);
      if (mom <= lower_mom) 
	{
	  // all arbitrary, but 3/4 will get rid of the noise...
	  // kinda ok
	  if (i || (mom / lower_mom > Moment (3, 4)))
	    return dur_array_s[ i ];
	  else 
	    {
	      Duration d;
	      d.durlog_i_ = -100;
	      return d;
	    }
	}
      Moment upper_mom = dur2_mom (dur_array_s[ i + 1 ]);
      if ((mom < upper_mom)
	  && ((mom - lower_mom) / lower_mom
	      < (upper_mom - mom) / upper_mom))
	return dur_array_s[ i ];
    }
  return dur_array_s[ dur_array_s.size () - 1 ];
}

void
Duration_convert::set_array ()
{
  dur_array_s.clear ();

  Duration_iterator iter_dur;
  assert (iter_dur);
  while (iter_dur)
    dur_array_s.push (iter_dur++);
}


Moment
Duration_convert::plet_factor_mom (Duration dur)
{
  return dur.plet_.mom ();
}

Real
Duration_convert::sync_f (Duration dur, Moment mom)
{
  return mom / dur2_mom (dur);
}

Duration
Duration_convert::ticks2_dur (int ticks_i)
{
  //		Duration dur (4, 0);
  //		dur.set_plet (ticks_i, Duration::division_1_i_s / 4); 

  Moment mom (ticks_i, Duration::division_1_i_s);
  if (midi_as_plet_b_s)
    return mom2_dur (mom);

  Duration dur = mom2standardised_dur (mom);

  //	if (dur.mom () == mom)
  if (dur.length () == mom)
    return dur;
		
// huh?
#if 0
  dur.durlog_i_ = -100;
  dur.dots_i_ = 0;
  dur.set_ticks (ticks_i);
  return dur;
#else
  return mom2_dur (mom);
#endif
}

Duration
Duration_convert::ticks2standardised_dur (int ticks_i)
{
  Moment mom (ticks_i, Duration::division_1_i_s);
  Duration dur = mom2standardised_dur (mom);
  return dur;
}

Duration_iterator::Duration_iterator ()
{
  cursor_dur_.durlog_i_ = 7;
  if (Duration_convert::no_smaller_than_i_s)
    cursor_dur_.durlog_i_ = Duration_convert::no_smaller_than_i_s;
  //	cursor_dur_.set_plet (1, 1);
}

Duration 
Duration_iterator::operator ++(int)
{
  return forward_dur ();
}

Duration
Duration_iterator::operator ()()
{
  return dur ();
}

Duration_iterator::operator bool ()
{
  return ok ();
}

Duration
Duration_iterator::dur ()
{
  return cursor_dur_;
}

Duration
Duration_iterator::forward_dur ()
{
  /* should do smart table? guessing: 
     duration wholes
     16 	0.0625
     32.. 	0.0703
     8:2/3	0.0833
     16.	0.0938
     8	0.1250
     16..	0.1406
     4:2/3	0.1667
     8.	0.1875
		
     */
  assert (ok ());

  Duration dur = cursor_dur_;

  if (!cursor_dur_.dots_i_ && !cursor_dur_.plet_b ()) 
    {
      cursor_dur_.durlog_i_ += 1;
      cursor_dur_.dots_i_ = 2;
    }
  else if (cursor_dur_.dots_i_ == 2) 
    {
      assert (!cursor_dur_.plet_b ());
      cursor_dur_.dots_i_ = 0;
      cursor_dur_.durlog_i_ -=2;
      cursor_dur_.set_plet (2, 3);
    }
  else if (cursor_dur_.plet_b () 
	   && (cursor_dur_.plet_.iso_i_ == 2)
	   && (cursor_dur_.plet_.type_i_ == 3)) 
    {
      assert (!cursor_dur_.dots_i_);
      cursor_dur_.set_plet (1, 1);
      cursor_dur_.durlog_i_ += 1;
      cursor_dur_.dots_i_ = 1;
    }
  else if (cursor_dur_.dots_i_ == 1) 
    {
      assert (!cursor_dur_.plet_b ());
      cursor_dur_.dots_i_ = 0;
      cursor_dur_.durlog_i_ -= 1;
    }
		
  if (Duration_convert::no_triplets_b_s
      && cursor_dur_.plet_b () && ok ())
    forward_dur ();
  if (Duration_convert::no_double_dots_b_s 
      && (cursor_dur_.dots_i_ == 2) && ok ())
    forward_dur ();
  if (Duration_convert::no_smaller_than_i_s
      && (cursor_dur_.durlog_i_ > Duration_convert::no_smaller_than_i_s) && ok ())
    forward_dur ();
  if (Duration_convert::no_smaller_than_i_s
      && cursor_dur_.dots_i_
      && (cursor_dur_.durlog_i_ >= Duration_convert::no_smaller_than_i_s)
      && ok ())
    forward_dur ();
  if (Duration_convert::no_smaller_than_i_s
      && (cursor_dur_.dots_i_ == 2)
      && (cursor_dur_.durlog_i_ >= Duration_convert::no_smaller_than_i_s / 2)
      && ok ())
    forward_dur ();

  return dur;
}

bool
Duration_iterator::ok ()
{
  return (cursor_dur_.durlog_i_ 
	  && !((cursor_dur_.durlog_i_ == 0) && (cursor_dur_.dots_i_ > 2)));
}
