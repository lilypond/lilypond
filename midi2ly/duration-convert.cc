/*
  duration-convert.cc -- implement Duration_convert

  source file of the LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
	   Jan Nieuwenhuizen <janneke@gnu.org>
*/
#include <assert.h>
#include "duration-convert.hh"
#include "duration-iter.hh"
#include "warn.hh"

// statics Duration_convert
bool Duration_convert::no_quantify_b_s = false;
bool Duration_convert::no_double_dots_b_s = false;
bool Duration_convert::no_tuplets_b_s = false;
int Duration_convert::no_smaller_than_i_s = 0;
Array<Duration> Duration_convert::dur_array_s;
	
String 
Duration_convert::dur2_str (Duration dur)
{
  if (dur.ticks_i_)
    return String ("[") + to_str (dur.ticks_i_) + "]";
  
  String str;
  if (dur.durlog_i_ >= 0)
    str = to_str ( type2_i (dur.durlog_i_) );
  else if (dur.durlog_i_ == -1)
    str = "\\breve";
  else if (dur.durlog_i_ <= -2)
    {
      str = "\\longa";
      if (dur.durlog_i_ < -2)
	{
	  dur.plet_.iso_i_ *= 1 << (-2 - dur.durlog_i_);
	}
     }
  str += to_str ('.', dur.dots_i_);
  if (dur.plet_b ())
    str += String ("*") + to_str (dur.plet_.iso_i_)
      + String ("/") + to_str (dur.plet_.type_i_);
  return str;
}

int
Duration_convert::dur2ticks_i (Duration dur)
{
  if (dur.ticks_i_)
    return dur.ticks_i_;
  return dur2_mom (dur) * Rational (Duration::division_1_i_s);
}

int
Duration_convert::i2_type (int i)
{
  int t=0;
  while (i && !(i & 1)) {
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

Rational
Duration_convert::dur2_mom (Duration dur)
{
  if (dur.ticks_i_)
    return Rational (dur.ticks_i_, Duration::division_1_i_s);	

  // or simply assert?
  if (dur.durlog_i_<-10)
    return Rational (0);
  Rational mom;
  if (dur.durlog_i_<0)
    mom = Rational (type2_i (-dur.durlog_i_), 1);
  else
    mom = Rational (1 , type2_i (dur.durlog_i_));

  Rational delta = mom;
  while (dur.dots_i_--) 
    {
      delta /= 2.0;
      mom += delta;
    }

  return mom * plet_factor_mom (dur);    
}

Duration
Duration_convert::mom2_dur (Rational mom)
{
  if (!mom) 
    {
      Duration dur;
      dur.set_plet (0,1);
      return dur;
    }

  return mom2standardised_dur (mom);
}

Duration
Duration_convert::mom2standardised_dur (Rational mom)
{
  //	if (!dur_array_s.length_i ())
  if (!dur_array_s.size ())
    set_array ();
  assert (dur_array_s.size ());
  for (int i = 0; i < dur_array_s.size () - 1; i++) 
    {
      Rational lower_mom = dur2_mom (dur_array_s[ i ]);
      if (mom <= lower_mom) 
	{
	  // all arbitrary, but 3/4 will get rid of the noise...
	  // kinda ok
	  if (i || (mom / lower_mom > Rational (3, 4)))
	    return dur_array_s[ i ];
	  else 
	    {
	      Duration d;
	      d.durlog_i_ = -100;
	      return d;
	    }
	}
      Rational upper_mom = dur2_mom (dur_array_s[ i + 1 ]);
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

  Duration_iterator i;
  dur_array_s.push (i.dur ());
  while (i.ok ())
    dur_array_s.push (i.forward_dur ());
}


Rational
Duration_convert::plet_factor_mom (Duration dur)
{
  return dur.plet_.mom ();
}

Real
Duration_convert::sync_f (Duration dur, Rational mom)
{
  return mom / dur2_mom (dur);
}

Duration
Duration_convert::ticks2_dur (int ticks_i)
{
  Rational mom (ticks_i, Duration::division_1_i_s);
  return mom2standardised_dur (mom);
}

Duration
Duration_convert::ticks2standardised_dur (int ticks_i)
{
  Rational mom (ticks_i, Duration::division_1_i_s);
  Duration dur = mom2standardised_dur (mom);
  return dur;
}
