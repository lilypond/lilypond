/*   
  musical-pitch.cc --  implement Musical_pitch
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "musical-pitch.hh"
#include "debug.hh"
#include "main.hh"

Musical_pitch::Musical_pitch (int n, int a, int o)
{
  notename_i_ = n;
  accidental_i_ = a;
  octave_i_ = o;
}

void
Musical_pitch::print () const
{
#ifndef NPRINT
  DOUT << str ();
#endif
}

int
Musical_pitch::compare (Musical_pitch const &m1, Musical_pitch const &m2)
{
    int o=  m1.octave_i_ - m2.octave_i_;
  int n = m1.notename_i_ - m2.notename_i_;
  int a = m1.accidental_i_ - m2.accidental_i_;

  if (o)
	return o;
  if (n)
	return n;
  if (a)
	return a;
  return 0;
}

int
Musical_pitch::steps () const
{
  return  notename_i_ + octave_i_*7;
}

/*
  should be settable from input to allow "viola"-mode
 */
static Byte pitch_byte_a[  ] = { 0, 2, 4, 5, 7, 9, 11 };

int
Musical_pitch::semitone_pitch () const
{
  return  pitch_byte_a[ notename_i_ % 7 ] + accidental_i_ + octave_i_ * 12;
}

void
Musical_pitch::transpose (Musical_pitch delta)
{
  int old_pitch = semitone_pitch ();
  int delta_pitch = delta.semitone_pitch ();
  octave_i_ += delta.octave_i_;
  notename_i_ += delta.notename_i_;

  
  while  (notename_i_ >= 7)
    {
      notename_i_ -= 7;
      octave_i_ ++;
    }

  int new_pitch = semitone_pitch ();
  int delta_acc = new_pitch - old_pitch - delta_pitch;
  accidental_i_ -= delta_acc;
}


#if 0
// nice test for internationalisation strings
char const *accname[] = {"double flat", "flat", "natural",
			 "sharp" , "double sharp"};
#else
char const *accname[] = {"eses", "es", "", "is" , "isis"};
#endif

String
Musical_pitch::str () const
{
  int n = (notename_i_ + 2) % 7;
  String s = to_str (char(n + 'a'));
  if (accidental_i_)
    s += String (accname[accidental_i_ + 2]);

  if (octave_i_ > 0)
    {
      int o = octave_i_ + 1;
      while (o--)
	s += to_str ('\'');
    }
  else if (octave_i_ <0)
    {
      int o = (-octave_i_) - 1;
      while (o--)
	s += to_str (',');
    }
#if 0  
  if (octave_i_)
    s  += String ((octave_i_> 0)? "^": "_") + to_str (octave_i_);
#endif
  
  return s;
}

/*
  change me to relative, counting from last pitch p
  return copy of resulting pitch
 */
Musical_pitch
Musical_pitch::to_relative_octave (Musical_pitch p)
{
  int oct_mod = octave_i_  + 1;	// account for c' = octave 1 iso. 0 4
  Musical_pitch up_pitch (p);
  Musical_pitch down_pitch (p);

  up_pitch.accidental_i_ = accidental_i_;
  down_pitch.accidental_i_ = accidental_i_;
  
  Musical_pitch n = *this;
  up_pitch.up_to (notename_i_);
  down_pitch.down_to (notename_i_);

  int h = p.steps ();
  if (abs (up_pitch.steps () - h) < abs (down_pitch.steps () - h))
    n = up_pitch;
  else
    n = down_pitch;
  
  n.octave_i_ += oct_mod;

  *this = n;
  return *this;
}

void
Musical_pitch::up_to (int notename)
{
  if (notename_i_  > notename)
    {
      octave_i_ ++;
    }
  notename_i_  = notename;
}

void
Musical_pitch::down_to (int notename)
{
  if (notename_i_ < notename)
    {
      octave_i_ --;
    }
  notename_i_ = notename;
}

