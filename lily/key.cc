/*
  key.cc -- implement Key, Octave_key

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>

*/

#include "key.hh"
#include "debug.hh"
#include "musical-pitch.hh"

const int NUMBER_OF_OCTAVES=14;		// ugh..
const int ZEROOCTAVE=7;


void
Octave_key::print () const
{
  for (int i= 0; i < 7 ; i++)
    DEBUG_OUT << "note " << i << " acc: " << accidental_i_arr_[i] << " iforce: " << internal_forceacc_b_arr_[i] << '\n';
}



Octave_key::Octave_key()
{
  accidental_i_arr_.set_size (7);
  internal_forceacc_b_arr_.set_size(7);
  clear ();
}

void
Octave_key::clear ()
{
  for (int i= 0; i < 7 ; i++)
  {
    accidental_i_arr_[i] = 0;
    internal_forceacc_b_arr_[i] = false;
  }
}

Key::Key()
{
  multi_octave_b_ = false;
  octaves_.set_size (NUMBER_OF_OCTAVES);
}

int 
Key::octave_to_index (int o) const
{
  int i = o + ZEROOCTAVE;
  if (i < 0)
    {
      warning (_f ("Don't have that many octaves (%s)", to_str (o)));
      i = 0;
    }
  if (i >= NUMBER_OF_OCTAVES)
    {
      warning (_f ("Don't have that many octaves (%s)", to_str (o)));
      i = NUMBER_OF_OCTAVES -1;
    }
  return i;
}

Octave_key const&
Key::oct (int i) const
{
  return octaves_[octave_to_index (i)];    
}


void
Octave_key::set (int i, int a)
{
  if (a <= -3)
    {
      warning (_f ("underdone accidentals (%s)", to_str (a)));
      a = -2;
    }
  if (a >= 3)
    {
      warning (_f ("overdone accidentals (%s)", to_str (a)));
      a = 2;
    }
  accidental_i_arr_[i]=a;
}

void
Key::set (Musical_pitch p)
{
  int   i = octave_to_index (p.octave_i_);
  octaves_[i].set (p.notename_i_,p.accidental_i_);
}

void
Key::set_internal_forceacc (Musical_pitch p)
{
  int   i = octave_to_index (p.octave_i_);
  octaves_[i].internal_forceacc_b_arr_[p.notename_i_] = true;
}

void
Key::clear_internal_forceacc (Musical_pitch p)
{
  int   i = octave_to_index (p.octave_i_);
  octaves_[i].internal_forceacc_b_arr_[p.notename_i_] = false;
}

void
Key::set (int n, int a)
{
  for (int i= 0; i < NUMBER_OF_OCTAVES ; i++)
    octaves_[i].set (n,a);
}
void
Key::clear ()
{
  for (int i= 0; i < NUMBER_OF_OCTAVES ; i++)
    octaves_[i].clear ();
}
void
Key::print () const
{
  for (int i= 0; i < NUMBER_OF_OCTAVES ; i++)
    {
      DEBUG_OUT << "octave " << i - ZEROOCTAVE << " Octave_key { ";
      octaves_[i].print ();
      DEBUG_OUT << "}\n";
    }
}

bool
Key::different_acc (Musical_pitch p)const
{
  return oct (p.octave_i_).acc (p.notename_i_) == p.accidental_i_;
}


bool
Key::internal_forceacc (Musical_pitch p)const
{
  return oct (p.octave_i_).internal_forceacc_b_arr_[p.notename_i_];
}


bool
Key::double_to_single_acc (Musical_pitch p) const
{
  return ((oct (p.octave_i_).acc (p.notename_i_) == -2
      && p.accidental_i_ == -1)
	  
	 || (oct (p.octave_i_).acc (p.notename_i_) == 2
	    && p.accidental_i_ == 1));
}
