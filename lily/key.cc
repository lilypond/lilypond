/*
  key.cc -- implement Key, Octave_key

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>

  TODO
  transposition.
*/

#include "key.hh"
#include "debug.hh"

const int OCTAVES=14;		// ugh..
const int ZEROOCTAVE=7;


void
Octave_key::print () const
{
  for (int i= 0; i < 7 ; i++)
    DOUT << "note " << i << " acc: " << accidental_i_arr_[i] << "\n";
}



Octave_key::Octave_key()
{
  accidental_i_arr_.set_size (7);
  clear ();
}

void
Octave_key::clear ()
{
  for (int i= 0; i < 7 ; i++)
    accidental_i_arr_[i] = 0;
}

Key::Key()
{
  multi_octave_b_ = false;
  octaves.set_size (OCTAVES);
}

Octave_key&
Key::oct (int i)
{
  return octaves[i+ZEROOCTAVE];    
}


void
Octave_key::set (int i, int a)
{
  assert (a > -3 && a < 3);
  accidental_i_arr_[i]=a;
}

void
Key::set (int o, int n , int a)
{
  octaves[o + ZEROOCTAVE].set (n,a);
}

void
Key::set (int n, int a)
{
  for (int i= 0; i < OCTAVES ; i++)
    octaves[i].set (n,a);
}
void
Key::clear ()
{
  for (int i= 0; i < OCTAVES ; i++)
    octaves[i].clear ();
}
void
Key::print () const
{
  for (int i= 0; i < OCTAVES ; i++)
    {
      DOUT << "octave " << i - ZEROOCTAVE << " Octave_key { ";
      octaves[i].print ();
      DOUT << "}\n";
    }
}
