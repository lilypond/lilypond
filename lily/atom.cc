/*
  atom.cc -- implement Atom

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "atom.hh"
#include "tex.hh"
#include "interval.hh"
#include "dimension.hh"
#include "string.hh"
#include "array.hh"
#include "debug.hh"
#include "main.hh"

inline bool
Atom::check_infinity_b ()const
{
  bool ridiculous = false;
#ifndef NDEBUG
  
  /* infinity checks. */
  for (int a = X_AXIS; a < NO_AXES; a++)
    {
      Axis ax = (Axis)a;
      if (abs (off_[ax]) >= 100 CM)
	{
	  warning (_f ("ridiculous dimension: %s, %s", axis_name_str (ax),
		   print_dimen(off_[ax])));
	  
	  if (experimental_features_global_b)
	    assert (false);

	  ((Atom*)this)->off_[ax] = 0.0;
	  ridiculous = true;
	}
    }
#endif
  return ridiculous;
}


void
Atom::print() const
{
#ifndef NPRINT
  DOUT << "texstring: " << tex_ << '\n';

  DOUT << "dim:";
  for (Axis i=X_AXIS; i < NO_AXES; incr(i))
    DOUT << axis_name_str(i) << " = " << dim_[i].str();

  DOUT << "\noffset: " << off_.str ();
#endif
}

Box
Atom::extent() const
{
  Box b (dim_);
  b.translate (off_);
  return b;
}


Atom::Atom()
  : dim_ (Interval (0,0),Interval (0,0))
{
  tex_ = "\\unknown";
}

Atom::Atom (String s, Box b)
  :  dim_ (b)
{
  tex_ = s;
}


String
Atom::str() const
{
  return String ("Atom (\'") + tex_ + "\', (" + dim_.x().str () + ", "
    + dim_.y ().str () + "))";
}

Offset
Atom::offset () const
{
  return off_;
}



void
Atom::translate_axis (Real r, Axis a)
{
  off_[a] += r;
  check_infinity_b ();
}

void
Atom::translate (Offset o)
{
  off_ += o;
  check_infinity_b ();
}
