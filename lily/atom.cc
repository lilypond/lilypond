/*
  atom.cc -- implement Atom

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "atom.hh"
#include "tex.hh"
#include "interval.hh"
#include "dimen.hh"
#include "string.hh"
#include "varray.hh"
#include "debug.hh"

void
Atom::print() const
{
#ifndef NPRINT
  DOUT << "texstring: " <<tex_<<"\n";

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
  return "Atom (\'"+tex_+"\', (" + dim_.x().str () + ", "
    + dim_.y ().str () + "))";
}


bool
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
	  warning (_("ridiculous dimension ") + axis_name_str (ax)  + ", "
		   +print_dimen(off_[ax]));
	  ((Atom*)this)->off_[ax] = 0.0;
	  ridiculous = true;
	}
    }
#endif
  return ridiculous;
}
String
Atom::TeX_string() const
{
  String tex_str = tex_;
  if (check_infinity_b ())
    tex_str += "\errormark";
  
   // whugh.. Hard coded...
  String s ("\\placebox{");
  s += print_dimen (off_[Y_AXIS])+"}{";
  s += print_dimen (off_[X_AXIS]) + "}{";
  s += tex_str + "}";
  return s;
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
