/*
  molecule.cc -- implement Molecule

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

/*
  ugh. Rewrite not finished yet. Still must copy atom lists.
 */


#include <math.h>

#include "font-metric.hh"
#include "dimensions.hh"
#include "interval.hh"
#include "string.hh"
#include "molecule.hh"

#include "debug.hh"
#include "killing-cons.tcc"


Box
Molecule::extent() const
{
  return dim_;
}

Interval
Molecule::extent(Axis a) const
{
  return dim_[a];
}

Molecule::Molecule (Box b, SCM func)
{
  expr_ = func;
  dim_ = b ;
}

Molecule::Molecule()
{
  expr_ = SCM_EOL;
  set_empty (true);
}

void
Molecule::translate (Offset o)
{
  Axis a = X_AXIS;
  while (a < NO_AXES)
    {
      if (abs(o[a]) > 30 CM
	  || isinf (o[a]) || isnan (o[a]))
	{
	  programming_error ("Improbable offset for translation: setting to zero");
	  o[a] =  0.0;
	}
      incr (a);
    }

  expr_ = gh_list (ly_symbol2scm ("translate-molecule"),
		   to_scm (o),
		   expr_, SCM_UNDEFINED);
  if (!empty_b ())
    dim_.translate (o);
}
  

void
Molecule::translate_axis (Real x,Axis a)
{
  Offset o(0,0);
  o[a] = x;
  translate (o);
}  



void
Molecule::add_molecule (Molecule const &m)
{
  expr_ = gh_list (ly_symbol2scm ("combine-molecule"),
		   m.expr_,
		   expr_, SCM_UNDEFINED);
  dim_.unite (m.dim_);
}

void
Molecule::set_empty (bool e)
{
  if (e)
    {
      dim_[X_AXIS].set_empty ();
      dim_[Y_AXIS].set_empty ();
    }
  else
    {
      dim_[X_AXIS] = Interval(0,0);
      dim_[Y_AXIS] = Interval (0,0);
    }
}


void
Molecule::align_to (Axis a, Direction d)
{
  Interval i (extent (a));
  Real r =  (d == CENTER) ? i.center () : i[d];
  translate_axis (-r, a);
}


void
Molecule::add_at_edge (Axis a, Direction d, Molecule const &m, Real padding)
{
  Real my_extent= empty_b () ? 0.0 : dim_[a][d];
  Interval i (m.extent ()[a]);
  if (i.empty_b ())
    programming_error ("Molecule::add_at_edge: adding empty molecule.");
  
  Real his_extent = i[-d];
  Real offset = my_extent -  his_extent;
  Molecule toadd (m);
  toadd.translate_axis (offset + d * padding, a);
  add_molecule (toadd);
}

bool
Molecule::empty_b () const
{
  return expr_ == SCM_EOL;
}


SCM
fontify_atom(Font_metric * met, SCM f)
{
  return  gh_list (ly_symbol2scm ("fontify"),
		   ly_quote_scm (met->description ()), f, SCM_UNDEFINED);
}
