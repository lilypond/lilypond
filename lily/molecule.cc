/*
  molecule.cc -- implement Molecule

  source file of the GNU LilyPond music typesetter

  (c)  1997--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <math.h>
#include <libc-extension.hh>	// isinf

#include "font-metric.hh" 
#include "dimensions.hh"
#include "interval.hh"
#include "string.hh"
#include "molecule.hh"
#include "warn.hh"


#include "ly-smobs.icc"


SCM
Molecule::smobbed_copy () const
{
  Molecule * m = new Molecule (*this);

  return m->smobbed_self ();
}

Interval
Molecule::extent (Axis a) const
{
  return dim_[a];
}

Molecule::Molecule (Box b, SCM func)
{
  expr_ = func;
  dim_ = b;
}

Molecule::Molecule ()
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
      if (abs (o[a]) > 100 CM
	  || isinf (o[a]) || isnan (o[a]))
	{
	  programming_error ("Improbable offset for translation: setting to zero");
	  o[a] =  0.0;
	}
      incr (a);
    }

  expr_ = scm_list_n (ly_symbol2scm ("translate-molecule"),
		   ly_offset2scm (o),
		   expr_, SCM_UNDEFINED);
  if (!empty_b ())
    dim_.translate (o);
}
  

void
Molecule::translate_axis (Real x,Axis a)
{
  Offset o (0,0);
  o[a] = x;
  translate (o);
}  



void
Molecule::add_molecule (Molecule const &m)
{
  expr_ = scm_list_n (ly_symbol2scm ("combine-molecule"),
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
      dim_[X_AXIS] = Interval (0,0);
      dim_[Y_AXIS] = Interval (0,0);
    }
}


void
Molecule::align_to (Axis a, Real x)
{
  if (empty_b())
    return ;

  Interval i (extent (a));
  translate_axis (-i.linear_combination (x), a);
}

/*
  See scheme Function.
 */
void
Molecule::add_at_edge (Axis a, Direction d, Molecule const &m, Real padding,
		       Real minimum)
{
  Real my_extent= empty_b () ? 0.0 : dim_[a][d];
  Interval i (m.extent (a));
  Real his_extent;
  if (i.empty_b ())
    {
      programming_error ("Molecule::add_at_edge: adding empty molecule.");
      his_extent = 0.0;
    }
  else
    his_extent = i[-d];      

  Real offset = (my_extent -  his_extent)  + d*padding;
  if (minimum > 0  && fabs (offset) <  minimum)
    offset = sign (offset) * minimum; 
  
  Molecule toadd (m);
  toadd.translate_axis (offset, a);
  add_molecule (toadd);
}



/*
  Hmm... maybe this is not such a good idea ; stuff can be empty,
  while expr_ == '()
 */
bool
Molecule::empty_b () const
{
  return expr_ == SCM_EOL;
}

SCM
Molecule::get_expr () const
{
  return expr_;
}



Box
Molecule::extent_box () const
{
  return dim_;
}
IMPLEMENT_SIMPLE_SMOBS (Molecule);


int
Molecule::print_smob (SCM , SCM port, scm_print_state *)
{
  scm_puts ("#<Molecule ", port);
#if 0
  Molecule  *r = (Molecule *) ly_cdr (s);
  String string (r->to_string ());
  scm_puts ((char *)str.to_str0 (), port);
#endif
  scm_puts (" >", port);
  
  return 1;
}

  
SCM
Molecule::mark_smob (SCM s)
{
  Molecule  *r = (Molecule *) ly_cdr (s);
  
  return r->expr_;
}

IMPLEMENT_TYPE_P (Molecule, "ly:molecule?");
IMPLEMENT_DEFAULT_EQUAL_P (Molecule);

