/*
  molecule.cc -- implement Molecule

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <math.h>
#include <libc-extension.hh>	// isinf

#include "font-metric.hh" 
#include "dimensions.hh"
#include "interval.hh"
#include "string.hh"
#include "molecule.hh"
#include "debug.hh"


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
      if (abs (o[a]) > 30 CM
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
Molecule::align_to (Axis a, Direction d)
{
  if (empty_b())
    return ;

  Interval i (extent (a));
  Real r = (d == CENTER) ? i.center () : i[d];
  translate_axis (-r, a);
}

void
Molecule::add_at_edge (Axis a, Direction d, Molecule const &m, Real padding)
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

  Real offset = my_extent -  his_extent;
  Molecule toadd (m);
  toadd.translate_axis (offset + d * padding, a);
  add_molecule (toadd);
}

LY_DEFINE(ly_set_molecule_extent_x,"ly-set-molecule-extent!", 3 , 0, 0, 
	  (SCM mol, SCM axis, SCM np),
	  "Set the extent (@var{extent} must be a pair of numbers) of @var{mol} in 
@var{axis} direction (0 or 1 for x- and y-axis respectively).

Note that an extent @code{(A . B)} is an interval and hence @code{A} is
smaller than @code{B}, and is often negative.
5")
{
  Molecule* m = unsmob_molecule (mol);
  SCM_ASSERT_TYPE (m, mol, SCM_ARG1, __FUNCTION__, "molecule");
  SCM_ASSERT_TYPE (ly_axis_p(axis), axis, SCM_ARG2, __FUNCTION__, "axis");
  SCM_ASSERT_TYPE (ly_number_pair_p (np), np, SCM_ARG3, __FUNCTION__, "number pair");

  Interval iv = ly_scm2interval (np);
  m->dim_[Axis (gh_scm2int (axis))] = iv;

  return SCM_UNDEFINED;
}

LY_DEFINE(ly_get_molecule_extent,
	  "ly-get-molecule-extent", 2 , 0, 0,  (SCM mol, SCM axis),
	  "Return a pair of numbers signifying the extent of @var{mol} in
@var{axis} direction (0 or 1 for x and y axis respectively).
")
{
  Molecule *m = unsmob_molecule (mol);
  SCM_ASSERT_TYPE (m, mol, SCM_ARG1, __FUNCTION__, "molecule");
  SCM_ASSERT_TYPE (ly_axis_p(axis), axis, SCM_ARG2, __FUNCTION__, "axis");
 
  return ly_interval2scm (m->extent (Axis (gh_scm2int (axis))));
}


LY_DEFINE(ly_molecule_combined_at_edge,
	  "ly-combine-molecule-at-edge",
	  5 , 0, 0,  (SCM first, SCM axis, SCM direction,
		      SCM second, SCM padding),
	  "Construct a molecule by putting @var{second} next to
@var{first}. @var{axis} can be 0 (x-axis) or 1 (y-axis), @var{direction} can be
-1 (left or down) or 1 (right or up).  @var{padding} specifies extra
space to add in between measured in global staff space.")

{
  Molecule * m1 = unsmob_molecule (first);
  Molecule * m2 = unsmob_molecule (second);
  Molecule result;


  SCM_ASSERT_TYPE(ly_axis_p(axis), axis, SCM_ARG2, __FUNCTION__, "axis");
  SCM_ASSERT_TYPE(ly_dir_p (direction), direction, SCM_ARG3, __FUNCTION__, "dir");
  SCM_ASSERT_TYPE(gh_number_p(padding), padding, SCM_ARG4, __FUNCTION__, "number");

  if (m1)
    result = *m1;
  if (m2)
    result.add_at_edge (Axis (gh_scm2int (axis)), Direction (gh_scm2int (direction)),
			*m2, gh_scm2double (padding));

  return result.smobbed_copy ();
}

/*
  FIXME: support variable number of arguments "
 */
LY_DEFINE(ly_add_molecule , 
	  "ly-add-molecule", 2, 0,0,(SCM first, SCM second),
	  "Combine two molecules."
	  )
{
  Molecule * m1 = unsmob_molecule (first);
  Molecule * m2 = unsmob_molecule (second);
  Molecule result;


  if (m1)
    result = *m1;
  if (m2)
    result.add_molecule (*m2);

  return result.smobbed_copy ();
}

LY_DEFINE(ly_make_molecule,
	  "ly-make-molecule", 3, 0, 0,  (SCM expr, SCM xext, SCM yext),
	  "")
{
  SCM_ASSERT_TYPE (ly_number_pair_p (xext), xext, SCM_ARG2, __FUNCTION__, "number pair");
  SCM_ASSERT_TYPE (ly_number_pair_p (yext), yext, SCM_ARG3, __FUNCTION__, "number pair");  

  Box b (ly_scm2interval (xext), ly_scm2interval(yext));
  Molecule m (b, expr);
  return m.smobbed_copy ();
}

SCM
fontify_atom (Font_metric * met, SCM f)
{
  if (f == SCM_EOL)
    return f;
  else
    return  scm_list_n (ly_symbol2scm ("fontify"),
			ly_quote_scm (met->description_), f, SCM_UNDEFINED);
}

LY_DEFINE(ly_fontify_atom,"ly-fontify-atom", 2, 0, 0, 
	  (SCM met, SCM f),
	  "Add a font selection command for the font metric @var{met} to @var{f}.")
{
  SCM_ASSERT_TYPE(unsmob_metrics (met), met, SCM_ARG1, __FUNCTION__, "font metric");

  return fontify_atom (unsmob_metrics (met), f);
}
LY_DEFINE(ly_align_to_x,"ly-align-to!", 3, 0, 0,  (SCM mol, SCM axis, SCM dir),
	  "Align @var{mol} using its own extents.")
{
  SCM_ASSERT_TYPE(unsmob_molecule (mol), mol, SCM_ARG1, __FUNCTION__, "molecule");
  SCM_ASSERT_TYPE(ly_axis_p(axis), axis, SCM_ARG2, __FUNCTION__, "axis");
  SCM_ASSERT_TYPE(ly_dir_p (dir), dir, SCM_ARG3, __FUNCTION__, "dir");

  unsmob_molecule (mol)->align_to ((Axis)gh_scm2int (axis), Direction (gh_scm2int (dir)));

  return SCM_UNDEFINED;
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
  String str (r->str ());
  scm_puts ((char *)str.ch_C (), port);
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

IMPLEMENT_TYPE_P (Molecule, "molecule?");
IMPLEMENT_DEFAULT_EQUAL_P (Molecule);

