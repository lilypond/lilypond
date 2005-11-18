/*
  system-start-delimiter.cc -- implement Nested_system_start_delimiter

  source file of the GNU LilyPond music typesetter

  (c) 2000--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "system-start-delimiter.hh"
#include "spanner.hh"
#include "axis-group-interface.hh"
#include "output-def.hh"
#include "font-interface.hh"
#include "all-font-metrics.hh"
#include "staff-symbol-referencer.hh"
#include "lookup.hh"
#include "item.hh"

#include "pointer-group-interface.hh"


struct Nested_system_start_delimiter
{
public:
  bool has_interface (Grob *);
  Stencil static make_substencil (Spanner *, Grob *, SCM, SCM, SCM);
  DECLARE_SCHEME_CALLBACK (print, (SCM));
};

Link_array<Grob> 
flatten_hierarchy (SCM hier)
{
  Link_array<Grob> retval;
  if (unsmob_grob (hier))
    retval.push (unsmob_grob (hier));
  
  if (scm_is_pair (hier))
    {
      retval = flatten_hierarchy (scm_car (hier));
      retval.concat (flatten_hierarchy (scm_cdr (hier)));
    }
  
  return retval;
}


Stencil
Nested_system_start_delimiter::make_substencil (Spanner *me,
					     Grob *common,
					     SCM hierarchy,
					     SCM depth_styles,
					     SCM default_style
					     )
{
  Interval ext;
  Link_array<Grob> elts = flatten_hierarchy (hierarchy);

  if (elts.is_empty ())
    return Stencil();
  
  int non_empty_count = 0;
  for (int i = elts.size (); i--;)
    {
      Spanner *sp = dynamic_cast<Spanner *> (elts[i]);

      if (sp
	  && sp->get_bound (LEFT) == me->get_bound (LEFT))
	{
	  Interval dims = sp->extent (common, Y_AXIS);
	  if (!dims.is_empty ())
	    {
	      non_empty_count ++;
	      ext.unite (dims);
	    }
	}
    }

  SCM glyph_sym = ly_symbol2scm ("bracket");
  if (scm_is_pair (depth_styles))
    glyph_sym = scm_car (depth_styles);
  
  if (ext.is_empty ()
      || (robust_scm2double (me->get_property ("collapse-height"), 0.0) >= ext.length()))
    {
      return Stencil();
    }

  Stencil total;

  Real len = ext.length ();
  if (glyph_sym == ly_symbol2scm ("bracket"))
    total = System_start_delimiter::staff_bracket (me, len);
  else if (glyph_sym == ly_symbol2scm ("brace"))
    total = System_start_delimiter::staff_brace (me, len);
  else if (glyph_sym == ly_symbol2scm ("bar-line"))
    total = System_start_delimiter::simple_bar (me, len);
  else if (glyph_sym == ly_symbol2scm ("line-bracket"))
    total = System_start_delimiter::line_bracket (me, len);

  total.translate_axis (ext.center (), Y_AXIS);

  Real left_x = total.extent (X_AXIS)[LEFT];
  for (SCM s = hierarchy; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM entry = scm_car (s);

      if (scm_is_pair (entry)
	  && !scm_is_symbol (scm_car (entry)))
	{
	  Stencil sub = make_substencil (me, common, entry,
					 scm_is_pair (depth_styles)
					 ? scm_cdr (depth_styles) : SCM_EOL,
					 default_style);
	  sub.translate_axis (left_x, X_AXIS);
	  total.add_stencil (sub);
	}
    }

  return total;
}

MAKE_SCHEME_CALLBACK (Nested_system_start_delimiter, print, 1);
SCM
Nested_system_start_delimiter::print (SCM smob)
{
  Spanner *me = unsmob_spanner (smob);
  if (!me)
    return SCM_EOL;

  SCM hierarchy = me->get_object ("staff-hierarchy");
  Link_array<Grob> elts = flatten_hierarchy (hierarchy);
  Grob *common = common_refpoint_of_array (elts, me, Y_AXIS);

  SCM default_style = me->get_property ("style");
  if (!scm_is_symbol (default_style))
    default_style = ly_symbol2scm ("line-bracket"); 
  
  Stencil total = make_substencil (me, common, hierarchy, me->get_property ("styles"),
				   default_style);
  total.translate_axis (- me->relative_coordinate (common, Y_AXIS), Y_AXIS);

  return total.smobbed_copy ();

}

ADD_INTERFACE (Nested_system_start_delimiter,
	       "nested-system-start-delimiter-interface",
	       
	       "The brace, bracket or bar in front of the system.  By setting "
	       "@code{staff-hierarchy}, eg. to @code{(a (b d) c)}, nesting for staff "
	       "braces can be produced."  
	       ,
	       

	       /* properties */
	       "collapse-height "
	       "styles "
	       "staff-hierarchy "
	       );
