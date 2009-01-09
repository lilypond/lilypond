/*
  system-start-text.cc -- implement System_start_text

  source file of the GNU LilyPond music typesetter

  (c) 2006--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "text-interface.hh"
#include "pointer-group-interface.hh"
#include "output-def.hh"
#include "font-interface.hh"
#include "spanner.hh"
#include "stencil.hh"
#include "item.hh"

class System_start_text
{
public:
  static Stencil get_stencil (Grob *);
  DECLARE_GROB_INTERFACE ();

  DECLARE_SCHEME_CALLBACK (print, (SCM));
};

Stencil
System_start_text::get_stencil (Grob *me_grob)
{
  Spanner *me = dynamic_cast<Spanner*> (me_grob);
  SCM t = me->get_property ("text");
  if (me->get_break_index () == 0)
    t = me->get_property ("long-text");
	   
  
  SCM chain = Font_interface::text_font_alist_chain (me);

  SCM scm_stencil = Text_interface::is_markup (t)
    ? Text_interface::interpret_markup (me->layout ()->self_scm (), chain, t)
    : SCM_EOL;

  
  if (Stencil *p = unsmob_stencil (scm_stencil))
    {
      SCM align_y  = me_grob->get_property ("self-alignment-Y");
      if (scm_is_number (align_y))
	p->align_to (Y_AXIS, robust_scm2double (align_y, 0.0));

      /* Horizontal alignment according to the self-alignment-X property
       * and indent value. */
      Output_def *layout = me_grob->layout ();
      Real indent;
      if (me->get_break_index () == 0)
	indent = robust_scm2double (layout->c_variable ("indent"), 0);
      else
	indent = robust_scm2double (layout->c_variable ("short-indent"), 0);
      Real align_x = robust_scm2double (me->get_property ("self-alignment-X"), 0);
      Interval p_extent_x = p->extent (X_AXIS);
      Interval padding (0.0, max (0.0, indent - p_extent_x.length ()));
      Real right_padding = padding.length () - padding.linear_combination (align_x);
      Box box (Interval (p_extent_x[LEFT], p_extent_x[RIGHT] + right_padding),
	       p->extent (Y_AXIS));
      Stencil *aligned_p = new Stencil (box, p->expr ());
      return *aligned_p;
    }
  return Stencil ();
}


MAKE_SCHEME_CALLBACK (System_start_text, print, 1);
SCM
System_start_text::print (SCM smob)
{
  Spanner *me = unsmob_spanner (smob);

  if (!me->get_bound (LEFT)->break_status_dir ())
    {
      me->suicide ();
      return SCM_EOL;
    }

  extract_grob_set (me, "elements", all_elts);
  vector<Grob*> elts;
  for (vsize i = 0; i < all_elts.size (); i++)
    if (all_elts[i]->is_live ())
      elts.push_back (all_elts[i]);

  if (!elts.size ())
    {
      me->suicide ();
      return SCM_EOL;
    }
  
  Grob *common = common_refpoint_of_array (elts, me, Y_AXIS);

  Interval ext;
  for (vsize i = elts.size (); i--;)
    {
      Spanner *sp = dynamic_cast<Spanner *> (elts[i]);

      if (sp
	  && sp->get_bound (LEFT) == me->get_bound (LEFT))
	ext.add_point (sp->relative_coordinate (common, Y_AXIS));
    }

  Stencil m = get_stencil (me);
  if (!ext.is_empty ())
    m.translate_axis (ext.center (), Y_AXIS);
  return m.smobbed_copy ();
}


ADD_INTERFACE (System_start_text,
	       "Text in front of the system.",

	       /* properties */
	       "text "
	       "long-text "
	       "self-alignment-Y "
	       "self-alignment-X "
	       );
