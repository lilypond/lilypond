/*
  balloon.cc -- implement Balloon objects
 */

#include "text-item.hh"
#include "grob.hh"
#include "line-interface.hh"
#include "lookup.hh"
#include "font-interface.hh"
#include "stencil.hh"
#include "lily-guile.hh"
#include "paper-def.hh"
#include "misc.hh"

struct Balloon_interface
{
  
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  static bool has_interface (Grob*);
};

MAKE_SCHEME_CALLBACK (Balloon_interface, print, 1);
SCM
Balloon_interface::print (SCM smob) 
{
  Grob *me= unsmob_grob (smob);

  SCM cb = me->get_property ("balloon-original-callback");
  SCM scm_mol  =  SCM_EOL;

  if (ly_procedure_p (cb))
    {
      scm_mol = scm_call_1 (cb, smob);
    }

  if (!unsmob_stencil (scm_mol))
    return scm_mol;

  SCM scm_off = me->get_property ("balloon-text-offset");

  if (!is_number_pair (scm_off))
    return scm_mol;

  Offset off = ly_scm2offset (scm_off);
  Stencil * m = unsmob_stencil (scm_mol);
  Box orig_extent = m->extent_box ();
  Box box_extent = orig_extent;

  Real w = robust_scm2double (me->get_property ("balloon-padding"),  .1);
  box_extent.widen (w, w);
  
  
  Stencil fr = Lookup::frame (box_extent, 0.1, 0.05);

  
  fr.add_stencil (*m);



  SCM bt = me->get_property ("balloon-text");
  SCM chain = Font_interface::text_font_alist_chain (me);
  chain = scm_cons (me->get_property ("balloon-text-props"), chain);


  SCM text = Text_item::interpret_markup (me->get_paper ()->self_scm (), chain, bt);

  
  Stencil *text_mol = unsmob_stencil (text);
  
  Offset z1;

  for (int i = X_AXIS; i < NO_AXES; i++)
    {
      Axis  a ((Axis)i);
      z1[a] = box_extent [a].linear_combination (sign (off[a]));
      text_mol->align_to (a, -sign (off[a]));
    }

  Offset z2 = z1 + off;
  
  fr.add_stencil (Line_interface::line (me, z1, z2));

  text_mol->translate (z2);
  fr.add_stencil (*text_mol);
  
  fr = Stencil (orig_extent, fr.get_expr ());
  return fr.smobbed_copy ();
}

ADD_INTERFACE (Balloon_interface,"text-balloon-interface",
	       "A collection of routines to put text balloons around an object.",
	       "balloon-padding balloon-text-props balloon-text-offset balloon-text balloon-original-callback");

