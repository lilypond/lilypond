/*
  balloon.cc -- implement Balloon objects
 */

#include "text-item.hh"
#include "grob.hh"
#include "lookup.hh"
#include "font-interface.hh"
#include "molecule.hh"
#include "lily-guile.hh"
#include "paper-def.hh"
#include "misc.hh"

struct Balloon_interface
{
  
public:
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM));
  static bool has_interface (Grob*);
};

MAKE_SCHEME_CALLBACK (Balloon_interface, brew_molecule, 1);
SCM
Balloon_interface::brew_molecule (SCM smob) 
{
  Grob *me= unsmob_grob (smob);

  SCM cb = me->get_grob_property ("original-callback");
  SCM scm_mol  =  SCM_EOL;

  if (gh_procedure_p (cb))
    {
      scm_mol = scm_call_1 (cb, smob);
    }

  if (!unsmob_molecule (scm_mol))
    return scm_mol;

  SCM scm_off = me->get_grob_property ("balloon-text-offset");

  if (!is_number_pair (scm_off))
    return scm_mol;

  Offset off = ly_scm2offset (scm_off);
  Molecule * m = unsmob_molecule (scm_mol);
  Box orig_extent = m->extent_box ();
  Box box_extent = orig_extent;

  SCM widen = me->get_grob_property ("balloon-padding");
  Real w = .1;
  if (gh_number_p (widen))
    {
      w = gh_scm2double (widen);
    }
  box_extent.widen (w, w);
  
  
  Molecule fr = Lookup::frame (box_extent, 0.1, 0.05);

  
  fr.add_molecule (*m);



  SCM bt = me->get_grob_property ("balloon-text");
  SCM chain = Font_interface::font_alist_chain (me);
  chain = gh_cons (me->get_grob_property ("balloon-text-props"), chain);


  SCM text = Text_item::interpret_markup (me->get_paper ()->self_scm (), chain, bt);

  
  Molecule *text_mol = unsmob_molecule (text);
  
  Offset z1;

  for (int i = X_AXIS; i < NO_AXES; i++)
    {
      Axis  a((Axis)i);
      z1[a] = box_extent [a].linear_combination (sign (off[a]));
      text_mol->align_to (a, -sign (off[a]));
    }

  Offset z2 = z1 + off;
  
  fr.add_molecule (Lookup::line (0.1, z1, z2));

  text_mol->translate (z2);
  fr.add_molecule (*text_mol);
  
  fr = Molecule (orig_extent, fr.get_expr ());
  return fr.smobbed_copy ();
}

ADD_INTERFACE (Balloon_interface,"text-balloon-interface",
	       "comic books.",
	       "balloon-padding balloon-text-props balloon-text-offset balloon-text balloon-original-callback");

