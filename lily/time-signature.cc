/*   
  time-signature.cc --  implement Time_signature
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1996--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


#include "molecule.hh"
#include "text-item.hh"
#include "time-signature.hh"
#include "paper-def.hh"
#include "font-interface.hh"
#include "warn.hh"
#include "staff-symbol-referencer.hh"

MAKE_SCHEME_CALLBACK (Time_signature,brew_molecule,1);
/*
  TODO: make different functions for special and normal timesigs.
 */
SCM
Time_signature::brew_molecule (SCM smob) 
{
  Grob * me = unsmob_grob (smob);
  SCM st = me->get_grob_property ("style");
  SCM frac = me->get_grob_property ("fraction");
  int n = 4;
  int d = 4;
  if (gh_pair_p (frac))
    {
      n = gh_scm2int (ly_car (frac));
      d = gh_scm2int (ly_cdr (frac));
    }

  Molecule m;
  if (gh_symbol_p (st))
    {
      String style (ly_scm2string (scm_symbol_to_string (st)));
      if (style[0]=='1')
	{
	  m = numbered_time_signature (me, n, 0);
	}
      else
	{
	  m = special_time_signature (me, st, n, d);
	}
    }
  else
    m = numbered_time_signature (me, n,d);

  if (Staff_symbol_referencer::line_count (me) % 2 == 0)
    m.translate_axis (Staff_symbol_referencer::staff_space (me)/2 , Y_AXIS);

  return m.smobbed_copy ();
}

Molecule
Time_signature::special_time_signature (Grob *me, SCM scm_style, int n, int d)
{
  String style = ly_scm2string (scm_symbol_to_string (scm_style));

  if (style == "numbered")
    return numbered_time_signature (me, n, d);

  if ((style == "default") || (style == ""))
    style = to_string ("C");

  if (style == "C")
    {
      if /* neither C2/2 nor C4/4 */
	(((n != 2) || (d != 2)) && 
	 ((n != 4) || (d != 4)))
	{
	  return numbered_time_signature (me, n, d);
	}
    }

  String char_name = style + to_string (n) + "/" + to_string (d);
  me->set_grob_property ("font-family", ly_symbol2scm ("music"));
  Molecule out =
    Font_interface::get_default_font (me)->find_by_name ("timesig-" + char_name);
  if (!out.empty_b ())
    return out;

  /*
    If there is no such symbol, we default to the numbered style.
    (Here really with a warning!)
  */
  me->warning (_f ("time signature symbol `%s' not found; "
		   "reverting to numbered style", char_name));
  return numbered_time_signature (me, n, d);
}

Molecule
Time_signature::numbered_time_signature (Grob*me,int num, int den)
{
  SCM chain = Font_interface::font_alist_chain (me);
  me->set_grob_property("font-family", ly_symbol2scm ("number"));

  Molecule n = Text_item::text2molecule (me,
					 scm_makfrom0str (to_string (num).to_str0 ()),
					 chain);
  Molecule d = Text_item::text2molecule (me,
					 scm_makfrom0str (to_string (den).to_str0 ()),
					 chain);
  n.align_to (X_AXIS, CENTER);
  d.align_to (X_AXIS, CENTER);
  Molecule m;
  if (den)
    {
      m.add_at_edge (Y_AXIS, UP, n, 0.0);
      m.add_at_edge (Y_AXIS, DOWN, d, 0.0);
    }
  else
    {
      m = n;
      m.align_to (Y_AXIS, CENTER);
    }

  m.align_to (X_AXIS, LEFT);
  
  return m;
}

ADD_INTERFACE (Time_signature,"time-signature-interface",
  "A time signature, in different styles.
  The following values for 'style are are recognized:

    @table @samp
      @item @code{C}
        4/4 and 2/2 are typeset as C and struck C, respectively.  All
        other time signatures are written with two digits.

      @item @code{neo_mensural}
        2/2, 3/2, 2/4, 3/4, 4/4, 6/4, 9/4, 4/8, 6/8 and 9/8 are
        typeset with neo-mensural style mensuration marks.  All other time
        signatures are written with two digits.

      @item @code{mensural}
        2/2, 3/2, 2/4, 3/4, 4/4, 6/4, 9/4, 4/8, 6/8 and 9/8 are
        typeset with mensural style mensuration marks.  All other time
        signatures are written with two digits.

      @item @code{1xxx}
        All time signatures are typeset with a single
        digit, e.g. 3/2 is written as 3. (Any symbol starting
	with the digit @code{1} will do.)
    @end table

See also the test-file @file{input/test/time.ly}.
",
  "fraction style");
