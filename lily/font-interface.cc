/*   
  font-interface.cc --  implement Font_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "all-font-metrics.hh"
#include "font-metric.hh"
#include "font-interface.hh"
#include "grob.hh"
#include "paper-def.hh"
#include "warn.hh"


/*
  TODO revise font handling.


* relative sizes should relate to staff-space, eg.  font-staff-space
= 1.2 ^ relative-size

* If a relative size is given, lily should magnify the closest
design size font to match that. (ie. fonts should have variable
scaling)

(this requires that fonts are stored as (filename , designsize))


  
 */

SCM
Font_interface::font_alist_chain (Grob *me)
{
  /*
    Ugh: why the defaults?
   */
  SCM defaults = ly_cdr (scm_assoc (ly_symbol2scm ("font-defaults"),
				    me->paper_l ()->style_sheet_));

  SCM ch = scm_list_n (me->mutable_property_alist_,
		    me->immutable_property_alist_,
		    defaults,
		    SCM_UNDEFINED);

  return ch;
}

/*
  todo: split up this func, reuse in text_item? 
 */
Font_metric *
Font_interface::get_default_font (Grob*me)
{
  Font_metric * fm =  unsmob_metrics (me->get_grob_property ("font"));
  if (fm)
    return fm;

  fm = get_font (me,  font_alist_chain (me));
  me->set_grob_property ("font", fm->self_scm ());
  return fm;
}


SCM
ly_font_interface_get_default_font (SCM grob)
{
  Grob * gr  = unsmob_grob (grob);
  SCM_ASSERT_TYPE(gr, grob, SCM_ARG1, __FUNCTION__, "grob");

  return Font_interface::get_default_font (gr)->self_scm ();
}

SCM
ly_font_interface_get_font (SCM grob, SCM alist)
{
  Grob * gr  = unsmob_grob (grob);
  SCM_ASSERT_TYPE(gr, grob, SCM_ARG1, __FUNCTION__, "grob");

  Font_metric*fm=
    Font_interface::get_font (gr, gh_cons (alist,
					   Font_interface::font_alist_chain (gr)));

  return fm->self_scm();
}



Font_metric *
Font_interface::get_font (Grob *me, SCM chain)
{
  SCM name = me->get_grob_property ("font-name");
  
  if (!gh_string_p (name))
    {
      SCM ss = me->paper_l ()->style_sheet_;

      SCM proc = ly_cdr (scm_assoc (ly_symbol2scm ("properties-to-font"),
				    ss));

      SCM fonts = ly_cdr (scm_assoc (ly_symbol2scm ("fonts"), ss));

      assert (gh_procedure_p (proc));
      name = gh_call2 (proc, fonts, chain);
    }
  
  SCM mag = me->get_grob_property ("font-magnification");
  Real rmag = gh_number_p (mag) ? gh_scm2double (mag) : 1.0;
  
  Font_metric *fm = me->paper_l ()->find_font (name, rmag);
  return fm;
}

SCM
Font_interface::add_style (Grob* me, SCM style, SCM chain)
{
  assert (gh_symbol_p (style));
  
  SCM sheet = me->paper_l ()->style_sheet_;
      
  SCM style_alist = ly_cdr (scm_assoc (ly_symbol2scm ("style-alist"), sheet));
  SCM entry = scm_assoc (style, style_alist);
  if (gh_pair_p (entry))
    {
      chain = gh_cons (ly_cdr (entry), chain);
    }
  return chain;
}

/*
SCM routines:  

Interpreting music...
MIDI output to wtk1-fugue2.midi...
Track ... 

real	0m31.862s
user	0m29.110s
sys	0m0.260s

real	0m26.964s
user	0m24.850s
sys	0m0.280s


so a 14% speedup.

*/

static SCM shape_sym, family_sym, series_sym, rel_sz_sym, design_sz_sym, wild_sym;


static void
init_syms ()
{
  shape_sym  = scm_permanent_object (ly_symbol2scm ("font-shape"));
  family_sym = scm_permanent_object (ly_symbol2scm ("font-family"));
  series_sym = scm_permanent_object (ly_symbol2scm ("font-series"));
  rel_sz_sym = scm_permanent_object (ly_symbol2scm ("font-relative-size"));
  design_sz_sym = scm_permanent_object (ly_symbol2scm ("font-design-size"));
  wild_sym = scm_permanent_object (ly_symbol2scm ("*"));

  scm_c_define_gsubr ("ly-get-default-font", 1 , 0, 0,
		      (Scheme_function_unknown) ly_font_interface_get_default_font);
  scm_c_define_gsubr ("ly-get-font", 2, 0, 0,
		      (Scheme_function_unknown) ly_font_interface_get_font);
}


bool
Font_interface::wild_compare (SCM field_val, SCM val)
{
  return (val == SCM_BOOL_F || field_val == wild_sym || field_val == val);
}

ADD_SCM_INIT_FUNC (Font_interface_syms,init_syms);


MAKE_SCHEME_CALLBACK (Font_interface,properties_to_font_name,2);
SCM
Font_interface::properties_to_font_name (SCM fonts, SCM alist_chain)
{
  SCM shape = SCM_BOOL_F;
  SCM family = SCM_BOOL_F;
  SCM series = SCM_BOOL_F;

  
  SCM point_sz = ly_assoc_chain (design_sz_sym, alist_chain);
  SCM rel_sz = SCM_BOOL_F;

  shape = ly_assoc_chain (shape_sym, alist_chain);
  family = ly_assoc_chain (family_sym, alist_chain);
  series = ly_assoc_chain (series_sym, alist_chain);

  if (gh_pair_p (shape))
    shape = ly_cdr (shape);
  if (gh_pair_p (family))
    family = ly_cdr (family);
  if (gh_pair_p (series))
    series = ly_cdr (series);


  if (gh_pair_p (point_sz))
    point_sz = ly_cdr (point_sz);
  else
    {
      rel_sz = ly_assoc_chain (rel_sz_sym, alist_chain);
      if (gh_pair_p (rel_sz))
	rel_sz = ly_cdr (rel_sz);
    }

  for (SCM s = fonts ; gh_pair_p (s); s = ly_cdr (s))
    {
      SCM qlist = ly_caar (s);

      if (!wild_compare (scm_list_ref (qlist, gh_int2scm (1)), series))
	continue;
      if (!wild_compare (scm_list_ref (qlist, gh_int2scm (2)), shape))
	continue;
      if (!wild_compare (scm_list_ref (qlist, gh_int2scm (3)), family))
	continue;
  
      if (point_sz == SCM_BOOL_F && !wild_compare (ly_car (qlist), rel_sz))
	continue;
          
      SCM qname = ly_cdar (s);
      return qname;
    }

  warning (_ ("couldn't find any font satisfying "));
  scm_write (scm_list_n (point_sz, shape, series , family, rel_sz, SCM_UNDEFINED), scm_current_error_port ());
  scm_flush (scm_current_error_port ());
 
  return ly_str02scm ("cmr10");
  
}



ADD_INTERFACE (Font_interface, "font-interface",
  "Any symbol that is typeset through fixed sets of glyphs (ie. fonts)",
  "font-magnification font-style font font-series font-shape font-family font-name font-design-size font-relative-size");
