/*   
  text-item.cc -- implement Text_item

  source file of the GNU LilyPond music typesetter
  
 (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "debug.hh"
#include "text-item.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "staff-symbol-referencer.hh"
#include "staff-symbol-referencer.hh"
#include "main.hh"
#include "all-font-metrics.hh"
#include "afm.hh"


/*
  text: string | (markup sentence)
  markup: markup-symbol | (markup-symbol . parameter)
  sentence: text | sentence text
  

  Properties:

  * Font:
  ---* Type:
  ------* Series: medium, bold
  ------* Shape: upright, italic, slanted
  ------* Family: roman, music, orator, typewriter

  ---* Size:
  ------* size: ...,-2,-1,0,1,2,... (style-sheet -> cmrXX, fetaXX)
  ------* points: 11,13,16,20,23,26 (for feta)
  ------* magnification: UNSIGNED

  * Typesetting:
  ---* kern: INT (staff-space)
  ---* align: horizontal/vertical / lines / rows
 */
Molecule
Text_item::text2molecule (Score_element *me, SCM text, SCM properties) 
{
  if (gh_string_p (text))
    return string2molecule (me, text, properties);
  else if (gh_list_p (text))
    {
      if (!gh_pair_p (gh_car (text)) && gh_string_p (gh_car (text)))
	return string2molecule (me, gh_car (text), properties);
      else
	return markup_sentence2molecule (me, text, properties);
    }
  return Molecule ();
}

static
SCM
get_elt_property (Score_element *me, char const *name)
{
  SCM s = me->get_elt_property (name);
  if (s == SCM_EOL)
    error (_f ("No `%s' defined for %s", name, me->name ()));
  return s;
}

Molecule
Text_item::string2molecule (Score_element *me, SCM text, SCM properties)
{
  SCM style = scm_assoc (ly_symbol2scm ("font-style"), properties);
  SCM paper = me->get_elt_property ("style-sheet");
  if (paper == SCM_EOL)
    paper = scm_string_to_symbol (me->paper_l ()->get_scmvar ("style_sheet"));

  SCM font_name;
  if (gh_pair_p (style))
    {
      SCM f = get_elt_property (me, "style-to-font-name");
      font_name = gh_call2 (f, paper, gh_cdr (style));
    }
  else
    {
      SCM f = get_elt_property (me, "properties-to-font-name");
      font_name = gh_call2 (f, paper, properties);
    }
   
  // should move fallback to scm
  if (!gh_string_p (font_name))
    font_name = ly_str02scm ("cmr10");
    
  SCM lookup = scm_assoc (ly_symbol2scm ("lookup"), properties);

  Molecule mol;
  if (gh_pair_p (lookup) && ly_symbol2string (gh_cdr (lookup)) == "name")
    mol = lookup_character (me, font_name, text);
  else
    mol = lookup_text (me, font_name, text);
  
  return mol;
}

/*
  caching / use some form of Lookup without 'paper'?
*/
Molecule
Text_item::lookup_character (Score_element *me, SCM font_name, SCM char_name)
{
  Adobe_font_metric *afm = all_fonts_global_p->find_afm (ly_scm2string (font_name));

  if (!afm)
    {
      warning (_f ("can't find font: `%s'", ly_scm2string (font_name)));
      warning (_f ("(search path: `%s')", global_path.str ().ch_C()));
      error (_ ("Aborting"));
    }
  
  AFM_CharMetricInfo const *metric =
    afm->find_char_metric (ly_scm2string (char_name), true);

  if (!metric)
    {
      Molecule m;
      m.set_empty (false);
      return m;
    }

  SCM list = gh_list (ly_symbol2scm ("char"),
		      gh_int2scm (metric->code),
		      SCM_UNDEFINED);
  
  list = fontify_atom (afm, list);
  return Molecule (afm_bbox_to_box (metric->charBBox), list);
}

Molecule
Text_item::lookup_text (Score_element *me, SCM font_name, SCM text)
{
  SCM magnification = me->get_elt_property ("font-magnification");
  Font_metric* metric = 0;
  if (gh_number_p (magnification))
    metric = all_fonts_global_p->find_scaled (ly_scm2string (font_name),
					      gh_scm2int (magnification));
  else
    metric = all_fonts_global_p->find_font (ly_scm2string (font_name));
  
  SCM list = gh_list (ly_symbol2scm ("text"), text, SCM_UNDEFINED);
  list = fontify_atom (metric, list);
  
  return Molecule (metric->text_dimension (ly_scm2string (text)), list);
}

Molecule
Text_item::markup_sentence2molecule (Score_element *me, SCM markup_sentence,
				     SCM properties)
{
  SCM markup = gh_car (markup_sentence);
  SCM sentence = gh_cdr (markup_sentence);
  SCM f = get_elt_property (me, "markup-to-properties");
  SCM p = gh_append2 (gh_call1 (f, markup), properties);

  Axis align = X_AXIS;
  SCM a = scm_assoc (ly_symbol2scm ("align"), p);
  if (gh_pair_p (a) && gh_number_p (gh_cdr (a)))
    align = (Axis)gh_scm2int (gh_cdr (a));

  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real kern = 0;
  SCM k = scm_assoc (ly_symbol2scm ("kern"), p);
  if (gh_pair_p (k) && gh_number_p (gh_cdr (k)))
    kern = gh_scm2double (gh_cdr (k)) * staff_space;
			     
  Real raise = 0;
  SCM r = scm_assoc (ly_symbol2scm ("raise"), p);
  if (gh_pair_p (r) && gh_number_p (gh_cdr (r)))
    raise = gh_scm2double (gh_cdr (r)) * staff_space;

  Offset o (align == X_AXIS ? kern : 0,
	    (align == Y_AXIS ? - kern : 0) + raise);

  Molecule mol;
  while (gh_pair_p (sentence))
    {
      Molecule m = text2molecule (me, gh_car (sentence), p);
      if (!m.empty_b ())
	{
	  m.translate (o);
	  mol.add_at_edge (align, align == X_AXIS ? RIGHT : DOWN, m, 0);
	}
      sentence = gh_cdr (sentence);
    }
  return mol;
}

MAKE_SCHEME_CALLBACK (Text_item, brew_molecule, 1);
SCM 
Text_item::brew_molecule (SCM smob)
{
  Score_element *me = unsmob_element (smob);
  
  SCM text = me->get_elt_property ("text");

  SCM properties = gh_append2 (me->immutable_property_alist_,
			       me->mutable_property_alist_);

  Molecule mol = Text_item::text2molecule (me, text, properties);

  SCM space = me->get_elt_property ("word-space");
  if (gh_number_p (space))
    {
      Molecule m;
      m.set_empty (false);
      mol.add_at_edge (X_AXIS, RIGHT, m, gh_scm2double (space)
		       * Staff_symbol_referencer::staff_space (me));
    }
  return mol.create_scheme (); 
}

