/*
  chord-name.cc -- implement Chord_name

  source file of the GNU LilyPond music typesetter

  (c)  1999--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "chord-name.hh"
#include "musical-request.hh"
#include "warn.hh"
#include "debug.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "staff-symbol-referencer.hh"


/*
  TODO: move text lookup out of Chord_name
 */

/*
  word is roman text or styled text:
   "text"
   ("style" . "text")
 */
Molecule
Chord_name::ly_word2molecule (SCM word) const
{
  Dictionary<SCM> option_dict;
  if (gh_pair_p (word))
    {
      SCM options = gh_cdr (word);
      word = gh_car (word);
      while (gh_pair_p (options))
        {
	  SCM option = gh_car (options);
	  if (option != SCM_UNDEFINED && option != SCM_BOOL_F
	      && gh_pair_p (option))
	    {
	      SCM key = gh_car (option);
	      SCM val = gh_cdr (option);
	      String k;
	      if (gh_symbol_p (key))
		k = ly_symbol2string (key);
	      else if (gh_string_p (key))
	        k = ly_scm2string (key);
              else
	        continue;
              option_dict[k] = val;
	    }
	  options = gh_cdr (options);
        }
    }
  Real ex = lookup_l ()->text ("", "x", paper_l ()).extent
	    ()[Y_AXIS].length ();
  if (gh_string_p (word))
    {
      String w = ly_scm2string (word);
      Molecule mol;
      Offset offset;

      int size = 0;
      if (option_dict.elem_b ("size"))
        size = gh_scm2int (option_dict["size"]);

      String style;
      if (option_dict.elem_b ("style"))
        style = ly_scm2string (option_dict["style"]);

      if (option_dict.elem_b ("type")
	  && ly_scm2string (option_dict["type"]) == "super")
	{
	  Real super_y = ex / 2;
	  //super_y += -acc.extent ()[Y_AXIS][MIN];
	  offset = Offset (0, super_y);
	  if (!size)
	    size = -2;
	}
      if (option_dict.elem_b ("offset"))
	{
	  // hmm
	  SCM s = option_dict["offset"];
	  if (gh_pair_p (s))
	    offset = Offset (gh_scm2double (gh_car (s)),
			     gh_scm2double (gh_cdr (s))) * ex;
	}
      if (option_dict.elem_b ("font") 
	  && ly_scm2string (option_dict["font"]) == "feta")
        mol = paper_l ()->lookup_l (size)->afm_find (w);
      else
	mol = paper_l ()->lookup_l (size)->text (style, w, paper_l ());

      mol.translate (offset);
      return mol;
    }
  return Molecule ();
}

/*
  ;; text: list of word
  ;; word: string + optional list of property
  ;; property: align, kern, font (?), size
 */
Molecule
Chord_name::ly_text2molecule (SCM text) const
{
  Molecule mol;
  if (gh_list_p (text))
    {
      while (gh_cdr (text) != SCM_EOL)
        {
	  Molecule m = ly_word2molecule (gh_car (text));
	  if (!m.empty_b ())
	    mol.add_at_edge (X_AXIS, RIGHT, m, 0);
	  text = gh_cdr (text);
	}
      text = gh_car (text);
    }  
  Molecule m = ly_word2molecule (text);
  if (!m.empty_b ())
    mol.add_at_edge (X_AXIS, RIGHT, m, 0);
  return mol;
}

Molecule 
Chord_name::do_brew_molecule () const
{
  SCM style = get_elt_property ("style");
  if (style == SCM_UNDEFINED)
    style = ly_str02scm ("banter");

  SCM inversion = get_elt_property ("inversion");
  if (inversion == SCM_UNDEFINED)
    inversion = SCM_BOOL_F;

  SCM bass = get_elt_property ("bass");
  if (bass == SCM_UNDEFINED)
    bass = SCM_BOOL_F;

  SCM pitches = get_elt_property ("pitches");

  SCM text = scm_eval (gh_list (ly_symbol2scm ("chord::user-name"),
				style,
				ly_quote_scm (pitches),
				ly_quote_scm (gh_cons (inversion, bass)),
				SCM_UNDEFINED));

  return ly_text2molecule (text);
}
