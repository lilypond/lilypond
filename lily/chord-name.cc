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
  word is roman text or property-styled text:
   "text"
   ("text" . property-alist)
 */

Molecule
Chord_name::ly_word2molecule (SCM word, Real* x) const
{
  *x = 0;

  SCM options = SCM_EOL;
  if (gh_pair_p (word))
    {
      options = gh_cdr (word);
      word = gh_car (word);
    }

  if (gh_string_p (word))
    {
      /*
	UGH. Should read from font metric structure.
      */
      Real ex = lookup_l ()->text ("", "x",
				   paper_l ()).extent (Y_AXIS).length ();
      Real em = lookup_l ()->text ("", "m",
				   paper_l ()).extent (X_AXIS).length ();

      String w = ly_scm2string (word);

      String style;
      SCM s = scm_assoc (ly_symbol2scm ("style"), options);
      if (s != SCM_BOOL_F)
	{
	  style = ly_scm2string (gh_cdr (s));
	}

      Offset offset;
      int size = 0;
      /*
	urg, `type'
      */
      s = scm_assoc (ly_symbol2scm ("type"), options);
      if (s != SCM_BOOL_F && ly_scm2string (gh_cdr (s)) == "super")
	{
	  Real super_y = ex / 2;
	  offset = Offset (0, super_y);
	  if (!size)
	    size = -2;
	}

      s = scm_assoc (ly_symbol2scm ("size"), options);
      if (s != SCM_BOOL_F)
	{
	  size = gh_scm2int (gh_cdr (s));
	}

      s = scm_assoc (ly_symbol2scm ("offset"), options);
      if (s != SCM_BOOL_F)
	{
	  // hmm
	  SCM o = gh_cdr (s);
	  if (gh_pair_p (o))
	    offset = Offset (0, gh_scm2double (gh_cdr (o))) * ex;
	  *x = gh_scm2double (gh_car (o)) * em;
	}

      Molecule mol;
      s = scm_assoc (ly_symbol2scm ("font"), options);
      if (s != SCM_BOOL_F && ly_scm2string (gh_cdr (s)) == "feta")
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
	  Real x;
	  Molecule m = ly_word2molecule (gh_car (text), &x);
	  if (!m.empty_b ())
	    mol.add_at_edge (X_AXIS, RIGHT, m, x);
	  text = gh_cdr (text);
	}
      text = gh_car (text);
    }  
  Real x;
  Molecule m = ly_word2molecule (text, &x);
  if (!m.empty_b ())
    mol.add_at_edge (X_AXIS, RIGHT, m, x);
  return mol;
}

MAKE_SCHEME_SCORE_ELEMENT_CALLBACKS(Chord_name);

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

Chord_name::Chord_name (SCM s)
  : Item (s)
{
}
