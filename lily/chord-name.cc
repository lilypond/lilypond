/*
  chord-name.cc -- implement Chord_name

  source file of the GNU LilyPond music typesetter

  (c)  1999--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "chord-name.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "score-element.hh"
#include "paper-column.hh"
#include "line-of-score.hh"

/*
  TODO: move text lookup out of Chord_name
 */

/*
  word is roman text or property-styled text:
   "text"
   ("text" . property-alist)
 */
Molecule
Chord_name::ly_word2molecule (Score_element * me, SCM word, Real* x) 
{
  *x = 0;

  SCM options_alist = SCM_EOL;
  if (gh_pair_p (word))
    {
      options_alist = gh_cdr (word);
      word = gh_car (word);
    }

  if (gh_string_p (word))
    {
      /*
	UGH. Should read from font metric structure.
      */
      Real ex = me->lookup_l ()->text ("", "x",
				   me->paper_l ()).extent (Y_AXIS).length ();
      Real em = me->lookup_l ()->text ("", "m",
				   me->paper_l ()).extent (X_AXIS).length ();

      String w = ly_scm2string (word);

      String style;
      SCM s = scm_assoc (ly_symbol2scm ("style"), options_alist);
      if (s != SCM_BOOL_F)
	{
	  style = ly_scm2string (gh_cdr (s));
	}

      Offset offset;
      int size = 0;
      /*
	urg, `type'
      */
      s = scm_assoc (ly_symbol2scm ("type"), options_alist);
      if (s != SCM_BOOL_F && ly_scm2string (gh_cdr (s)) == "super")
	{
	  Real super_y = ex / 2;
	  offset = Offset (0, super_y);
	  if (!size)
	    size = -2;
	}

      s = scm_assoc (ly_symbol2scm ("size"), options_alist);
      if (s != SCM_BOOL_F)
	{
	  size = gh_scm2int (gh_cdr (s));
	}

      s = scm_assoc (ly_symbol2scm ("offset"), options_alist);
      if (s != SCM_BOOL_F)
	{
	  // hmm
	  SCM o = gh_cdr (s);
	  if (gh_pair_p (o))
	    offset = Offset (0, gh_scm2double (gh_cdr (o))) * ex;
	  *x = gh_scm2double (gh_car (o)) * em;
	}

      Molecule mol;
      s = scm_assoc (ly_symbol2scm ("font"), options_alist);
      if (s != SCM_BOOL_F && ly_scm2string (gh_cdr (s)) == "feta")
        mol = me->paper_l ()->lookup_l (size)->afm_find (w);
      else
	mol = me->paper_l ()->lookup_l (size)->text (style, w, me->paper_l ());

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
Chord_name::ly_text2molecule (Score_element * me, SCM text) 
{
  Molecule mol;
  if (gh_list_p (text))
    {
      while (gh_cdr (text) != SCM_EOL)
        {
	  Real x;
	  Molecule m = ly_word2molecule (me, gh_car (text), &x);
	  if (!m.empty_b ())
	    mol.add_at_edge (X_AXIS, RIGHT, m, x);
	  text = gh_cdr (text);
	}
      text = gh_car (text);
    }  
  Real x;
  Molecule m = ly_word2molecule (me,text, &x);
  if (!m.empty_b ())
    mol.add_at_edge (X_AXIS, RIGHT, m, x);
  return mol;
}

MAKE_SCHEME_CALLBACK (Chord_name,after_line_breaking,1);
SCM
Chord_name::after_line_breaking (SCM smob)
{
  Item* me = dynamic_cast<Item*> (unsmob_element (smob));
  assert (me);
    
  SCM s = me->get_elt_property ("begin-of-line-visible");
  if (to_boolean (s))
    {
      if (Paper_column::rank_i (me->column_l ()) -
	  /*
	    hmm, what's my column number in this line?
	    why doesn't this work?
	    me->line_l ()->rank_i_ > 2)
	  */
	  me->line_l ()->spanned_rank_iv ()[LEFT] > 1)
	me->suicide ();
    }
  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Chord_name,brew_molecule,1);
SCM
Chord_name::brew_molecule (SCM smob) 
{
  Score_element *me = unsmob_element (smob);
  SCM style = me->get_elt_property ("style");

  if (!gh_string_p (style))
    style = ly_str02scm ("banter");

  SCM inversion = me-> get_elt_property ("inversion");
  if (inversion == SCM_EOL)
    inversion = SCM_BOOL_F;

  SCM bass =  me->get_elt_property ("bass");
  if (bass == SCM_EOL)
    bass = SCM_BOOL_F;

  SCM pitches =  me->get_elt_property ("pitches");
  SCM func = me->get_elt_property (ly_symbol2scm ("chord-name-function"));
  SCM text = gh_call3 (func, style, pitches, gh_cons (inversion, bass));

  return ly_text2molecule (me, text).create_scheme ();
}
