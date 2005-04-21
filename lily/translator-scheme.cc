/*
  translator-scheme.cc -- implement Scheme context functions

  source file of the GNU LilyPond music typesetter

  (c) 2002--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "context-def.hh"
#include "translator-group.hh"
#include "moment.hh"

LY_DEFINE (ly_translator_name, "ly:translator-name",
	   1, 0, 0, (SCM trans),
	   "Return the type name of the translator object @var{trans}. "
	   "The name is a symbol.")
{
  Translator *tr = unsmob_translator (trans);
  SCM_ASSERT_TYPE (tr, trans, SCM_ARG1, __FUNCTION__, "Translator");
  char const *nm = classname (tr);
  return ly_symbol2scm (nm);
}

LY_DEFINE (ly_translator_now, "ly:translator-now",
	   1, 0, 0, (SCM trans),
	   "Return now-moment of translater TRANS")
{
  Translator *tr = unsmob_translator (trans);
  SCM_ASSERT_TYPE (tr, trans, SCM_ARG1, __FUNCTION__, "Translator");
  return tr->now_mom ().smobbed_copy ();
}

LY_DEFINE (ly_translator_description, "ly:translator-description",
	   1, 0, 0, (SCM me),
	   "Return an alist of properties of  translator @var{me}.")
{
  Translator *tr = unsmob_translator (me);
  SCM_ASSERT_TYPE (tr, me, SCM_ARG1, __FUNCTION__, "Translator");
  return tr->translator_description ();
}

LY_DEFINE (ly_translator_property, "ly:translator-property",
	   2, 0, 0, (SCM translator, SCM sym),
	   "Return the value of a value in translator @var{g} of property @var{sym}. "
	   "It will return @code{' ()} (end-of-list) "
	   "if  @var{sym} is undefined in @var{g}."
	   "\n\n")
{
  Translator *sc = unsmob_translator (translator);
  SCM_ASSERT_TYPE (sc, translator, SCM_ARG1, __FUNCTION__, "translator");
  SCM_ASSERT_TYPE (scm_is_symbol (sym), sym, SCM_ARG2, __FUNCTION__, "symbol");

  return sc->internal_get_property (sym);
}

int
Translator::print_smob (SCM s, SCM port, scm_print_state *)
{
  Translator *me = (Translator *) SCM_CELL_WORD_1 (s);
  scm_puts ("#<Translator ", port);
  scm_puts (classname (me), port);
  scm_puts (" >", port);
  return 1;
}

