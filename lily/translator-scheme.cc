/*
  translator-scheme.cc -- implement Scheme context functions

  source file of the GNU LilyPond music typesetter

  (c) 2002--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "context-def.hh"
#include "translator-group.hh"
#include "moment.hh"

LY_DEFINE (ly_translator_name, "ly:translator-name",
	   1, 0, 0, (SCM trans),
	   "Return the type name of the translator object @var{trans}."
	   "  The name is a symbol.")
{
  LY_ASSERT_SMOB (Translator, trans, 1);
  Translator *tr = unsmob_translator (trans);
  char const *nm = tr->class_name ();
  return ly_symbol2scm (nm);
}

LY_DEFINE (ly_translator_description, "ly:translator-description",
	   1, 0, 0, (SCM me),
	   "Return an alist of properties of translator @var{me}.")
{
  LY_ASSERT_SMOB (Translator, me, 1);
  Translator *tr = unsmob_translator (me);
  return tr->translator_description ();
}

