/*
  translator-scheme.cc -- implement Scheme context functions

  source file of the GNU LilyPond music typesetter

  (c) 2002--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
  char const *nm = tr->class_name ();
  return ly_symbol2scm (nm);
}

LY_DEFINE (ly_translator_description, "ly:translator-description",
	   1, 0, 0, (SCM me),
	   "Return an alist of properties of  translator @var{me}.")
{
  Translator *tr = unsmob_translator (me);
  SCM_ASSERT_TYPE (tr, me, SCM_ARG1, __FUNCTION__, "Translator");
  return tr->translator_description ();
}

