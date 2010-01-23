/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "context.hh"
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


LY_DEFINE (ly_translator_context, "ly:translator-context",
	   1, 0, 0, (SCM trans),
	   "Return the context of the translator object @var{trans}.")
{
  LY_ASSERT_SMOB (Translator, trans, 1);
  Translator *tr = unsmob_translator (trans);

  Context *c = tr->context ();
  return c ? c->self_scm () : SCM_BOOL_F;
}
