/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "context-def.hh"
#include "context.hh"
#include "moment.hh"
#include "translator-group.hh"

LY_DEFINE (ly_translator_context, "ly:translator-context", 1, 0, 0, (SCM trans),
           "Return the context of the translator object @var{trans}.")
{
  LY_ASSERT_SMOB (Translator, trans, 1);
  Translator *tr = unsmob<Translator> (trans);

  Context *c = tr->context ();
  return c ? c->self_scm () : SCM_BOOL_F;
}
