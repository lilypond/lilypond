/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef PARSE_SCM_HH
#define PARSE_SCM_HH

#include "input.hh"
#include "lily-guile.hh"

extern bool parse_protect_global;
extern bool parsed_objects_should_be_dead;

class Parse_start
{
public:
  SCM form_;
  Input &location_;
  bool safe_;
  Lily_parser *parser_;

  Parse_start (SCM form, Input &location, bool safe, Lily_parser *parser)
      : form_ (form), location_ (location), safe_ (safe), parser_ (parser)
  {
  }
};

SCM catch_protected_parse_body (void *);
SCM protected_ly_parse_scm (Parse_start *, bool);
SCM ly_parse_scm (Input &, bool, Lily_parser *);
SCM ly_eval_scm (SCM, Input, bool, Lily_parser *);

#endif /* PARSE_SCM_HH */
