/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2026 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

struct Parser_error_handler
{
  // Start of the to-be-parsed form.
  const Input start_;

  // The pre-unwind handler, which prints the Scheme error.
  static SCM handle_error_before_unwinding (void *data, SCM tag, SCM args)
  {
    const auto &self = *static_cast<const Parser_error_handler *> (data);
    return self.handle_error_before_unwinding (tag, args);
  }

  // The outer handler, which just returns SCM_UNDEFINED, leaving the caller to
  // do something appropriate.  Unreachable with -dno-protected-scheme-parsing.
  static SCM handle_error_after_unwinding (void * /*data*/, SCM /*tag*/,
                                           SCM /*args*/)
  {
    return SCM_UNDEFINED;
  }

private:
  SCM handle_error_before_unwinding (SCM tag, SCM args) const;
};

SCM evaluate_embedded_scheme (SCM form, Input const &start,
                              Lily_parser *parser);
SCM parse_embedded_scheme (const Input &start, Lily_parser *parser,
                           Input *parsed_output);

#endif /* PARSE_SCM_HH */
