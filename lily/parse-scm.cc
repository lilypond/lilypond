/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "parse-scm.hh"

#include <cstdio>
using namespace std;

#include "lily-parser.hh"
#include "international.hh"
#include "main.hh"
#include "paper-book.hh"
#include "source-file.hh"

/* Pass string to scm parser, evaluate one expression.
   Return result value and #chars read.

   Thanks to Gary Houston <ghouston@freewire.co.uk>  */
SCM
internal_ly_parse_scm (Parse_start *ps)
{
  Source_file *sf = ps->start_location_.get_source_file ();
  SCM port = sf->get_port ();

  int off = ps->start_location_.start () - sf->c_str ();

  scm_seek (port, scm_long2num (off), scm_long2num (SEEK_SET));
  SCM from = scm_ftell (port);

  scm_set_port_line_x (port,  scm_from_int (ps->start_location_.line_number () -1));
  scm_set_port_column_x (port,  scm_from_int (ps->start_location_.column_number () -1));
  
  SCM answer = SCM_UNSPECIFIED;
  SCM form = scm_read (port);

  SCM to = scm_ftell (port);
  ps->nchars = scm_to_int (to) - scm_to_int (from);
  

  /* Read expression from port.  */
  if (!SCM_EOF_OBJECT_P (form))
    {
      if (ps->safe_)
	{
	  static SCM module = SCM_BOOL_F;
	  if (module == SCM_BOOL_F)
	    {
	      SCM function = ly_lily_module_constant ("make-safe-lilypond-module");
	      module = scm_call_0 (function);
	    }
	  
	  // We define the parser so trusted Scheme functions can
	  // access the real namespace underlying the parser.
	  if (ps->parser_)
	    scm_module_define (module, ly_symbol2scm ("parser"),
			       ps->parser_->self_scm());
	  answer = scm_eval (form, module);
	}
      else
	answer = scm_primitive_eval (form);
    }

  /* Don't close the port here; if we re-enter this function via a
     continuation, then the next time we enter it, we'll get an error.
     It's a string port anyway, so there's no advantage to closing it
     early. */
  // scm_close_port (port);

  return answer;
}

SCM
catch_protected_parse_body (void *p)
{
  Parse_start *ps = (Parse_start *) p;

  return internal_ly_parse_scm (ps);
}

SCM
parse_handler (void *data, SCM tag, SCM args)
{
  Parse_start *ps = (Parse_start *) data;

  ps->start_location_.error (_ ("GUILE signaled an error for the expression beginning here"));

  if (scm_ilength (args) > 2)
    scm_display_error_message (scm_cadr (args), scm_caddr (args), scm_current_error_port ());

  if (tag == ly_symbol2scm ("read-error"))
    ps->nchars = 1;

  return SCM_UNDEFINED;
}

SCM
protected_ly_parse_scm (Parse_start *ps)
{
  /*
    Catch #t : catch all Scheme level errors.
   */
  return scm_internal_catch (SCM_BOOL_T, 
			     &catch_protected_parse_body,
			     (void *) ps,
			     &parse_handler, (void *) ps);
}

bool parse_protect_global = true;
bool parsed_objects_should_be_dead = false;

/* Try parsing.  Upon failure return SCM_UNDEFINED.
   FIXME: shouldn't we return SCM_UNSCPECIFIED -- jcn  */
SCM
ly_parse_scm (char const *s, int *n, Input i, bool safe, Lily_parser *parser)
{
  Parse_start ps;
  ps.str = s;
  ps.start_location_ = i;
  ps.safe_ = safe;
  ps.parser_ = parser;

  SCM ans = parse_protect_global ? protected_ly_parse_scm (&ps)
    : internal_ly_parse_scm (&ps);
  *n = ps.nchars;

  return ans;
}

