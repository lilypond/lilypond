#include <stdio.h>

#include "lily-guile.hh"
#include "parse-scm.hh"
#include "string.hh"
#include "source-file.hh"

/*
  Pass string to scm parser, evaluate one expression.
  Return result value and #chars read.

  Thanks to Gary Houston <ghouston@freewire.co.uk>

  Need guile-1.3.4 (>1.3 anyway) for ftell on str ports -- jcn
*/
SCM
internal_ly_parse_scm (Parse_start * ps)
{
  Source_file* sf =ps->start_location_.source_file_;
  SCM port = sf->get_port();

  int off = ps->start_location_.defined_str0_ - sf->to_str0();
  
  scm_seek (port, scm_long2num (off), scm_long2num (SEEK_SET));
  SCM from = scm_ftell (port);

  SCM form;
  SCM answer = SCM_UNSPECIFIED;

  /* Read expression from port */
  if (!SCM_EOF_OBJECT_P (form = scm_read (port)))
    answer = scm_primitive_eval (form);
 
  /*
   After parsing

 (begin (foo 1 2))

   all seems fine, but after parsing

 (foo 1 2)

   read_buf has been advanced to read_pos - 1,
   so that scm_ftell returns 1, instead of #parsed chars
   */
  
  /*
    urg: reset read_buf for scm_ftell
    shouldn't scm_read () do this for us?
  */
  scm_fill_input (port);
  SCM to = scm_ftell (port);
  ps->nchars = gh_scm2int (to) - gh_scm2int (from);

  /* Don't close the port here; if we re-enter this function via a
     continuation, then the next time we enter it, we'll get an error.
     It's a string port anyway, so there's no advantage to closing it
     early.

     scm_close_port (port);
  */

  return answer;
}


SCM
catch_protected_parse_body (void *p)
{
  Parse_start *ps = (Parse_start*) p;
  return internal_ly_parse_scm (ps);
}

SCM 
parse_handler (void * data, SCM tag, SCM args)
{
  Parse_start* ps = (Parse_start*) data;

  ps->start_location_.error (_("GUILE signaled an error for the expression begining here"));

  if (scm_ilength (args) > 2)
    scm_display_error_message (gh_cadr (args), gh_caddr(args), scm_current_error_port());

  /*
    The following is a kludge; we should probably search for
    [a-z][0-9] (a note), and start before that.
   */
  ps->nchars = 1;
    
  return SCM_EOL;
}

/*
  Do some magical incantations: if not, lily will exit on the first
  GUILE error, leaving no location trace. 
 */


#if GUILE_MINOR_VERSION < 7
  #define READ_ERROR "misc-error"
  #else
  #define READ_ERROR "read-error"
#endif

SCM
protected_ly_parse_scm (Parse_start *ps)
{
  return scm_internal_catch (ly_symbol2scm (READ_ERROR),
			     &catch_protected_parse_body,
			     (void*)ps,
			     &parse_handler, (void*)ps);

}


static bool protect = true;

LY_DEFINE(set_parse_protect, "ly-set-parse-protect",
	  1,0,0, (SCM t),
	  "If protection is switched on, errors in inline scheme are caught.
If off, GUILE will halt on errors, and give a stack trace. Default is protected evaluation.")
{
  protect =  (t == SCM_BOOL_T);
  return SCM_UNSPECIFIED;
}

SCM
ly_parse_scm (char const* s, int *n, Input i)
{
  
  Parse_start ps ;
  ps.str = s;
  ps.start_location_ = i;

  SCM ans = protect ? protected_ly_parse_scm (&ps)
    : internal_ly_parse_scm (&ps);
  *n = ps.nchars;
  return ans;  
}

