/*
  paper-outputter.cc -- implement Paper_outputter

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <time.h>
#include <fstream.h>
#include <math.h>
#include <iostream.h>

#include "dimensions.hh"
#include "virtual-methods.hh"
#include "paper-outputter.hh"
#include "paper-stream.hh"
#include "molecule.hh"
#include "array.hh"
#include "string-convert.hh"
#include "debug.hh"
#include "font-metric.hh"
#include "main.hh"
#include "scope.hh"
#include "identifier.hh"
#include "lily-version.hh"


/*
  Ugh, this is messy.
 */

Paper_outputter::Paper_outputter (Paper_stream  * ps )
{
 /*
   lilypond -f scm x.ly
   guile -s x.scm
  */
  verbatim_scheme_b_ =  output_global_ch == String ("scm");

  if (verbatim_scheme_b_)
    {
	*ps << ""
	  ";;; Usage: guile -s x.scm > x.tex\n"
	  "(primitive-load-path 'lily.scm)\n"
	  "(scm-tex-output)\n"
	  ";(scm-ps-output)\n"
	  "(map (lambda (x) (display (eval x))) '(\n"
	;
    }

  stream_p_ = ps;
}

Paper_outputter::~Paper_outputter ()
{
  if (verbatim_scheme_b_)
    {
      *stream_p_ << "))";
    }
  delete stream_p_;
}


void
Paper_outputter::output_header ()
{
  if (safe_global_b)
    {
      gh_define ("security-paranoia", SCM_BOOL_T);      
    }

  SCM exp = gh_list (ly_symbol2scm ((String (output_global_ch) + "-scm").ch_C()),
		     ly_quote_scm (ly_symbol2scm ("all-definitions")),
		     SCM_UNDEFINED);
  exp = scm_eval2 (exp, SCM_EOL);
  scm_eval2 (exp, SCM_EOL);
  
  String creator;
  if (no_timestamps_global_b)
    creator = gnu_lilypond_str ();
  else
    creator = gnu_lilypond_version_str ();
  
  String generate;
  if (no_timestamps_global_b)
    generate = ".";
  else
    {
      generate = _ (", at ");
      time_t t (time (0));
      generate += ctime (&t);
      generate = generate.left_str (generate.length_i () - 1);
    }

  SCM args_scm = 
    gh_list (ly_str02scm (creator.ch_l ()),
	     ly_str02scm (generate.ch_l ()), SCM_UNDEFINED);


  SCM scm = gh_cons (ly_symbol2scm ("header"), args_scm);
  output_scheme (scm);
}



void
Paper_outputter::output_comment (String str)
{
  output_scheme (gh_list (ly_symbol2scm ("comment"),
			  ly_str02scm ((char*)str.ch_C()),
			  SCM_UNDEFINED)
		 );
}


void
Paper_outputter::output_scheme (SCM scm)
{
  /*
    we don't rename dump_scheme, because we might in the future want
    to remember Scheme. We don't now, because it sucks up a lot of memory.
  */
  dump_scheme (scm);
}


/*
  UGH.

  Should probably change interface to do less eval ( symbol ), and more
  apply (procedure, args)
 */
void
Paper_outputter::dump_scheme (SCM s)
{
  if  (verbatim_scheme_b_)
    {
      SCM p;

      p = scm_mkstrport (SCM_INUM0, 
			 scm_make_string (SCM_INUM0, SCM_UNDEFINED),
			 SCM_OPN | SCM_WRTNG,
			 "Paper_outputter::dump_scheme()");

      SCM wr =scm_eval2 (ly_symbol2scm ("write"), SCM_EOL);
      scm_apply (wr, s, gh_list (p, SCM_UNDEFINED));
  
      SCM result =  scm_strport_to_string (p);
      *stream_p_ << ly_scm2string (result);
    }
  else
    {
      SCM result = scm_eval2 (s, SCM_EOL);
      char *c=gh_scm2newstr (result, NULL);
  
      *stream_p_ << c;
      free (c);
    }
}
void
Paper_outputter::output_scope (Scope *scope, String prefix)
{
  SCM al = scope->to_alist ();
  for (SCM s = al ; gh_pair_p (s); s = gh_cdr (s))
    {
      SCM k = gh_caar (s);
      SCM v = gh_cdar (s);
      String s = ly_symbol2string (k);

      
      if (gh_string_p (v))
	{
	  output_String_def (prefix + s, ly_scm2string (v));
	}
      else if (scm_integer_p (v) == SCM_BOOL_T)
	{
	  output_int_def (prefix + s, gh_scm2int (v));	  
	}
      else if (gh_number_p (v))
	{
	  output_Real_def (prefix + s, gh_scm2double (v));	  
	}
    }
}

void
Paper_outputter::output_version ()
{
  String id_str = "Lily was here";
  if (no_timestamps_global_b)
    id_str += ".";
  else
    id_str += String (", ") + version_str ();

  output_String_def ( "mudelatagline", id_str);
  output_String_def ( "LilyPondVersion", version_str ());
}




void
Paper_outputter::output_Real_def (String k, Real v)
{
  
  SCM scm = gh_list (ly_symbol2scm ("lily-def"),
		     ly_str02scm (k.ch_l ()),
		     ly_str02scm (to_str(v).ch_l ()),
		     SCM_UNDEFINED);
  output_scheme (scm);
}

void
Paper_outputter::output_String_def (String k, String v)
{
  
  SCM scm = gh_list (ly_symbol2scm ("lily-def"),
		     ly_str02scm (k.ch_l ()),
		     ly_str02scm (v.ch_l ()),
		     SCM_UNDEFINED);
  output_scheme (scm);
}

void
Paper_outputter::output_int_def (String k, int v)
{
  SCM scm = gh_list (ly_symbol2scm ("lily-def"),
		     ly_str02scm (k.ch_l ()),
		     ly_str02scm (to_str (v).ch_l ()),
		     SCM_UNDEFINED);
  output_scheme (scm);
}



void
Paper_outputter::output_string (SCM str)
{
  *stream_p_ <<  ly_scm2string (str);
}
