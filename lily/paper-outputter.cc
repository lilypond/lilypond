/*
  paper-outputter.cc -- implement Paper_outputter

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <time.h>

#include <math.h>


#include "dimensions.hh"
#include "virtual-methods.hh"
#include "paper-outputter.hh"
#include "molecule.hh"
#include "array.hh"
#include "string-convert.hh"
#include "debug.hh"
#include "font-metric.hh"
#include "main.hh"
#include "scope.hh"

#include "lily-version.hh"
#include "paper-def.hh"
#include "file-results.hh"


/*
  Ugh, this is messy.
 */
Paper_outputter::Paper_outputter (String name)
{
  if (safe_global_b)
    {
      gh_define ("security-paranoia", SCM_BOOL_T);      
    }
  
  file_ = scm_open_file (ly_str02scm (name.ch_C()),
			    ly_str02scm ("w"));
  
  SCM exp = scm_list_n (ly_symbol2scm ("find-dumper"),
			ly_str02scm (output_format_global.ch_C()),
			SCM_UNDEFINED);

  output_func_  = scm_primitive_eval (exp);
}

Paper_outputter::~Paper_outputter ()
{
  
}


void
Paper_outputter::output_header ()
{
  String       generate = _ (", at ");
  time_t t (time (0));
  generate += ctime (&t);
  generate = generate.left_str (generate.length_i () - 1);
  
  /*
    Make fixed length time stamps
   */
  generate = generate + to_str (' ' * (120 - generate.length_i ())>? 0)  ;
  String creator = "lelie";
  
  SCM args_scm = scm_list_n (ly_str02scm (creator.ch_C ()),
			     ly_str02scm (generate.ch_C ()), SCM_UNDEFINED);


  SCM scm = gh_cons (ly_symbol2scm ("header"), args_scm);

  output_scheme (scm);
}



void
Paper_outputter::output_comment (String str)
{
  output_scheme (scm_list_n (ly_symbol2scm ("comment"),
			  ly_str02scm ((char*)str.ch_C ()),
			  SCM_UNDEFINED)
		 );
}

void
Paper_outputter::output_scheme (SCM scm)
{
  gh_call2 (output_func_, scm, file_);
}

void
Paper_outputter::output_scope (Scope *scope, String prefix)
{
  SCM al = scope->to_alist ();
  for (SCM s = al ; gh_pair_p (s); s = ly_cdr (s))
    {
      SCM k = ly_caar (s);
      SCM v = ly_cdar (s);
      String s = ly_symbol2string (k);
      
      if (gh_string_p (v))
	{
	  output_String_def (prefix + s, ly_scm2string (v));
	}
      else if (scm_exact_p (v) == SCM_BOOL_T)
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
  id_str += String_convert::pad_to (String (", ") + version_str (), 40);

  output_String_def ("lilypondtagline", id_str);
  output_String_def ("LilyPondVersion", version_str ());
}




void
Paper_outputter::output_Real_def (String k, Real v)
{
  
  SCM scm = scm_list_n (ly_symbol2scm ("lily-def"),
		     ly_str02scm (k.ch_l ()),
		     ly_str02scm (to_str (v).ch_l ()),
		     SCM_UNDEFINED);
  output_scheme (scm);
}

void
Paper_outputter::output_String_def (String k, String v)
{
  
  SCM scm = scm_list_n (ly_symbol2scm ("lily-def"),
		     ly_str02scm (k.ch_l ()),
		     ly_str02scm (v.ch_l ()),
		     SCM_UNDEFINED);
  output_scheme (scm);
}

void
Paper_outputter::output_int_def (String k, int v)
{
  SCM scm = scm_list_n (ly_symbol2scm ("lily-def"),
		     ly_str02scm (k.ch_l ()),
		     ly_str02scm (to_str (v).ch_l ()),
		     SCM_UNDEFINED);
  output_scheme (scm);
}

void
Paper_outputter::write_header_field_to_file (String filename, SCM key, SCM value)
{
  output_scheme (scm_list_n (ly_symbol2scm ("header-to-file"),
			     ly_str02scm (filename.ch_C()),
			     ly_quote_scm (key), value,
			     SCM_UNDEFINED));
}

void
Paper_outputter::write_header_fields_to_file (Scope * header)
{
  if (dump_header_fieldnames_global.size ())
    {
      SCM fields = header->to_alist ();
      for (int i = 0; i < dump_header_fieldnames_global.size (); i++)
	{
	  String key = dump_header_fieldnames_global[i];
	  SCM val = gh_assoc (ly_symbol2scm (key.ch_C ()), fields);
	  String s;
	  /* Only write header field to file if it exists */
	  if (gh_pair_p (val))
	    {
	      s = ly_scm2string (ly_cdr (val));
	      /* Always write header field file, even if string is empty ... */
	      write_header_field_to_file (basename_ , ly_car (val), ly_cdr (val));
	    }
	}
    }
}
