/*
  paper-outputter.cc -- implement Paper_outputter

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <time.h>
#include <fstream.h>

#include "dimensions.hh"
#include "dictionary-iter.hh"
#include "virtual-methods.hh"
#include "paper-outputter.hh"
#include "paper-stream.hh"
#include "molecule.hh"
#include "array.hh"
#include "string-convert.hh"
#include "debug.hh"
#include "lookup.hh"
#include "main.hh"
#include "scope.hh"
#include "identifier.hh"
#include "lily-version.hh"
#include "atom.hh"

Paper_outputter::Paper_outputter (Paper_stream *s)
{
  outstream_l_ = s;
  output_header ();

  if (output_global_ch == String ("scm"))
    *outstream_l_->os << ""
      "(primitive-load-path 'lily.scm)\n"
      "(eval (tex-scm 'all-definitions))\n"
      ";(eval (ps-scm 'all-definitions))\n"
      "(display (map (lambda (x) (string-append (eval x) \"\\n\")) '(\n"
    ;
}

Paper_outputter::~Paper_outputter ()
{
  SCM scm = gh_list (ly_symbol ("end-output"), SCM_UNDEFINED);
  output_scheme (scm);

  if (String (output_global_ch) == "scm")
    {
      *outstream_l_->os << ")))";
    }
}

void
Paper_outputter::output_header ()
{
  if (safe_global_b)
    {
      ly_set_scm ("security-paranoia", SCM_BOOL_T);
      //      gh_eval_str ("(set! security-paranoia #t)");
    }
  String s = String ("(eval (") + output_global_ch + "-scm 'all-definitions))";
  gh_eval_str (s.ch_C ());
  
  String creator;
  if (no_timestamps_global_b)
    creator = gnu_lilypond_str ();
  else
    creator = gnu_lilypond_version_str ();
  
  String generate;
  if (no_timestamps_global_b)
    generate = ".\n";
  else
    {
      generate = _ (", at ");
      time_t t (time (0));
      generate += ctime (&t);
      //urg
    }

  SCM args_scm = 
    gh_list (gh_str02scm (creator.ch_l ()),
	     gh_str02scm (generate.ch_l ()), SCM_UNDEFINED);

#ifndef NPRINT
  DOUT << "output_header\n";
  if (check_debug && !monitor->silent_b ("Guile"))
    {
      gh_display (args_scm); gh_newline ();
    }
#endif

  SCM scm = gh_cons (header_scm_sym, args_scm);
  output_scheme (scm);
}

void
Paper_outputter::output_molecule (Molecule const*m, Offset o, char const *nm)
{
  if (check_debug)
    *outstream_l_ << String ("\n%start: ") << nm << "\n";


  if (check_debug)
    {
      output_comment (nm);
    }
      
#ifdef ATOM_SMOB
  for (SCM ptr = m->atom_list_; ptr != SCM_EOL; ptr = SCM_CDR(ptr))
    {
      Atom *i = Atom::atom_l (SCM_CAR(ptr));
#else
  for (Cons<Atom> *ptr = m->atom_list_; ptr; ptr = ptr->next_)
    {
      Atom * i = ptr->car_;
#endif
#if 0
    }
#endif      
      Offset a_off = i->off_;
      a_off += o;

      if (!i->func_)
	continue; 

      if (a_off.length () > 100 CM)
	{
	  warning (_f("Improbable offset for object type `%s\'", nm));
	  Axis a  =X_AXIS;
	  while (a < NO_AXES)
	    {
	      if (abs(a_off[a]) > 50 CM)
		a_off[a] = 50 CM;
	      incr (a);
	    }
	}
	
      if (i->font_)
	{
	  output_scheme (gh_list (ly_symbol ("select-font"),
				  gh_str02scm (symbol_to_string (i->font_).ch_C()),
				  SCM_UNDEFINED));
	}

      SCM box_scm
	= gh_list (placebox_scm_sym,
		   gh_double2scm (a_off.x ()),
		   gh_double2scm (a_off.y ()),
		   SCM(i->func_),
		   SCM_UNDEFINED);
      
      output_scheme (box_scm);
    }
}

void
Paper_outputter::output_comment (String str)
{
  if (String (output_global_ch) == "scm")
    {
      *outstream_l_ << "; " << str << '\n';
    }
  else
    {
      *outstream_l_ << "% " << str << "\n";
    }
}


void
Paper_outputter::output_scheme (SCM scm)
{
  if (String (output_global_ch) == "scm")
    {
      SCM result =  scm_eval (scm_listify (ly_symbol ("scm->string"), ly_quote_scm (scm), SCM_UNDEFINED));
    *outstream_l_->os << ly_scm2string (result)	<< endl;
    }
  else
    {
      SCM result = scm_eval (scm);
      char *c=gh_scm2newstr (result, NULL);

      *outstream_l_ << c;
      free (c);
    }
}

void
Paper_outputter::output_scope (Scope *scope, String prefix)
{
  for (Scope_iter i (*scope); i.ok (); i++)
    {
      if (dynamic_cast<String_identifier*> (i.val ()))
	{
	  String val = *i.val()->access_content_String (false);

	  output_String_def (prefix + i.key (), val);
	}
      else if(dynamic_cast<Real_identifier*> (i.val ()))
	{
	  Real val  = *i.val ()->access_content_Real (false);

	  output_Real_def (prefix + i.key (), val);	  
	}
      else if (dynamic_cast<int_identifier*> (i.val ()))
	{
	  int val  = *i.val ()->access_content_int (false);	  
	  
	  output_int_def (prefix + i.key (), val);	  
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
Paper_outputter::start_line (Real height)
{
  SCM scm = gh_list (ly_symbol ("start-line"),
		     gh_double2scm (height),
		     SCM_UNDEFINED);
  output_scheme (scm);
}

void
Paper_outputter::output_font_def (int i, String str)
{
  SCM scm = gh_list (ly_symbol ("font-def"),
		     gh_int2scm (i),
		     gh_str02scm (str.ch_l ()),
		     SCM_UNDEFINED);

  output_scheme (scm);
}

void
Paper_outputter::output_Real_def (String k, Real v)
{
  
  SCM scm = gh_list (ly_symbol ("lily-def"),
		     gh_str02scm (k.ch_l ()),
		     gh_str02scm (to_str(v).ch_l ()),
		     SCM_UNDEFINED);
  output_scheme (scm);

  gh_define (k.ch_l (), gh_double2scm (v));
}

void
Paper_outputter::output_String_def (String k, String v)
{
  
  SCM scm = gh_list (ly_symbol ("lily-def"),
		     gh_str02scm (k.ch_l ()),
		     gh_str02scm (v.ch_l ()),
		     SCM_UNDEFINED);
  output_scheme (scm);

  gh_define (k.ch_l (), gh_str02scm (v.ch_l ()));
}

void
Paper_outputter::output_int_def (String k, int v)
{
  SCM scm = gh_list (ly_symbol ("lily-def"),
		     gh_str02scm (k.ch_l ()),
		     gh_str02scm (to_str (v).ch_l ()),
		     SCM_UNDEFINED);
  output_scheme (scm);

  gh_define (k.ch_l (), gh_int2scm (v));
}



void
Paper_outputter::stop_line ()
{
  SCM scm = gh_list (ly_symbol ("stop-line"), SCM_UNDEFINED);
  output_scheme (scm);
}

void
Paper_outputter::stop_last_line ()
{
  SCM scm = gh_list (ly_symbol ("stop-last-line"), SCM_UNDEFINED);
  output_scheme (scm);
}
