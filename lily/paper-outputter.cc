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

Paper_outputter::Paper_outputter ()
{
  molecules_ = gh_cons (SCM_EOL, SCM_EOL);
  last_cons_ = molecules_;
}


void
Paper_outputter::output_header ()
{
  if (safe_global_b)
    {
      //      ly_set_x ("security-paranoia", SCM_BOOL_T);
      gh_define ("security-paranoia", SCM_BOOL_T);      
    }
  String s = String ("(eval (") + output_global_ch + "-scm 'all-definitions))";
  ly_eval_str (s.ch_C ());
  
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
    gh_list (ly_str02scm (creator.ch_l ()),
	     ly_str02scm (generate.ch_l ()), SCM_UNDEFINED);


  SCM scm = gh_cons (ly_symbol2scm ("header"), args_scm);
  output_scheme (scm);
}

void
Paper_outputter::output_molecule (Molecule const*m, Offset o, char const *nm)
{
  if (flower_dstream)
    {
      output_comment (nm);
    }
      
  for (Cons<Atom> *ptr = m->atom_list_; ptr; ptr = ptr->next_)
    {
      Atom * i = ptr->car_;

      Offset a_off = i->off_;
      a_off += o;

      if (!i->func_)
	continue; 

      if (a_off.length () > 100 CM)
	{
	  programming_error ("improbable offset for object");
	  Axis a  =X_AXIS;
	  while (a < NO_AXES)
	    {
	      if (abs(a_off[a]) > 30 CM)
		a_off[a] = 30 CM;
	      incr (a);
	    }
	}
	
      SCM box_scm
	= gh_list (ly_symbol2scm ("placebox"),
		   gh_double2scm (a_off[X_AXIS]),
		   gh_double2scm (a_off[Y_AXIS]),
		   SCM(i->func_),
		   SCM_UNDEFINED);

      output_scheme (box_scm);
    }
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
  SCM c = gh_cons (scm,gh_cdr (last_cons_));
  gh_set_cdr_x(last_cons_, c);
  last_cons_ = c;
}


void
Paper_outputter::dump_onto (Paper_stream *ps)
{
  if (String (output_global_ch) == "scm")
#if 1  // both are fine
    {
      /*
        default to stdin
       */
      int fd = 1;
      if (ofstream* of = dynamic_cast<ofstream*> (ps->os))
	fd = of->rdbuf ()->fd ();
      SCM port = scm_fdes_to_port (fd, "a", SCM_EOL);

      /*
	 lilypond -f scm x.ly
	 guile -s x.scm
       */
      scm_display (gh_str02scm (
	";;; Usage: guile -s x.scm > x.tex\n"
	"(primitive-load-path 'lily.scm)\n"
	"(scm-tex-output)\n"
	";(scm-ps-output)\n"
	"(map (lambda (x) (display (eval x))) '(\n"
	), port);

      SCM newline = gh_str02scm ("\n");
      for (SCM s = gh_cdr (molecules_); gh_pair_p (s); s = gh_cdr (s))
        {
	  scm_write (gh_car (s), port);
	  scm_display (newline, port);
	  scm_flush (port);
	}
      scm_display (gh_str02scm (")))"), port);
      scm_display (newline, port);
      scm_flush (port);
      scm_close_port (port);
    }
#else
    {
      /*
	 lilypond -f scm x.ly
	 guile -s x.scm
       */
      if (output_global_ch == String ("scm"))
	*ps << ""
	  ";;; Usage: guile -s x.scm > x.tex\n"
	  "(primitive-load-path 'lily.scm)\n"
	  "(scm-tex-output)\n"
	  ";(scm-ps-output)\n"
	  "(map (lambda (x) (display (eval x))) '(\n"
	;
      for (SCM s = gh_cdr (molecules_); gh_pair_p (s); s = gh_cdr (s))
	{
	  SCM result =  scm_eval (scm_listify (ly_symbol2scm ("scm->string"),
					       ly_quote_scm (gh_car (s)), SCM_UNDEFINED));
	  
	  *ps << ly_scm2string (result);
	}
      *ps << ")))";
    }
#endif
  
  else
    {
      for (SCM s = gh_cdr (molecules_); gh_pair_p (s); s = gh_cdr (s))
	{
	  SCM result = scm_eval (gh_car (s));
	  char *c=gh_scm2newstr (result, NULL);
	  
	  *ps << c;
	  free (c);
	}
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
  if (height > 50 CM)
    {
      programming_error ("Improbable system height");
      height = 50 CM;
    }
  SCM scm = gh_list (ly_symbol2scm ("start-line"),
		     gh_double2scm (height),
		     SCM_UNDEFINED);
  output_scheme (scm);
}

void
Paper_outputter::output_font_def (int i, String str)
{
  SCM scm = gh_list (ly_symbol2scm ("font-def"),
		     gh_int2scm (i),
		     ly_str02scm (str.ch_l ()),
		     SCM_UNDEFINED);

  output_scheme (scm);
}

void
Paper_outputter::output_Real_def (String k, Real v)
{
  
  SCM scm = gh_list (ly_symbol2scm ("lily-def"),
		     ly_str02scm (k.ch_l ()),
		     ly_str02scm (to_str(v).ch_l ()),
		     SCM_UNDEFINED);
  output_scheme (scm);

  gh_define (k.ch_l (), gh_double2scm (v));
}

void
Paper_outputter::output_String_def (String k, String v)
{
  
  SCM scm = gh_list (ly_symbol2scm ("lily-def"),
		     ly_str02scm (k.ch_l ()),
		     ly_str02scm (v.ch_l ()),
		     SCM_UNDEFINED);
  output_scheme (scm);

  gh_define (k.ch_l (), ly_str02scm (v.ch_l ()));
}

void
Paper_outputter::output_int_def (String k, int v)
{
  SCM scm = gh_list (ly_symbol2scm ("lily-def"),
		     ly_str02scm (k.ch_l ()),
		     ly_str02scm (to_str (v).ch_l ()),
		     SCM_UNDEFINED);
  output_scheme (scm);

  gh_define (k.ch_l (), gh_int2scm (v));
}



void
Paper_outputter::stop_line ()
{
  SCM scm = gh_list (ly_symbol2scm ("stop-line"), SCM_UNDEFINED);
  output_scheme (scm);
}

void
Paper_outputter::stop_last_line ()
{
  SCM scm = gh_list (ly_symbol2scm ("stop-last-line"), SCM_UNDEFINED);
  output_scheme (scm);
  scm = gh_list (ly_symbol2scm ("end-output"), SCM_UNDEFINED);
  output_scheme (scm);
}


