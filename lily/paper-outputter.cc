/*
  paper-outputter.cc -- implement Paper_outputter

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <time.h>
#include <fstream.h>

#include "dictionary-iter.hh"
#include "virtual-methods.hh"
#include "paper-outputter.hh"
#include "paper-stream.hh"
#include "molecule.hh"
#include "atom.hh"
#include "array.hh"
#include "string-convert.hh"
#include "debug.hh"
#include "lookup.hh"
#include "main.hh"
#include "scope.hh"
#include "identifier.hh"

Paper_outputter::Paper_outputter (Paper_stream *s)
{
  outstream_l_ = s;
  output_header ();
}

Paper_outputter::~Paper_outputter ()
{
  SCM scm = gh_list (ly_symbol ("end-output"), SCM_UNDEFINED);
  output_scheme (scm);
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
  gh_eval_str (s.ch_C());
  
  String creator;
  if (no_timestamps_global_b)
    creator = "GNU LilyPond\n";
  else
    creator = get_version_str ();
  String generate;
  if (no_timestamps_global_b)
    generate = ".";
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

  SCM scm = gh_cons (ly_symbol ("header"), args_scm);
  output_scheme (scm);
}

void
Paper_outputter::output_molecule (Molecule const*m, Offset o, char const *nm)
{
  if (check_debug)
    *outstream_l_ << String ("\n%start: ") << nm << "\n";

  for (PCursor <Atom*> i (m->atoms_); i.ok (); i++)
    {
      Offset a_off = i->offset ();
      a_off += o;

      if (!i->lambda_)
	continue; 

      if (check_debug)
	{
	  output_comment (classname (i.ptr ()->origin_l_));

	}
      
      switch_to_font (i->font_);

      SCM args_scm = gh_list (gh_double2scm (a_off.x ()),
		 gh_double2scm (a_off.y ()), 
		 i->lambda_.to_SCM (),
		 SCM_UNDEFINED);


      SCM box_scm = gh_cons (ly_symbol ("placebox"), args_scm);

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
  String o = String ("\'") + output_global_ch;

  if (String (output_global_ch) == "scm")
    {
      static SCM port = 0;

      // urg
      if (!port)
        {
	  int fd = 1;
	  ofstream * of = dynamic_cast<ofstream*> (outstream_l_->os);
	  if (of)
	    fd = of->rdbuf()->fd();
	  FILE *file = fdopen (fd, "a");
	  port = scm_standard_stream_to_port (file, "a", "");
	  scm_display (gh_str02scm ("(load 'lily.scm)\n"), port);
	}

      scm_display (gh_str02scm ("("), port);
      scm_write (scm, port);
      scm_display (gh_str02scm (")\n"),port);
      scm_fflush (port);
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
  for (Dictionary_iter<Identifier*> i (*scope); i.ok (); i++)
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
    id_str += String (", ") + get_version_str ();
  output_String_def ( "LilyIdString", id_str);
}

void
Paper_outputter::switch_to_font (String fontname)
{
  if (fontname.length_i () && fontname != current_font_)
    {
      current_font_ = fontname;
      int i=0;

      for (; i< font_arr_.size (); i++)
	if (font_arr_[i] == fontname)
	    break;

      if (i == font_arr_.size ())
	{
	  font_arr_.push (fontname);
	  output_font_def (i, fontname);
	}
      output_font_switch (i);
    }
  return;
}

void
Paper_outputter::start_line ()
{
  SCM scm = gh_list (ly_symbol ("start-line"), SCM_UNDEFINED);
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
Paper_outputter::output_font_switch (int i)
{
  SCM scm = gh_list (ly_symbol ("font-switch"),
		     gh_int2scm (i),
		     SCM_UNDEFINED);

  output_scheme (scm);
}

void
Paper_outputter::stop_line ()
{
  SCM scm =    gh_list (ly_symbol ("stop-line"), SCM_UNDEFINED);
  output_scheme (scm);

  current_font_ = "";
  font_arr_.clear ();
}
