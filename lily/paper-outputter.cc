/*
  paper-outputter.cc -- implement Paper_outputter

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <time.h>
#include <fstream.h>
#include "paper-outputter.hh"
#include "paper-stream.hh"
#include "molecule.hh"
#include "atom.hh"
#include "array.hh"
#include "string-convert.hh"
#include "debug.hh"
#include "lookup.hh"
#include "main.hh"

Paper_outputter::Paper_outputter (Paper_stream *s)
{
  outstream_l_ = s;
  output_header ();
}

Paper_outputter::~Paper_outputter ()
{
  SCM scm =
    ly_append (ly_lambda_o (),
    ly_list1 (ly_append (ly_func_o ("end-output"), SCM_EOL)));

  output_scheme (scm);
}

void
Paper_outputter::output_header ()
{
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
    gh_cons (gh_str02scm (creator.ch_l ()),
    gh_cons (gh_str02scm (generate.ch_l ()), SCM_EOL));

#ifndef NPRINT
  DOUT << "output_header\n";
  if (check_debug && !monitor->silent_b ("Guile"))
    {
      gh_display (args_scm); gh_newline ();
    }
#endif

  SCM scm =
    ly_append (ly_lambda_o (),
    ly_list1 (ly_append (ly_func_o ("header"), args_scm)));

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
        {
	  // urg
	  i->lambda_ = ly_append (ly_lambda_o (), 
	    ly_list1 (ly_func_o ("empty")));
	}

      switch_to_font (i->font_);

#ifndef NPRINT
      if (check_debug && !monitor->silent_b ("Guile"))
	{
	  DOUT << i->str_ << "\n";
	  gh_display (i->lambda_); gh_newline ();
	}
#endif

      SCM args_scm = 
	gh_cons (gh_double2scm (a_off.x ()), 
	gh_cons (gh_double2scm (a_off.y ()), 
	gh_cons (i->lambda_, SCM_EOL)));

#ifndef NPRINT
      if (check_debug && !monitor->silent_b ("Guile"))
	{
	  gh_display (args_scm); gh_newline ();
	}
#endif

      SCM box_scm =
	ly_append (ly_lambda_o (),
	ly_list1 (ly_append (ly_func_o ("placebox"), args_scm)));

      output_scheme (box_scm);
    }
}

void
Paper_outputter::output_comment (String str)
{
  // urg
  *outstream_l_ << "% " << str << "\n";
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
	  ofstream * of = dynamic_cast <ofstream*> (outstream_l_->os);
	  if (of)
	    fd = of->rdbuf()->fd();
	  FILE *file = fdopen (fd, "a");
	  port = scm_standard_stream_to_port (file, "a", "");
	  scm_display (gh_str02scm ("(load 'lily.scm)\n"), port);
	}

      scm_display (gh_str02scm ("(display ((eval "), port);
      scm_write (scm, port);
      scm_display (gh_str02scm (") 'tex))\n"), port);
      scm_newline (port);
      scm_fflush (port);

      return;
    }
  else
    {
      SCM str_scm = gh_call1 (ly_eval (scm), gh_eval_str (o.ch_l ()));
      char* c = gh_scm2newstr (str_scm, NULL);
      *outstream_l_ << c;
      free (c);
    }
}
void
Paper_outputter::output_string (String str)
{
  // urg
  *outstream_l_ << str;
}

void
Paper_outputter::switch_to_font (String fontname)
{
  if (fontname.length_i () && (fontname != current_font_))
    {
      current_font_ = fontname;
      int i=0;
      bool new_b = false;
      for (; i< font_arr_.size (); i++)
	if (font_arr_[i] == fontname)
	  {
	    new_b = true;
	    break;
	  }

      if (new_b)
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
  SCM scm =
    gh_append2 (ly_lambda_o (),
		gh_list (ly_func_o ("start-line"), SCM_UNDEFINED));;

  output_scheme (scm);
}

/*
   26 fonts ought to be enough for anyone.
*/
void
Paper_outputter::output_font_def (int i, String str)
{
  //urg, broken with guile-1.3
  //return;
  SCM scm =
    ly_append (ly_lambda_o (),
    ly_list1 (ly_append (ly_func_o ("font-def"), 
    gh_cons (gh_int2scm (i), gh_cons (gh_str02scm (str.ch_l ()), SCM_EOL)))));

  output_scheme (scm);
}

void
Paper_outputter::output_font_switch (int i)
{
  //urg, broken with guile-1.2, 1.3
  //return;
  SCM scm =
    ly_append (ly_lambda_o (),
    ly_list1 (ly_append (ly_func_o ("font-switch"), 
    gh_cons (gh_int2scm (i), SCM_EOL))));

  output_scheme (scm);
}

void
Paper_outputter::stop_line ()
{
  SCM scm =
    ly_append (ly_lambda_o (),
    ly_list1 (ly_append (ly_func_o ("stop-line"), SCM_EOL)));

  output_scheme (scm);

  current_font_ = "";
  font_arr_.clear ();
}
