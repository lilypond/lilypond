/*
  paper-outputter.cc -- implement Paper_outputter

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
                 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <math.h>
#include <time.h>


#include "array.hh"
#include "dimensions.hh"
#include "font-metric.hh"
#include "input-smob.hh"
#include "lily-guile.hh"
#include "lily-version.hh"
#include "ly-module.hh"
#include "main.hh"
#include "page.hh"
#include "paper-book.hh"
#include "output-def.hh"
#include "paper-line.hh"
#include "paper-outputter.hh"
#include "file-name.hh"
#include "scm-hash.hh"
#include "stencil.hh"
#include "string-convert.hh"
#include "warn.hh"

// JUNKME
extern SCM stencil2line (Stencil* stil, bool is_title = false);

Paper_outputter::Paper_outputter (String filename)
{
  filename_ = filename;
  file_ = scm_open_file (scm_makfrom0str (filename.to_str0 ()),
			 scm_makfrom0str ("w"));

  String module_name = "scm output-" + output_format_global;
  output_module_ = scm_c_resolve_module (module_name.to_str0 ());
}

Paper_outputter::~Paper_outputter ()
{
  scm_close_port (file_);
  file_ = SCM_EOL;
}

void
Paper_outputter::output_scheme (SCM scm)
{
  scm_display (scm_eval (scm, output_module_), file_);
}

void
Paper_outputter::output_metadata (Output_def *paper, SCM scopes)
{
  SCM fields = SCM_EOL;
  for (int i = dump_header_fieldnames_global.size (); i--; )
    fields
      = scm_cons (ly_symbol2scm (dump_header_fieldnames_global[i].to_str0 ()),
		 fields);

  File_name file_name (filename_);
  file_name.ext_ = "";
  String basename = file_name.to_string ();
  output_scheme (scm_list_n (ly_symbol2scm ("output-scopes"),
			     paper->self_scm (),
			     ly_quote_scm (scopes),
			     ly_quote_scm (fields),
			     scm_makfrom0str (basename.to_str0 ()), 
			     SCM_UNDEFINED));
}

void
Paper_outputter::output_header (Output_def * bookpaper,
				SCM scopes,
				int page_count,
				bool is_classic)
{
  String creator = gnu_lilypond_version_string ();
  creator += " (http://lilypond.org)";
  time_t t (time (0));
  String time_stamp = ctime (&t);
  time_stamp = time_stamp.left_string (time_stamp.length () - 1)
    + " " + *tzname;
  output_scheme (scm_list_n (ly_symbol2scm ("header"),
			     scm_makfrom0str (creator.to_str0 ()),
			     scm_makfrom0str (time_stamp.to_str0 ()),
			     bookpaper->self_scm (), // FIXME.
			     scm_int2num (page_count),
			     ly_bool2scm (is_classic),
			     SCM_UNDEFINED));

  output_metadata (bookpaper, scopes);
  output_scheme (scm_list_2 (ly_symbol2scm ("define-fonts"),
			     bookpaper->self_scm ()));
  output_scheme (scm_list_1 (ly_symbol2scm ("header-end")));
}

void
Paper_outputter::output_line (SCM line, Offset *origin, bool is_last)
{
  Paper_line *p = unsmob_paper_line (line);
  Offset dim = p->dim ();
  if (dim[Y_AXIS] > 50 CM)
    {
      programming_error (to_string ("Improbable line height: %f",
				    dim[Y_AXIS]));
      dim[Y_AXIS] = 50 CM;
    }

  
  output_scheme (scm_list_3 (ly_symbol2scm ("start-system"),
			     ly_quote_scm (ly_offset2scm (*origin)),
			     ly_quote_scm (ly_offset2scm (dim))));

  
  output_stencil (p->to_stencil ());

  (*origin)[Y_AXIS] += dim[Y_AXIS];
  output_scheme (scm_list_2 (ly_symbol2scm ("stop-system"),
			     ly_bool2scm (is_last)));
}

void
Paper_outputter::output_page (Page *p, bool is_last)
{
  Stencil page_stencil =  p->to_stencil ();
  output_scheme (scm_list_1 (ly_symbol2scm ("start-page")));
  output_scheme (scm_list_3 (ly_symbol2scm ("start-system"),
			     ly_quote_scm (ly_offset2scm (Offset (0, 0))),
			     ly_quote_scm (ly_offset2scm (Offset (0, 0)))));

  
  output_stencil (page_stencil);

  output_scheme (scm_list_2 (ly_symbol2scm ("stop-system"), SCM_BOOL_T));
  output_scheme (scm_list_2 (ly_symbol2scm ("stop-page"),
			     ly_bool2scm (is_last
					  && !unsmob_stencil (p->footer_))));
}

void
Paper_outputter::output_music_output_def (Output_def *odef)
{
  output_scheme (scm_list_2 (ly_symbol2scm ("output-paper-def"),
			      odef->self_scm ()));
}


void
paper_outputter_dump (void * po, SCM x)
{
  Paper_outputter * me = (Paper_outputter*) po;
  me->output_scheme (x);
}


void
Paper_outputter::output_stencil (Stencil stil)
{
   interpret_stencil_expression (stil.expr (), paper_outputter_dump,
			  (void*) this, Offset (0,0));
}

Paper_outputter*
get_paper_outputter (String outname) 
{
  progress_indication (_f ("paper output to `%s'...",
			   outname == "-" ? String ("<stdout>") : outname));
  return new Paper_outputter (outname);

}
