/*
  paper-outputter.cc -- implement Paper_outputter

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <time.h>
#include <math.h>

#include "dimensions.hh"
#include "virtual-methods.hh"
#include "paper-outputter.hh"
#include "stencil.hh"
#include "array.hh"
#include "string-convert.hh"
#include "warn.hh"
#include "font-metric.hh"
#include "main.hh"
#include "scm-hash.hh"
#include "lily-version.hh"
#include "paper-def.hh"
#include "input-file-results.hh"
#include "ly-modules.hh"


Paper_outputter::Paper_outputter (String name)
{
  if (safe_global_b)
    {
      gh_define ("security-paranoia", SCM_BOOL_T);      
    }
  
  file_ = scm_open_file (scm_makfrom0str (name.to_str0 ()),
			    scm_makfrom0str ("w"));

  static SCM find_dumper;
  if (!find_dumper)
    find_dumper = scm_c_eval_string ("find-dumper");

  
  output_func_ = scm_call_1 (find_dumper,scm_makfrom0str (output_format_global.to_str0 ()));
  output_scheme (gh_cons (ly_symbol2scm ("top-of-file"), SCM_EOL));
}

Paper_outputter::~Paper_outputter ()
{
  scm_close_port (file_);
  file_ = SCM_EOL;
}

void
Paper_outputter::output_scheme (SCM scm)
{
  gh_call2 (output_func_, scm, file_);
}

void
Paper_outputter::output_metadata (SCM scopes, Paper_def *paper)
{
  SCM fields = SCM_EOL;
  for (int i = dump_header_fieldnames_global.size (); i--; )
    fields = gh_cons (ly_symbol2scm (dump_header_fieldnames_global[i].to_str0 ()),
				     fields);
  
  output_scheme (scm_list_n (ly_symbol2scm ("output-scopes"),
			     paper->self_scm (),
			     scm_list_n (ly_symbol2scm ("quote"),
					 scopes, SCM_UNDEFINED),
			     scm_list_n (ly_symbol2scm ("quote"),
					 fields, SCM_UNDEFINED),
			     scm_makfrom0str (basename_.to_str0 ()), 
			     SCM_UNDEFINED));
}


void
Paper_outputter::output_music_output_def (Music_output_def* odef)
{
  output_scheme (scm_list_n (ly_symbol2scm ("output-paper-def"),
			     odef->self_scm (), SCM_UNDEFINED));
}
