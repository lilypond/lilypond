/*
  tex-score.cc -- implement Tex_score

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "debug.hh"
#include "file-results.hh"
#include "header.hh"
#include "main.hh"
#include "paper-def.hh"
#include "tex-stream.hh"
#include "tex-outputter.hh"
#include "tex-score.hh"

Paper_outputter*
Tex_score::paper_outputter_p (Paper_stream* os_p) const
{
  if (header_global_p)
    *os_p << header_global_p->tex_string ();
  
  *os_p << _ ("\n% outputting Score, defined at: ") << origin_str_ << '\n';

  if (header_l_)
    *os_p << header_l_->tex_string();
  *os_p << paper_l_->output_settings_str ();
  
  if (experimental_features_global_b)
    *os_p << "\\turnOnExperimentalFeatures%\n";

  *os_p << "\\turnOnPostScript%\n";

  return new Tex_outputter (os_p);
}

Paper_stream *
Tex_score::paper_stream_p () const
{
  String outname = base_output_str ();

  Paper_stream* p;
  if (outname != "-")
    outname += ".tex";
  *mlog << _f ("TeX output to %s...", 
	       outname == "-" ? String ("<stdout>") : outname ) << endl;
  p = new Tex_stream (outname);
  target_str_global_array.push (outname);
  return p;
}

