/*
  ps-score.cc -- implement Ps_score

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "debug.hh"
#include "file-results.hh"
#include "header.hh"
#include "main.hh"
#include "paper-def.hh"
#include "ps-stream.hh"
#include "ps-outputter.hh"
#include "ps-score.hh"

Paper_outputter*
Ps_score::paper_outputter_p (Paper_stream* os_p) const
{
  if (header_global_p)
    *os_p << header_global_p->ps_string ();
  
  *os_p << _ ("\n% outputting Score, defined at: ") << origin_str_ << '\n';

  if (header_l_)
    *os_p << header_l_->ps_string ();
  *os_p << paper_l_->output_settings_str ();

  if (experimental_features_global_b)
    *os_p << "turnOnExperimentalFeatures\n";

  return new Ps_outputter (os_p);
}

Paper_stream*
Ps_score::paper_stream_p () const
{
  String outname = base_output_str ();

  if (outname != "-")
    outname += ".ps";
  *mlog << _f ("PostScript output to %s...", 
	       outname == "-" ? String ("<stdout>") : outname ) << endl;
  target_str_global_array.push (outname);
  return new Ps_stream (outname);
}

