/*
  paper-outputter.hh -- declare Paper_outputter

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef PAPER_OUTPUTTER_HH
#define PAPER_OUTPUTTER_HH

#include "lily-proto.hh"
#include "array.hh"
#include "string.hh"
#include "lily-guile.hh"
#include "protected-scm.hh"

/**
   Interface for a Grob to output itself; The Paper_score contains a
   pointer to a Paper_outputter, and this enables every grob to output
   itself.

   The Paper_outputter contains a reference to an output stream
 (Paper_stream).  */

class Paper_outputter
{
  bool verbatim_scheme_b_;
  SCM output_module_;
  Protected_scm file_;
  String filename_;
  
  void output_expr (SCM expr, Offset o);
  void output_metadata (Book_paper_def*, SCM);
  void output_music_output_def (Music_output_def* odef);

public:
  Paper_outputter (String nm);
  ~Paper_outputter ();

  void dump_scheme (SCM);
  void output_scheme (SCM scm);
  void output_stencil (Stencil);
  void output_header (Book_paper_def*, SCM, int, bool);
  void output_line (SCM, Offset*, bool);
  void output_page (Page*, bool);
};

#endif /* PAPER_OUTPUTTER_HH */
