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

/*
  Glue between the backend (grobs, systems, pages) and the output file.
  proxy for Scheme backends.
*/
class Paper_outputter
{
  SCM output_module_;
  String file_name_;
  SCM file_;

  SCM file ();

public:
  DECLARE_SMOBS (Paper_outputter,);

public:
  SCM dump_string (SCM);
  void output_scheme (SCM scm);
  Paper_outputter (String nm, String format);
  SCM scheme_to_string (SCM);
  void output_stencil (Stencil);
  void close();
};

Paper_outputter *get_paper_outputter (String, String);
DECLARE_UNSMOB (Paper_outputter, outputter);

#endif /* PAPER_OUTPUTTER_HH */
