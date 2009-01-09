/*
  paper-outputter.hh -- declare Paper_outputter

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef PAPER_OUTPUTTER_HH
#define PAPER_OUTPUTTER_HH

#include "lily-proto.hh"
#include "std-vector.hh"
#include "std-string.hh"
#include "protected-scm.hh"
#include "smobs.hh"

/*
  Glue between the backend (grobs, systems, pages) and the output file.
  proxy for Scheme backends.
*/
class Paper_outputter
{
  SCM output_module_;
  string file_name_;
  SCM file_;

public:
  DECLARE_SMOBS (Paper_outputter);

public:
  SCM file () const;
  SCM dump_string (SCM);
  void output_scheme (SCM scm);
  Paper_outputter (SCM port, string format);
  SCM scheme_to_string (SCM);
  void output_stencil (Stencil);
  void close ();
};

Paper_outputter *get_paper_outputter (string, string);
DECLARE_UNSMOB (Paper_outputter, outputter);

#endif /* PAPER_OUTPUTTER_HH */
