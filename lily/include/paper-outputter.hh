/*
  paper-outputter.hh -- declare Paper_outputter

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef PAPER_OUTPUTTER_HH
#define PAPER_OUTPUTTER_HH

#include "lily-proto.hh"
#include "array.hh"
#include "string.hh"
#include "lily-guile.hh"
#include "protected-scm.hh"

#ifdef __powerpc__
#include "protected-scm.hh"
#endif

/**
  Abstract interface for a Score_element to output itself.
 */
class Paper_outputter
{
public:
  Protected_scm molecules_;
  SCM last_cons_;
  Paper_outputter ();

  void dump_onto (Paper_stream *);


  void output_int_def (String k, int v);
  void output_Real_def (String k, Real v);
  void output_String_def (String k, String v);
  void output_scope (Scope*, String prefix);
  void output_version ();
  void output_font_def (int i, String str);
  void output_font_switch (int i);
  void output_header ();
  void output_molecule (Molecule const *, Offset, char const *);
  void output_comment (String s);
  void output_scheme (SCM scm);

  void start_line (Real height);
  void stop_line ();
  void stop_last_line ();
};

#endif // PAPER_OUTPUTTER_HH
