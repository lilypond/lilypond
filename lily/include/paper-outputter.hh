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

/**
  Abstract interface for a Grob to output itself.
 */
class Paper_outputter
{
  bool verbatim_scheme_b_;
  Paper_stream * stream_p_;
public:
  /**
     Assumes responsibility for deletion of P
   */
  Paper_outputter (Paper_stream*p);
  ~Paper_outputter ();
  
  void dump_scheme (SCM);

  void output_int_def (String k, int v);
  void output_Real_def (String k, Real v);
  void output_String_def (String k, String v);
  void output_scope (Scope*, String prefix);
  void output_version ();
  void output_font_def (int i, String str);
  void output_font_switch (int i);
  void output_header ();
  void output_comment (String s);
  void output_string (SCM s);
  void output_scheme (SCM scm);
};

#endif // PAPER_OUTPUTTER_HH
