/*
  paper-outputter.hh -- declare Paper_outputter

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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

  
public:

  SCM output_func_ ;
  Protected_scm file_;
  
  String basename_;
  Paper_outputter (String nm);
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

  void write_header_field_to_file (String filename, SCM, SCM);
  void write_header_fields_to_file (Scope *);
};

#endif // PAPER_OUTPUTTER_HH
