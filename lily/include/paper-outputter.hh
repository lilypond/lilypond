/*
  paper-outputter.hh -- declare Paper_outputter

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef PAPER_OUTPUTTER_HH
#define PAPER_OUTPUTTER_HH

#include "lily-proto.hh"
#include "array.hh"
#include "string.hh"
#include "lily-guile.hh"

/**
  Abstract interface for a Score_element to output itself.
 */
class Paper_outputter
{
public:
  Paper_outputter (Paper_stream *);
  ~Paper_outputter ();

  void output_font_def (int i, String str);
  void output_font_switch (int i);
  void output_header ();
  void output_molecule (Molecule const *, Offset, char const *);
  void output_comment (String s);
  void output_scheme (SCM scm);
  void output_string (String s);
  void start_line ();
  void stop_line ();
  void switch_to_font (String fontname);

  Array<String> font_arr_;
  String current_font_;
  Paper_stream* outstream_l_;
};

#endif // PAPER_OUTPUTTER_HH
