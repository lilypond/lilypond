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

/**
  Abstract interface for a Score_element to output itself.
 */
class Paper_outputter
{
public:
  Paper_outputter (Paper_stream *);
  virtual ~Paper_outputter ();

  virtual void output_molecule (Molecule const *, Offset, char const *)=0;
  void output_molecule (Molecule const *, Offset, char const *, String);
  virtual void start_line ()=0;
  virtual void stop_line ()=0;
  virtual void switch_to_font (String fontname)=0;

  Array<String> font_arr_;
  String current_font_;
  Paper_stream* outstream_l_;
};

#endif // PAPER_OUTPUTTER_HH
