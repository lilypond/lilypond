/*
  outputter.hh -- declare Outputter

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef OUTPUTTER_HH
#define OUTPUTTER_HH

#include "lily-proto.hh"

/**
  Abstract interface for a Score_elem to output itself.
 */
struct Outputter
{
  virtual ~Outputter(){}
  virtual void start_line ()=0;
  virtual void output_molecule (Molecule const *, Offset)=0;
  virtual void stop_line ()=0;
};

/**
  Output a line of score using  TeX strings
 */
struct Tex_outputter : Outputter
{
  Tex_outputter (Tex_stream *);
  Tex_stream * outstream_l_;
  virtual void output_molecule (Molecule const*, Offset);
  virtual void start_line ();
  virtual void stop_line ();
};

#endif // OUTPUTTER_HH
