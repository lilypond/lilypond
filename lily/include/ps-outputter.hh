/*
  ps-outputter.hh -- declare Ps_outputter

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef PS_OUTPUTTER_HH
#define PS_OUTPUTTER_HH

#include "paper-outputter.hh"

class Ps_outputter : public Paper_outputter
{
public:
  Ps_outputter (Paper_stream *);
  virtual ~Ps_outputter ();

  void switch_to_font (String);
  virtual void output_molecule (Molecule const*, Offset, char const*);
  virtual void start_line ();
  virtual void stop_line ();
};

#endif // PS_OUTPUTTER_HH
