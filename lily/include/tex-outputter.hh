/*
  tex-outputter.hh -- declare Tex_outputter

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef TEX_OUTPUTTER_HH
#define TEX_OUTPUTTER_HH

#include "paper-outputter.hh"

class Tex_outputter : public Paper_outputter
{
public:
  Tex_outputter (Paper_stream *);
  virtual ~Tex_outputter ();

  void switch_to_font (String);
  virtual void output_molecule (Molecule const*, Offset, char const*);
  virtual void start_line ();
  virtual void stop_line ();
};

#endif // TEX_OUTPUTTER_HH
