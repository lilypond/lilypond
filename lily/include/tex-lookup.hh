/*
  tex-lookup.hh -- declare Tex_lookup

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef TEX_LOOKUP_HH
#define TEX_LOOKUP_HH

#include "ps-lookup.hh"

class Tex_lookup : public Ps_lookup
{
public:
  Tex_lookup ();
  Tex_lookup (Lookup const& s);
  Tex_lookup (Symtables const& s);
  virtual ~Tex_lookup();

  virtual Atom afm_find (String s) const;
  virtual String character_str (int i) const;
  virtual Atom dashed_slur (Array<Offset> controls, Real thick, Real dash) const;
  virtual Atom* atom_p (String, int, Box) const;
  Atom embed (Atom a) const;
  virtual Atom hairpin (Real width, bool decresc, bool continued) const;
  virtual Lookup* lookup_p (Lookup const&) const;
  virtual Lookup* lookup_p (Symtables const&) const;
  virtual Paper_outputter* paper_outputter_p (Paper_stream*, Paper_def*, Scope*, String) const;
  virtual Paper_stream* paper_stream_p () const;
  virtual Atom plet (Real dy , Real dx, Direction dir) const;
  virtual String print_dimen (Real) const;
  virtual Atom ps_beam (Real slope, Real width, Real thick) const;
  virtual Atom slur (Array<Offset> controls) const;
  virtual Atom stem (Real y1, Real y2) const;
  virtual Atom text (String style, String text) const;
  virtual String unknown_str () const;
  virtual Atom vbracket (Real &y) const;
};

#endif // TEX_LOOKUP_HH
