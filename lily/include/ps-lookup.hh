/*
  tex-lookup.hh -- declare Tex_lookup

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef PS_LOOKUP_HH
#define PS_LOOKUP_HH

#include "lookup.hh"

class Ps_lookup : public Lookup
{
public:
  Ps_lookup ();
  Ps_lookup (Lookup const& s);
  Ps_lookup (Symtables const& s);
  virtual ~Ps_lookup ();

  virtual Atom afm_find (String s) const;
  virtual Atom* atom_p (String, int, Box) const;
  virtual String character_str (int i) const;
  virtual Atom dashed_slur (Array<Offset> controls, Real thick, Real dash) const;
  virtual Atom hairpin (Real width, bool decresc, bool continued) const;
  virtual Lookup* lookup_p (Lookup const&) const;
  virtual Lookup* lookup_p (Symtables const&) const;
  virtual Paper_outputter* paper_outputter_p (Paper_stream*, Paper_def*, Scope*, String) const;
  virtual Paper_stream* paper_stream_p () const;
  virtual Atom plet (Real dy , Real dx, Direction dir) const;
  virtual Atom ps_beam (Real slope, Real width, Real thick) const;
  virtual Atom slur (Array<Offset> controls) const;
  virtual Atom stem (Real y1, Real y2) const;
  virtual Atom text (String style, String text) const;
  virtual String unknown_str () const;
  virtual Atom vbracket (Real &y) const;
};

#endif // PS_LOOKUP_HH
