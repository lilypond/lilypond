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
  virtual Atom Ps_lookup::dashed_slur (Array<Offset> controls, Real thick, Real dash) const;
  virtual Atom Ps_lookup::hairpin (Real width, bool decresc, bool continued) const;
  virtual Atom Ps_lookup::plet (Real dy , Real dx, Direction dir) const;
  virtual Atom Ps_lookup::ps_beam (Real slope, Real width, Real thick) const;
  virtual Atom Ps_lookup::slur (Array<Offset> controls) const;
  virtual Atom stem (Real y1, Real y2) const;
  virtual Atom Ps_lookup::vbracket (Real &y) const;
};

#endif // PS_LOOKUP_HH
