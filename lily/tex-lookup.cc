/*
  tex-lookup.cc -- implement Tex_lookup

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "tex-lookup.hh"
#include "debug.hh"
#include "symtable.hh"
#include "dimension.hh"
#include "tex.hh"
#include "scalar.hh"
#include "paper-def.hh"
#include "string-convert.hh"
#include "main.hh"

Tex_lookup::Tex_lookup ()
  : Ps_lookup ()
{
}

Tex_lookup::Tex_lookup (Lookup const& s)
  : Ps_lookup (s)
{
}

Tex_lookup::Tex_lookup (Symtables const& s)
  : Ps_lookup (s)
{
}

Tex_lookup::~Tex_lookup()
{
}

Atom
Tex_lookup::afm_find (String s) const
{
  return Lookup::afm_find (s, String ("\\char%d"));
}

Atom
Tex_lookup::dashed_slur (Array<Offset> controls, Real thick, Real dash) const
{
  return embed (Ps_lookup::dashed_slur (controls, thick, dash));
}

Atom
Tex_lookup::embed (Atom a) const
{
  a.str_ = "\\embeddedps{\n" + a.str_ + "}";
  return a;
}

Atom
Tex_lookup::hairpin (Real width, bool decresc, bool continued) const
{
  return embed (Ps_lookup::hairpin (width, decresc, continued));
}

Atom
Tex_lookup::plet (Real dy , Real dx, Direction dir) const
{
  return embed (Ps_lookup::plet (dy, dx, dir));
}

Atom
Tex_lookup::ps_beam (Real slope, Real width, Real thick) const
{
  return embed (Ps_lookup::ps_beam (slope, width, thick));
}

Atom
Tex_lookup::slur (Array<Offset> controls) const
{
  return embed (Ps_lookup::slur (controls));
}

Atom
Tex_lookup::stem (Real y1, Real y2) const
{
  return Lookup::stem (y1, y2, "\\kern %\\vrule width % height % depth %");
}

Atom
Tex_lookup::vbracket (Real &y) const
{
  return embed (Ps_lookup::vbracket (y));
}

