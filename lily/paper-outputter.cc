/*
  paper-outputter.cc -- implement Paper_outputter

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "paper-outputter.hh"
#include "paper-stream.hh"
#include "molecule.hh"
#include "atom.hh"
#include "array.hh"
#include "string-convert.hh"
#include "debug.hh"
#include "lookup.hh"
#include "main.hh"

Paper_outputter::Paper_outputter (Paper_stream *s)
{
  outstream_l_ = s;
}

Paper_outputter::~Paper_outputter ()
{
}

void
Paper_outputter::output_molecule (Molecule const*m, Offset o, char const *nm, String s)
{
  if (check_debug)
    *outstream_l_ << String ("\n%start: ") << nm << "\n";

  for (PCursor <Atom*> i (m->atoms_); i.ok (); i++)
    {
      Offset a_off = i->offset ();
      a_off += o;

      switch_to_font (i->font_);

      Array<String> a;
      String r;
  
      a.push (global_lookup_l->print_dimen (a_off.y()));
      a.push (global_lookup_l->print_dimen (a_off.x()));
      a.push (i->str_);
      r += global_lookup_l->substitute_args (s, a);
      *outstream_l_ << r;
    }
}
