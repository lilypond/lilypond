/*
  text-def.cc -- implement Text_def

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "text-def.hh"
#include "dimen.hh"

Interval
Text_def::width(Paper_def * p) const
{
    Atom a = get_atom(p,0);

    Real guess_width_f = text_str_.length_i() * a.sym.dim.x.length(); // ugh
    Interval i(0, guess_width_f);
    i += - (align_i_ + 1)* i.center();
    return i;
}


Text_def::Text_def()
{   
    align_i_ = 1;			// right
    style_str_ = "roman";
}
bool
Text_def::do_equal_b(Text_def const &def)const
{
    return align_i_ == def.align_i_ && text_str_ == def.text_str_
	&& style_str_ == def.style_str_;
}

Atom
Text_def::get_atom(Paper_def *p, int ) const
{
    return p->lookup_l()->text(style_str_, text_str_, -align_i_);
}

void
Text_def::print() const
{
    mtor << "Text `" << text_str_ << "\', style " <<
	style_str_ << "align " << align_i_ << '\n';
}
IMPLEMENT_STATIC_NAME(Text_def);
