#include "debug.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "text-def.hh"
#include "dimen.hh"

Interval
Text_def::width() const
{
    Atom a = create_atom();

    Real guess_width_f = text_str_.length_i() * a.sym.dim.x.length(); // ugh
    Interval i(0, guess_width_f);
    i += - (align_i_ + 1)* i.center();
    return i;
}


Text_def::Text_def()
{   
    align_i_ = 1;			// right
    pdef_l_ = 0;
    style_str_ = "roman";
}
bool
Text_def::compare(Text_def const &def)
{
    return align_i_ == def.align_i_ && text_str_ == def.text_str_
	&& style_str_ == def.style_str_;
}

Atom
Text_def::create_atom() const
{
    return pdef_l_->lookup_l()->text(style_str_, text_str_, -align_i_);
}

void
Text_def::print() const
{
    mtor << "Text `" << text_str_ << "\', style " <<
	style_str_ << "align " << align_i_ << '\n';
}
