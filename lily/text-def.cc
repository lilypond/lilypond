#include "debug.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "text-def.hh"

Text_def::Text_def()
{   
    align_i_ = 1;			// right
    style_str_ = "roman";
    defined_ch_c_l_ = 0;
}
bool
Text_def::compare(const Text_def&def)
{
    return align_i_ == def.align_i_ && text_str_ == def.text_str_
	&& style_str_ == def.style_str_;
}

Atom
Text_def::create_atom(Paper_def*p) const
{
    return p->lookup_l()->text(style_str_, text_str_, -align_i_);
}

void
Text_def::print() const
{
    mtor << "Text `" << text_str_ << "\', style " <<
	style_str_ << "align " << align_i_ << '\n';
}
