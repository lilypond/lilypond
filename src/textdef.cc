#include "debug.hh"
#include "lookup.hh"
#include "paper.hh"
#include "molecule.hh"
#include "textdef.hh"

Text_def::Text_def()
{   
    align = 1;			// right
    style = "roman";
}

Atom
Text_def::create(Paperdef*p) const
{
    return p->lookup_->text(style, text, -align);
}

void
Text_def::print() const
{
    mtor << "Text `" << text << "\', style " <<
	style << "align " <<align<<'\n';
}
