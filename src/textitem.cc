#include "request.hh"
#include "paper.hh"
#include "textitem.hh"
#include "stem.hh"
#include "molecule.hh"
#include "lookup.hh"

Text_item::Text_item(Text_req * r, int s)
{
    dir = r->dir;
    if (!dir)
	dir = -1;
    
    specs = r->spec;
    staffsize = s;
    pos = 0;
}

void
Text_item::set_default_pos()
{
    pos  = (dir > 0) ? staffsize + 2: -4;
}
void
Text_item::do_pre_processing()
{
    set_default_pos();
}

    
Molecule*
Text_item::brew_molecule() const
{
    Molecule*    output = new Molecule(specs->create(paper()));
    if(dir <0)
	output->translate(Offset(0, -output->extent().y.length() ));
    
    output->translate(Offset(0, pos * paper()->internote()));
    return output;
}
