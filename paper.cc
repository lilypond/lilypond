#include "paper.hh"
#include "debug.hh"
#include "lookupsyms.hh"
#include "dimen.hh"
#include "textdb.hh"

Paperdef::Paperdef()
{
    linewidth = convert_dimen(15,"cm");		// in cm for now
    whole_width= convert_dimen(2,"cm");
    lookup_ = new Lookup();
    parse();
    
}

void 
Paperdef::parse()
{
    Text_db symini("symbol.ini");

    
    while (!symini.eof()) {
	
	 Text_record  r(  symini++);
	
	 if  (r[0] == "symboltables")
	     lookup_->parse(symini);	 
    }
}

Paperdef::~Paperdef()
{
    delete lookup_;
}

Real
Paperdef::interline() const
{
    return lookup_->ball(4).dim.y.length();
}
Real
Paperdef::note_width()const
{
    return lookup_->ball(4).dim.x.length( );
}
Real
Paperdef::standard_height() const
{
    return convert_dimen(20,"pt");
}

void
Paperdef::print() const
{
#ifndef NPRINT
    mtor << "Paper {width: " << print_dimen(linewidth);
    mtor << "whole: " << print_dimen(whole_width);
    mtor << "out: " <<outfile;
    mtor << "}\n";
#endif
}
