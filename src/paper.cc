#include <math.h>

#include "paper.hh"
#include "debug.hh"
#include "lookup.hh"
#include "dimen.hh"
#include "textdb.hh"

double log2(double x) {
    return log(x)  /log(2.0);
}


// golden ratio
const Real PHI = (1+sqrt(5))/2;

// see  Roelofs, p. 57
Real
Paperdef::duration_to_dist(Real d)
{
    return whole_width * pow(geometric_, log2(d));
}
Real
Paperdef::rule_thickness()const
{
    return convert_dimen(0.4, "pt");
}
Paperdef::Paperdef()
{
    lookup_ = new Lookup();
    parse();
    linewidth = convert_dimen(15,"cm");		// in cm for now
    whole_width= 8*note_width();
//    geometric_ = PHI;
        geometric_ = sqrt(2);
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
