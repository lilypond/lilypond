#include "keyitem.hh"
#include "stem.hh"
#include "rest.hh"
#include "notehead.hh"
#include "paper.hh"
#include "molecule.hh"
#include "linepstaff.hh"
#include "complexmelodicstaff.hh"
#include "sccol.hh" 
#include "localkeyitem.hh"
#include "request.hh"

const int NO_LINES=5;


void
Complex_melodic_staff::set_output(PScore*ps)
{
    theline_l_ = new Linestaff(NO_LINES,ps); // theline_l_ is added to pscore later.
    Complex_staff::set_output(ps);
}

Item *
Complex_melodic_staff::get_TYPESET_item(Command*com)
{
    if (com->args[0] == "KEY") {
	return new Keyitem(NO_LINES);	// urgh. depends on clef.
    } else
	return Complex_staff::get_TYPESET_item(com);
}

