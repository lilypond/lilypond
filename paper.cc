#include "paper.hh"
#include "dimen.hh"

Paperdef::Paperdef()
{
    width = convert_dimen(15,"cm");		// in cm for now
    whole_width= convert_dimen(5,"cm");
}
