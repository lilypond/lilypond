#include "staffline.hh"
#include "scoreline.hh"
#include "dimen.hh"
#include "spanner.hh"
#include "symbol.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "p-col.hh"
#include "p-score.hh"


IMPLEMENT_STATIC_NAME(Line_of_staff);

void
Line_of_staff::add_element(Score_elem*elem_l)
{
    if (!elem_l->group_element_i_)
	Element_group::add_element(elem_l);
}
