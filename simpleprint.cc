#include "lookupsyms.hh"
#include "request.hh"
#include "pscore.hh"
#include "paper.hh"
#include "simplestaff.hh"
#include "molecule.hh"
#include "sccol.hh"

Item *
Simple_column::create_req_item(Request *rq)
{
    Item *i = new Item;
    Symbol s;
    int dots=0;

    if (rq->note())
	s = staff_->paper()->lookup_->ball(rq->note()->balltype);
    else if (rq->rest())
	s = staff_->paper()->lookup_->rest(rq->rest()->balltype);

    if (rq->rhythmic())
	dots=rq->rhythmic()->dots;


    Molecule *m = new Molecule(Atom(s));
    if (dots) {
	Symbol d = staff_->paper()->lookup_->dots(dots);
	Molecule dm;
	dm.add(Atom(d));
	m->add_right(dm);
    }
    i->output=m;
    return i;
}
Item *
Simple_column::create_command_item(Command *com)
{
    Item *i = new Item;
    Symbol s;

    if (com -> args[0] ==  "BAR" ) {
	s = staff_->paper()->lookup_->bar(com->args[1]);	
    } else if (com->args[0] == "METER") {
	Parametric_symbol *p = staff_->paper()->lookup_->meter("general");
	svec<String> arg( com->args);
	arg.del(0);
	s = p->eval(arg);
    } else
	assert(false);

    Molecule * m =new Molecule(Atom(s));
    {
	Interval wid;
	svec<Item*> sv(staff_->pscore_->
		       select_items(staff_->theline, score_column->pcol));
	for (int j=0; j<sv.sz(); j++) {
	    wid.unite(sv[j]->output->extent().x);
	}
	if (!wid.empty())
	    m->translate(Offset(wid.max,0));
    }
    i->output=m;
    return i;
}

void
Simple_column::typeset_item(Item *i, int breakst)
{
    // ugh
    staff_->pscore_->typeset_item(i, score_column->pcol,
				  staff_->theline,breakst);
}

void
Simple_staff::set_output(PScore* ps )
{
    pscore_ = ps;
    pscore_->add(theline);
}
