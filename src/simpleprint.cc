#include "clefitem.hh"
#include "request.hh"
#include "pscore.hh"
#include "paper.hh"
#include "simplestaff.hh"
#include "sccol.hh"
#include "rest.hh"
#include "debug.hh"
#include "bar.hh"
#include "meter.hh"

Item *
Simple_staff::get_TYPESET_item(Command *com)
{
    Item *s=0;
    svec<String> arg( com->args);
    String type =arg[0];
    arg.del(0);
    if (type ==  "BAR" ) {
	s = new Bar(com->args[1]);	
    } else if (type == "METER") {
	s = new Meter(arg);
    } else if (type == "CLEF" ||type == "CURRENTCLEF") {
	Clef_item * c = new Clef_item;
	s = c;
	c->change = (type == "CLEF");	
    }else{
	WARN << "ignoring TYPESET command for " << type << '\n';

    }
    return s;
}


Interval
itemlist_width(const svec<Item*> &its)
{
    Interval iv;
    for (int j =0; j < its.sz(); j++)
	iv.unite(its[j]->width());
    return iv;
}

void
Simple_column::typeset_item(Item *i, int breakst)
{
    assert(i);
    
    staff_->pscore_->typeset_item(i, score_column->pcol_,
				  staff_->theline,breakst);
    
    if (breakst == BREAK_PRE - BREAK_PRE) {
	
        svec<Item*> to_move(
	    staff_->pscore_->select_items(staff_->theline,
					  score_column->pcol_->prebreak));
	Interval column_wid = itemlist_width(to_move);
	assert(!column_wid.empty());

	for (int j=0; j < to_move.sz(); j++) {
	    to_move[j]->translate(Offset(-column_wid.max, 0));
	}
    }
}    

void
Simple_column::typeset_item_directional(Item *i, int dir, int breakst)
{
    assert(i);
    PCol * c=score_column->pcol_;
    if (breakst == 0)
	c = c->prebreak;
    else if (breakst == 2)
	c = c->postbreak;
    
    svec<Item*> to_move(staff_->pscore_->select_items(staff_->theline,
						      c));    
    typeset_item(i, breakst);

    Interval column_wid = itemlist_width(to_move);
    if (column_wid.empty())
	column_wid = Interval(0,0);
    i->translate(Offset(column_wid[dir] - i->width()[-dir], 0));
}

void
Simple_staff::set_output(PScore* ps )
{
    pscore_ = ps;
    pscore_->add(theline);
}


Rest*
Simple_staff::get_rest(Rest_req*rq)
{
    int b = rq->rhythmic()->balltype;
    int d = rq->rhythmic()->dots;
    return new Rest(b, d);  
}

Local_key_item*
Simple_staff::get_local_key_item()
{
    return   0;
}

