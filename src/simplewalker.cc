#include "request.hh"
#include "beam.hh"
#include "pscore.hh"
#include "simplestaff.hh"
#include "sccol.hh"
#include "stem.hh"
#include "notehead.hh"
#include "rest.hh"
#include "debug.hh"

void
Simple_walker::process_command(Command*com)
{
    switch (com->code){
    case BREAK_PRE:
    case BREAK_MIDDLE:
    case BREAK_POST:
    case BREAK_END:
	(*this)->score_column->set_breakable();
	break_status = com->code- BREAK_PRE;
	break;
    case INTERPRET:
	break;
	
    case TYPESET:
    {
	Item* i = staff()->get_TYPESET_item(com);	
	col()->typeset_item(i, break_status);
    }
	break;
   
    default :
	break;
    }	
}

void
Simple_walker::process_requests()
{
    Simple_column*c = col();
    Simple_staff *s = staff();
    if (c->beam_&& c->beam_->spantype == Span_req::START) {
	if (beam_)
	    error("Too many beams");
	beam_ = new Beam;
    }
    
    if (c->stem_) {
	stem_ = s->get_stem(c->stem_->stem());
	c->typeset_item(stem_);
    }
    
    for (int i = 0; i <  c->notes.sz(); i ++)  {
	Rhythmic_req*rq = c->notes[i];
	if (rq->note()) {
	    Notehead*n = s->get_notehead(rq->note());
	    stem_->add(n);
	    noteheads.add(n);
	}
	
	if (rq->rest()) {
	    c->typeset_item( s->get_rest(rq->rest()) );
	}
    }
    
    
    if (beam_) {
	beam_->add(stem_);
    }
    
    if (c->beam_&& c->beam_->spantype == Span_req::STOP) {
	pscore_->typeset_spanner(beam_, s->theline);
	beam_ = 0;
    }
    for (int i = 0; i < noteheads.sz(); i++) {
	c->typeset_item(noteheads[i]);
    }
    noteheads.set_size(0);

    if (stem_) {
	stem_ = 0;
    }
}

Simple_walker::Simple_walker(Simple_staff*s)
    : Staff_walker(s, s->theline->pscore_)
{
    stem_ = 0;
    beam_ =0;
}



Simple_staff*
Simple_walker::staff()
{
    return (Simple_staff*) staff_;
}

Simple_column*
Simple_walker::col()
{
    return (Simple_column*) *(*this);
}


