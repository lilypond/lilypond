#include "request.hh"
#include "clef.hh"
#include "beam.hh"
#include "pscore.hh"
#include "simplestaff.hh"
#include "simplewalker.hh"
#include "sccol.hh"
#include "stem.hh"
#include "notehead.hh"
#include "rest.hh"
#include "debug.hh"
#include "keyitem.hh"
#include "clefitem.hh"
#include "keyitem.hh"
#include "localkeyitem.hh"

void
Simple_walker::do_INTERPRET_command(Command*com)
{
    if (com->args[0] == "BAR") {
	local_key_.reset(key_);
    } else if (com->args[0] == "KEY") {
	svec<String> s(com->args);
	s.del(0);
	if (com->when) {
	    assert(!oldkey_undo);
	    oldkey_undo = new svec<int>( key_.oldkey_undo(s));
	}
	
	typesetkey = key_.read(s);
	local_key_.reset(key_);	
    } else if (com->args[0] == "CLEF") {
	svec<String> s(com->args);
	s.del(0);
	clef_.read(s);
    } else {
	WARN << " ignoring INTERPRET command: " << com->args[0];
    }
}

void
Simple_walker::do_TYPESET_command(Command*com)
{
    /* ignore these commands if non-default versions have been
      processed.  */
    if (com->args[0] == "CURRENTKEY") 
	if (processed_key) 
	    return;
	else
	    com->args[0] = "KEY"; 
    
    if (com->args[0] == "CURRENTCLEF")
	if (processed_clef) {	    
	    return;
	} else
	    com->args[0] = "CLEF";
    

    Item* i = staff()->get_TYPESET_item(com);
    if (!i)
	return;

    if (com->args[0] == "KEY") {
	if (oldkey_undo) {
	    ((Keyitem*) i)->read(*oldkey_undo);
	    delete oldkey_undo;
	    oldkey_undo = 0;
	}
	processed_key = true;
	((Keyitem*) i)->read(typesetkey); // ugh	
    }

    if (com->args[0] == "CLEF") {
	processed_clef =true;
	((Clef_item*)i)->read(clef_);	
    }
    col()->typeset_item_directional(i, 1, break_status);
}

void
Simple_walker::do_local_key(Note_req*n)
{
    if ( local_key_.oct(n->octave).acc(n->name) != n->accidental) {
	if (!local_key_item_) {
	    local_key_item_ = staff()->get_local_key_item();
	}
	
	local_key_item_->add(n->octave, n->name, n->accidental);	
	local_key_.oct(n->octave).set(n->name, n->accidental);
    }
}

void
Simple_walker::do_notes()
{
    Simple_column*c = col();
    Simple_staff *s = staff();
    for (int i = 0; i <  c->notes.sz(); i ++)  {
	Rhythmic_req*rq = c->notes[i];
	if (rq->note()) {
	    Note_req * req = rq->note() ;
	    Notehead*n = s->get_notehead(req, clef_.c0_pos);
	    stem_->add(n);
	    noteheads.add(n);

	    if (wantkey)
		do_local_key(req);
	}
	
	if (rq->rest()) {
	    c->typeset_item( s->get_rest(rq->rest()) );
	}
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
    }
    
    do_notes();

    if (beam_) {
	beam_->add(stem_);
    }
    if (stem_) {
	c->typeset_item(stem_);
	/* needed, otherwise placement of
	   local_key fucks up */
	stem_->set_default_extents();
    }
    if (c->beam_&& c->beam_->spantype == Span_req::STOP) {
	pscore_->typeset_spanner(beam_, s->theline);
	beam_ = 0;
    }
    for (int i = 0; i < noteheads.sz(); i++) {
	c->typeset_item(noteheads[i]);
    }
    noteheads.set_size(0);

    if (local_key_item_) {
	c->typeset_item_directional(local_key_item_, -1);
	local_key_item_ = 0;	
    }
    if (stem_) {
	stem_ = 0;
    }
}

Simple_walker::Simple_walker(Simple_staff*s)
    : Staff_walker(s, s->theline->pscore_)
{
    stem_ = 0;
    beam_ =0;
    oldkey_undo = 0;

    Local_key_item * i = s->get_local_key_item();
    wantkey  =i;
    delete i;
    local_key_item_ = 0;
    reset();
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

void
Simple_walker::reset()
{
    processed_clef =false;    
    processed_key = false;
}
