/*
  UGR!! CHAOS RULEZ
  */
#include "textspanner.hh"
#include "script.hh"
#include "request.hh"
#include "voice.hh"
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
#include "slur.hh"
#include "localkeyitem.hh"
#include "textitem.hh"
#include "lyricitem.hh"

Rhythmic_grouping
parse_grouping(Array<Scalar> a, Moment one_beat)
{
    Array<int> r;
    for (int i= 0 ; i < a.size(); i++)
	r.add(a[i]);
    Moment here =0.0;

    Array<Rhythmic_grouping*> children;
    for (int i=0; i < r.size(); i++) {
	
	Moment last = here;
	here += one_beat * r[i];
	children.add(
	    new Rhythmic_grouping(MInterval(last, here), r[i] )
	    );
    }
    return Rhythmic_grouping(children);
}

void
Simple_walker::do_INTERPRET_command(Command*com)
{
    Array<Scalar> args(com->args);
    args.del(0);
    if (com->args[0] == "GROUPING") {	
	default_grouping = parse_grouping(args,
					  col()->tdescription_->one_beat);
    }else if (com->args[0] == "NEWMEASURE") {
	local_key_.reset(key_);

    } else if (com->args[0] == "KEY") {
	
	if (col()->when() > Moment(0)) {
	    assert(!oldkey_undo);
	    oldkey_undo = new Array<int>( key_.oldkey_undo(args));
	}
	
	typesetkey = key_.read(args);
	local_key_.reset(key_);	
    } else if (com->args[0] == "CLEF") {
	clef_.read(args);
    } else {
	WARN << " ignoring INTERPRET command: " << com->args[0]<< '\n';
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
	    com->args[0] = "KEY"; // urgh
    
    if (com->args[0] == "CURRENTCLEF") {
	if (processed_clef) 
	    return;
    }
    if (com->args[0] == "BAR") {
	
	if (processed_bar_priority > com->priority)
	    return;
	else
	    processed_bar_priority = com->priority;
    }

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

    if (com->args[0] == "CLEF"||com->args[0] == "CURRENTCLEF") {
	processed_clef =true;
	Clef_item*c=(Clef_item*)i;
	c->read(clef_);
	c->change = (break_status != BREAK_POST - BREAK_PRE);
    }
    col()->typeset_item_directional(i, 1, break_status);
}

void
Simple_walker::do_local_key(Note_req*n,Notehead* head_p)
{
    if ( local_key_.oct(n->octave).acc(n->name) != n->accidental) {
	if (!local_key_item_) {
	    local_key_item_ = staff()->get_local_key_item();
	    local_key_item_->c0_position = clef_.c0_pos;
	}
	
	local_key_item_->add(n->octave, n->name, n->accidental, head_p);
	local_key_.oct(n->octave).set(n->name, n->accidental);
    }
}

void
Simple_walker::do_note(Note_info n)
{
    Simple_column*c = col();
    Simple_staff *s = staff();
    	Item*rhythmic=0;
    if (n.rq->note()) {
	Note_req * req = n.rq->note() ;
	const Voice *v = req->elt_l_->voice_;

	Notehead*n = s->get_notehead(req, clef_.c0_pos);
	rhythmic=n;
	if (stem_)
	    stem_->add(n);
	
	if (current_grouping) {
	    current_grouping->add_child(
		c->tdescription_->whole_in_measure, req->duration());
	}
	noteheads.add(n);
	int sidx =find_slur(v);
	if (sidx >= 0) {
	    pending_slurs[sidx]->add(n);
	}

	if (wantkey)
	    do_local_key(req,n);
    }else if (n.rq->rest()) {
	rhythmic = s->get_rest(n.rq->rest());
	c->typeset_item(rhythmic);
    }
    for (int i= 0; i < n.scripts.size(); i ++)
	c->typeset_item(new Script(n.scripts[i], rhythmic, 10, stem_));	// UGR
}

void
Simple_walker::process_requests()
{
    Simple_column*c = col();
    Simple_staff *s = staff();

    if (c->beam_&& c->beam_->spantype == Span_req::START) {
	if (beam_)
	    error("Too many beams (t = "
			  +String(c->when())+")");
	beam_ = new Beam;
	assert(!current_grouping);
	current_grouping = new Rhythmic_grouping;
    }
    
    for (int i=0; i < c->slurs.size(); i++) {
	Slur_req*sl = c->slurs[i];

	if (sl->spantype == Span_req::START) {
	    if  (find_slur(sl->elt_l_->voice_ )>=0)
		error_t("Too many slurs in voice", *col()->tdescription_);
	    pending_slur_reqs.add(sl);
	    pending_slurs.add(new Slur);
	}
    }
    
    if (c->text_) {
	c->typeset_item(new Text_item(c->text_, 10)); // UGR
    }

//    if (c->lreq_p_) {
//	c->typeset_item(new Lyric_item(c->lreq_p_, 10)); // UGR
//    }

    if (c->stem_) {
	stem_ = s->get_stem(c->stem_->stem(), c->stem_requester_len);
    }
    
    for (int i = 0; i <  c->notes.size(); i ++)  {
	do_note(c->notes[i]);
    }
    
    if (beam_) {
	if (!stem_)
	    WARN <<"beamed note should have a stem (t = " 
		  <<String(c->when())<<")\n";
	else
	    beam_->add(stem_);


    }
    if (stem_) {
	c->typeset_item(stem_);
	/* needed, otherwise placement of
	   local_key fucks up */
    }

    if (c->beam_&& c->beam_->spantype == Span_req::STOP) {
	default_grouping.extend(current_grouping->interval());
	beam_->set_grouping(default_grouping, *current_grouping);
	pscore_->typeset_spanner(beam_, s->theline_l_);

	if (c->beam_->nplet) {
	    Text_spanner* t = new Text_spanner(beam_);
	    t->spec.align_i_ = 0;
	    t->spec.text_str_ = c->beam_->nplet;
	    pscore_->typeset_spanner(t, s->theline_l_);
	}

	beam_ = 0;
	delete current_grouping;
	current_grouping =0;
    }
    for (int i = 0; i < noteheads.size(); i++) {
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
    for (int i=0; i < c->slurs.size(); i++) {
	Slur_req *sl = c->slurs[i];

	if (sl->spantype == Span_req::STOP) {
	    int idx = find_slur(sl->elt_l_->voice_);
	    if (idx < 0)
		error_t("can't find slur to end; ", *c->tdescription_);
	    
	    pscore_->typeset_spanner(pending_slurs[idx],
				     s->theline_l_);
	    pending_slurs.del(idx);
	    pending_slur_reqs.del(idx);
	}	
    }
    
}

Simple_walker::Simple_walker(Simple_staff*s)
    : Staff_walker(s, s->theline_l_->pscore_l_)
{
    stem_ = 0;
    beam_ =0;
    oldkey_undo = 0;
    current_grouping = 0;
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
    processed_bar_priority = 0;
}

int
Simple_walker::find_slur(const Voice *v)
{
    for (int i=0; i < pending_slur_reqs.size(); i++) {
	if (pending_slur_reqs[i]->elt_l_->voice_ == v)
	    return i;
    }
    return -1;
}

    
