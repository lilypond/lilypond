#include "debug.hh"
#include "input-music.hh"
#include "voice.hh"
#include "musical-request.hh"
#include "command-request.hh"
#include "voice-element.hh"

void
Input_music::check_plet(Voice_element* velt_l)
{
    for (iter_top(velt_l->reqs,i); i.ok(); i++)
	if ( i->plet() ) {
	    Moment start_moment = 0;
	    if ( !find_plet_start_b( i->plet()->type_c_, start_moment ) ) {
		error( "begin of plet not found", i->defined_ch_C_ );
	        break;
	    }
	    Moment moment = 0;
	    set_plet_backwards( moment, start_moment, i->plet()->dur_i_, i->plet()->type_i_ );
	    i.del();
	    break;
        }
}

void
Simple_music::transpose(Melodic_req const &d)const
{
    voice_.transpose(d);
}

void
Simple_music::add(Voice_element*v)
{
    voice_.add(v);
}

Moment
Simple_music::length()const
{
    return voice_.last();
}
void
Simple_music::translate_time(Moment t)
{
    voice_.start += t;
}

Voice_list
Simple_music::convert()const
{
    Voice_list l;
    Voice * v_p = new Voice(voice_);
    PCursor<Voice_element*> i= v_p->elts.bottom();
	// need-to-have, otherwise memory will be filled up with regs. 
    if (!i.ok() || i->duration_) {
	v_p->add ( new Voice_element);
	i=v_p->elts.bottom();
    }
    i->add(new Terminate_voice_req); 
    l.bottom().add(v_p);
    return l;
}


void
Simple_music::print() const
{
#ifndef NPRINT
    mtor << "Simple_music {";
    voice_.print();
    mtor << "}\n";
#endif
}
bool
Simple_music::find_plet_start_b(char c, Moment& moment_r)
{
    return voice_.find_plet_start_b(c, moment_r);
}
void 
Simple_music::set_plet_backwards(Moment& now_moment_r, Moment until_moment, 
				 int num_i, int den_i)
{
    voice_.set_plet_backwards(now_moment_r, until_moment, num_i, den_i);
}

/* *************** */

void
Complex_music::transpose(Melodic_req const& d) const
{
     for (iter_top(elts,i); i.ok(); i++)
	 i->transpose(d);
}
void
Complex_music::add(Input_music*v)
{
    elts.bottom().add(v);
}

void
Complex_music::print() const
{
    for (iter_top(elts,i); i.ok(); i++)
	 i->print();
}

void
Complex_music::concatenate(Complex_music*h)
{
    for (iter_top(h->elts,i); i.ok(); i++)
	add(i->clone());    
}

Complex_music::Complex_music()
{
}

Complex_music::Complex_music(Complex_music const&s)
{
    for (iter_top(s.elts,i); i.ok(); i++)
	add(i->clone());
}
void
Complex_music::set_default_group(String g)
{
    for (iter_top(elts,i); i.ok(); i++)
	    i->set_default_group(g);
}
bool
Complex_music::find_plet_start_b(char c, Moment& moment_r)
{
    for (iter_bot(elts,i); i.ok(); i--) {
        if ( i->find_plet_start_b(c, moment_r) )
	    return true;
    }
    return false;
}
void 
Complex_music::set_plet_backwards(Moment& now_moment_r, Moment until_moment, int num_i, int den_i)
{
    for (iter_bot(elts,i); i.ok(); i--) {
    	i->set_plet_backwards(now_moment_r, until_moment, num_i, den_i);
    }
}
/* *************************************************************** */

void
Music_voice::print() const
{
    mtor << "Music_voice {";
    Complex_music::print();    
    mtor << "}\n";
}

void
Music_voice::add_elt(Voice_element*v)
{
    PCursor<Input_music*> c(elts.bottom());
    if (!c.ok() || !c->simple()) {
	Simple_music*vs = new Simple_music;
	
	c.add(vs);
    }
    
    c = elts.bottom();
    Simple_music *s = c->simple();
    s->add(v);    	    

    check_plet(v);
}

Moment
Music_voice::length()const
{
    Moment l = 0;
    
    for (iter_top(elts,i); i.ok(); i++)
	l += i->length();
    return l;
}

    
Voice_list
Music_voice::convert()const
{
    Voice_list l;
    Moment here = 0;
    
    for (iter_top(elts,i); i.ok(); i++) {
	Moment len = i->length();	
	Voice_list k(i->convert());
	k.translate_time(here);	
	l.concatenate(k);
	here +=len;	
    }
    return l;    
}

void
Music_voice::translate_time(Moment t)
{
    elts.bottom()->translate_time(t);
}

    
    
/* *************** */

void
Music_general_chord::add_elt(Voice_element*v)
{
    Simple_music*vs = new Simple_music;
    vs->add(v);
    elts.bottom().add(vs);

    check_plet(v);
}

void
Music_general_chord::print() const
{
    mtor << "Music_general_chord {";
    Complex_music::print();
     mtor << "}\n";
}

void
Music_general_chord::translate_time(Moment t)
{
    for (iter_top(elts,i); i.ok(); i++) 
	i->translate_time(t);    
}

Moment
Music_general_chord::length()const
{
    Moment l =0;
    
    for (iter_top(elts,i); i.ok(); i++) 
	l = l >? i->length();
    return l;
}

Voice_list
Music_general_chord::convert()const
{
    Voice_list l;
    for (iter_top(elts,i); i.ok(); i++) {
	Voice_list k(i->convert());
	l.concatenate(k);
    }
    return l;
}

/* *************** */

void
Multi_voice_chord::set_default_group(String g)
{
    int j=0;
    for (iter_top(elts, i); i.ok(); i++) {
	i->set_default_group(g + String(j));
	j++;
    }
}


/* *************** */

void
Voice_list::translate_time(Moment x)
{
    for (iter_top(*this,i); i.ok(); i++)
	i->start += x;    
}

