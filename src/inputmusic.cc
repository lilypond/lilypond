#include "debug.hh"
#include "inputmusic.hh"
#include "voice.hh"

void
Simple_music::add(Voice_element*v)
{
    voice_.add(v);
}

Moment
Simple_music::length()
{
    return voice_.last();
}
void
Simple_music::translate_time(Moment t)
{
    voice_.start += t;
}

Voice_list
Simple_music::convert()
{
    Voice_list l;
    l.bottom().add(new Voice(voice_));
    return l;
}


void
Simple_music::print() const
{
    mtor << "Simple_music {";
    voice_.print();
    mtor << "}\n";
}

/****************/

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

/****************************************************************/

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
}

Moment
Music_voice::length()
{
    Moment l = 0.0;
    
    for (iter_top(elts,i); i.ok(); i++)
	l += i->length();
    return l;
}

    
Voice_list
Music_voice::convert()
{
    Voice_list l;
    Moment here = 0.0;
    
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

    
    
/****************/

void
Music_general_chord::add_elt(Voice_element*v)
{
    Simple_music*vs = new Simple_music;
    vs->add(v);
    elts.bottom().add(vs);
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
Music_general_chord::length()
{
    Moment l =0.0;
    
    for (iter_top(elts,i); i.ok(); i++) 
	l = l >? i->length();
    return l;
}

Voice_list
Music_general_chord::convert()
{
    Voice_list l;
    for (iter_top(elts,i); i.ok(); i++) {
	Voice_list k(i->convert());
	l.concatenate(k);
    }
    return l;
}


/****************/

void
Voice_list::translate_time(Moment x)
{
    for (iter_top(*this,i); i.ok(); i++)
	i->start += x;    
}

