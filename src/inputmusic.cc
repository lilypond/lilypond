#include "debug.hh"
#include "inputmusic.hh"
#include "voice.hh"

void
Simple_music::add(Voice_element*v)
{
    voice_.add(v);
}

Real
Simple_music::length()
{
    return voice_.last();
}
void
Simple_music::translate_time(Real t)
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
    for (PCursor<Input_music*> i(elts); i.ok(); i++)
	 i->print();
}

void
Complex_music::concatenate(Complex_music*h)
{
    for (PCursor<Input_music*> i(h->elts); i.ok(); i++)
	add(i->clone());    
}

Complex_music::Complex_music()
{
}

Complex_music::Complex_music(Complex_music const&s)
{
    for (PCursor<Input_music*> i(s.elts); i.ok(); i++)
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

Real
Music_voice::length()
{
    Real l = 0.0;
    
    for (PCursor<Input_music*> i(elts); i.ok(); i++)
	l += i->length();
    return l;
}

    
Voice_list
Music_voice::convert()
{
    Voice_list l;
    Real here = 0.0;
    
    for (PCursor<Input_music*> i(elts); i.ok(); i++) {
	Real len = i->length();	
	Voice_list k(i->convert());
	k.translate_time(here);	
	l.concatenate(k);
	here +=len;
	
    }
    return l;    
}

void
Music_voice::translate_time(Real t)
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
Music_general_chord::translate_time(Real t)
{
    for (PCursor<Input_music*> i(elts); i.ok(); i++) 
	i->translate_time(t);    
}

Real
Music_general_chord::length()
{
    Real l =0.0;
    
    for (PCursor<Input_music*> i(elts); i.ok(); i++) 
	l = l >? i->length();
    return l;
}

Voice_list
Music_general_chord::convert()
{
    Voice_list l;
    for (PCursor<Input_music*> i(elts); i.ok(); i++) {
	Voice_list k(i->convert());
	l.concatenate(k);
    }
    return l;
}


/****************/

void
Voice_list::translate_time(Real x)
{
    for (PCursor<Voice*> i(*this); i.ok(); i++)
	i->start += x;    
}

