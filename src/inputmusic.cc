#include "inputmusic.hh"
#include "voice.hh"

Vertical_simple::Vertical_simple()
{
    voice_ = new Voice;
}

void
Vertical_simple::add(Voice_element*v)
{
    voice_->add(v);
}

Real
Vertical_simple::length()
{
    return voice_->last();
}
void
Vertical_simple::translate_time(Real t)
{
    voice_->start += t;
}
Voice_list
Vertical_simple::convert()
{
    Voice_list l;
    l.bottom().add(voice_);
    return l;
}

    
/****************/

void
Music_voice::add(Voice_element*v)
{
    PCursor<Vertical_music*> c(voice_.bottom());
    if (!c.ok() || !c->simple()) {
	Vertical_simple*vs = new Vertical_simple;
	
	c.add(vs);
    }
    
    c = voice_.bottom();
    Vertical_simple *s = c->simple();
    s->add(v);    	    
}

void
Music_voice::add(Vertical_music*v)
{
    //   v->translate_time(length());
    voice_.bottom().add(v);
}

Real
Music_voice::length()
{
    Real l = 0.0;
    
    for (PCursor<Vertical_music*> i(voice_); i.ok(); i++)
	l += i->length();
    return l;
}

    
Voice_list
Music_voice::convert()
{
    Voice_list l;
    Real here = 0.0;
    
    for (PCursor<Vertical_music*> i(voice_); i.ok(); i++) {
	Real len = i->length();	// has to be stored, since translate_time doesn't work on copies of the contents of i.
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
    for (PCursor<Vertical_music*> i(voice_); i.ok(); i++) 
	i->translate_time(t);
}

    
    
/****************/

void
Music_general_chord::translate_time(Real t)
{
    for (PCursor<Horizontal_music*> i(chord_); i.ok(); i++) 
	i->translate_time(t);    
}

    
	
Real
Music_general_chord::length()
{
    Real l =0.0;
    
    for (PCursor<Horizontal_music*> i(chord_); i.ok(); i++) 
	l = MAX(l, i->length());
    return l;
}

void
Music_general_chord::add(Horizontal_music*h)
{
    chord_.bottom().add(h);
}

Voice_list
Music_general_chord::convert()
{
    Voice_list l;
    for (PCursor<Horizontal_music*> i(chord_); i.ok(); i++) {
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

void
Voice_list::junk()
{
    for (PCursor<Voice*> i(*this); i.ok(); i++)
	delete i.ptr();
}
