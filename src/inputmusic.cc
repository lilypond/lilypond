#include "debug.hh"
#include "inputmusic.hh"
#include "voice.hh"

Vertical_simple::Vertical_simple()
{
    voice_ = new Voice;
}
Vertical_simple::Vertical_simple(Vertical_simple const&s)
{
    voice_ = new Voice(*s.voice_);
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
    l.bottom().add(new Voice(*voice_));
    return l;
}

Vertical_simple::~Vertical_simple()
{
    delete voice_;
}

void
Vertical_simple::print() const
{
    mtor << "Vertical_simple {";
    voice_->print();
    mtor << "}\n";
}

/****************/
void
Music_voice::print() const
{
     mtor << "Music_voice {";
    for (PCursor<Vertical_music*> i(voice_); i.ok(); i++)
	 i->print();
    mtor << "}\n";
}

void
Music_voice::concatenate(Music_voice*h)
{
    for (PCursor<Vertical_music*> i(h->voice_); i.ok(); i++)
	add(i->clone());    
}


Music_voice::Music_voice(Music_voice const&s)
{
    for (PCursor<Vertical_music*> i(s.voice_); i.ok(); i++)
	add(i->clone());
}

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
    for (PCursor<Vertical_music*> i(voice_); i.ok(); i++) 
	i->translate_time(t);
}

    
    
/****************/
void
Music_general_chord::print() const
{
    mtor << "Music_general_chord {";
     for (PCursor<Horizontal_music*> i(chord_); i.ok(); i++)
	i->print();
     mtor << "}\n";
}

void
Music_general_chord::concatenate(Music_general_chord*v)
{
    for (PCursor<Horizontal_music*> i(v->chord_); i.ok(); i++)
	add(i->clone());
}

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


Music_general_chord::Music_general_chord(
    Music_general_chord const & s)
{
    for (PCursor<Horizontal_music*> i(s.chord_); i.ok(); i++) {
	add(i->clone());
    }
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
