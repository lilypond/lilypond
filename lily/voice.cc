/*
  voice.cc -- implement Voice

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "proto.hh"
#include "plist.hh"
#include "debug.hh"
#include "voice.hh"
#include "musical-request.hh"
#include "command-request.hh"
#include "midi-item.hh"
#include "midi-stream.hh"
#include "voice-element.hh"

void
Voice::transpose(Melodic_req const & d)const
{
     for (iter_bot(elts_, i); i.ok(); i--)
	i->transpose(d); 
}

void
Voice::set_default_group(String s)
{
    elts_.top()->set_default_group(s);
}

Voice::Voice(Voice const&src)
{
    for (iter_top(src.elts_, i); i.ok(); i++)
	add(new Voice_element(**i));

    start_ = src.start_;
}

Voice::Voice()
{
    start_ = 0;
}

void
Voice::add(Voice_element*v)
{
    v->voice_C_ = this;
    elts_.bottom().add(v);
}

void
Voice::print() const
{
#ifndef NPRINT
    mtor << "Voice { start_: "<< start_<<eol;
    for (iter_top(elts_,i); i.ok(); i++)
	i->print();
    mtor << "}\n";
#endif
}

/**
   @return The moment at which last element stops.
 */
Moment
Voice::last() const
{
    Moment l =0;
    if (elts_.size())
	l = start_;
    
    for (iter_top(elts_,i); i.ok(); i++)
	l  += i->duration_;
    return l;
}

