/*
  voice.cc -- implement Voice

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "voice.hh"
#include "musicalrequest.hh"
#include "commandrequest.hh"
#include "midiitem.hh"
#include "midistream.hh"

void
Voice::set_default_group(String s)
{
    elts.top()->set_default_group(s);
}

bool
Voice::find_plet_start_bo(char c, Moment& moment_r)
{
    for (iter_bot(elts, i); i.ok(); i--)
	if ( i->find_plet_start_bo(c, moment_r) )
	    return true;
    return false;
}

void 
Voice::set_plet_backwards(Moment& now_moment_r, Moment until_moment, int num_i, int den_i)
{
    for (iter_bot(elts, i); i.ok(); i--) 
	if ( now_moment_r <= until_moment ) 
	    i->set_plet_backwards(now_moment_r, until_moment, num_i, den_i);
	else
	    return;
}

Voice::Voice(Voice const&src)
{
    for (iter_top(src.elts, i); i.ok(); i++)
	add(new Voice_element(**i));

    start = src.start;
}

Voice::Voice()
{
    start = 0;
}

void
Voice::add(Voice_element*v)
{
    v->voice_l_ = this;
    elts.bottom().add(v);
}

void
Voice::print() const
{
#ifndef NPRINT
    mtor << "start: "<< start<<eol;
    for (iter_top(elts,i); i.ok(); i++)
	i->print();
#endif
}

Moment
Voice::last() const
{
    Moment l =0;
    if (elts.size())
	l = start;
    
    for (iter_top(elts,i); i.ok(); i++)
	l  += i->duration;
    return l;
}

