#include <ctype.h>

#include "string.hh"
#include "real.hh"
#include "debug.hh"
#include "request.hh"
#include "voice.hh"
#include "notename.hh"
#include "vray.hh"

int default_duration = 4, default_dots=0, default_octave=0;

void
parse_duration(const char *a, int &j, int &intdur, int &dots)
{    
    String durstr;    
    while (isdigit(a[j])) 
	{
	durstr += a[j++];
	}

    dots=default_dots;
    
    while (a[j] == '.') 
	{
	j++;
	dots++;
	}
    
    intdur = (durstr.len()) ?
	durstr.value():default_duration;

    mtor << "dur " << intdur << "dots " << dots<<eol;
}



void 
parse_pitch( const char *a, int &j, int &oct, bool & overide_acc,
	     int & large, int & small)
{
    // octave
    oct =default_octave;
    
    while (1) 
	{	
	if (a[j] == '\'')
	    oct ++;
	else if (a[j] == '`')
	    oct --;
	else
	    break;
	j++;
	
	}

	mtor << "oct " << oct;
	
    // accidental
    overide_acc = false;
    
    if (a[j] == '!')
	{	
	overide_acc = true;
	j++;
	}

    
    // notename.
    String nm;
    while (isalpha(a[j])) 
	{
	nm += a[j++];
	}
    if (isupper(nm[0]))
	{
	oct--;	
	nm.lower();
	}
        

    lookup_notename(large,small,nm);
    mtor << "override: " << overide_acc;    
    mtor << "pitch "<< large <<", "<<small<<"\n";    
}


Voice_element *
get_note_element(String pitch, String durstr)
{
    Voice_element*v = new Voice_element;
    int i=0;
    
    int dur, dots;
    parse_duration(durstr, i, dur, dots);
    i=0;

    Note_req * rq = new Note_req;

    if (dur >= 2) {
	Stem_req * st = new Stem_req(dur);
	v->add(st);
    }
    
    int oct, pit, acc;
    bool forceacc;
    parse_pitch(pitch, i, oct, forceacc, pit, acc);
    rq->name =pit;
    
    rq->octave = oct;
    rq->accidental = acc;
    rq->forceacc = forceacc;
    rq->balltype = dur;
    rq->dots = dots;
    
    rq->print();

    v->add(rq);

    return v;
}

Voice_element *
get_rest_element(String, String durstr)
{    
    Voice_element*v = new Voice_element;
    int i=0;
    
    int dur, dots;
    parse_duration(durstr, i, dur, dots);
    i=0;

    Rest_req * rq = new Rest_req;
  
    rq->balltype = dur;
    rq->dots = dots;    
    rq->print();
    v->add(rq);

    return v;
}

void
set_default_duration(String d)
{
    int i=0;
    parse_duration(d, i, default_duration, default_dots);
}


void
set_default_pitch(String d)
{
    int i=0;
    bool b;
    int l,s;
    parse_pitch(d, i, default_octave, b, l,s);
}

Request*
get_request(char c)
{
    Request* ret=0;
    switch (c) {
    case '[':
    case ']':
	ret = new Beam_req;
	break;

    case ')':
    case '(':
	ret = new Slur_req;
	break;
    default:
	assert(false);
	break;
    }
    
    switch (c) {
    case '(':
    case '[':
	ret->span()->spantype = Span_req::START;
	break;
    case ')':
    case ']':
	ret->span()->spantype = Span_req::STOP;
	break;
    default:
	assert(false);
	break;
    }

    return ret;
}

void
add_requests(Voice_element *v, svec<Request*> &req)
{
    for (int i = 0; i < req.sz(); i++) {
	v->add(req[i]);
    }
    req.set_size(0);
}
