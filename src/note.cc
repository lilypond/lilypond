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
parse_octave (const char *a, int &j, int &oct)
{    
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
}

void 
parse_pitchmod( const char *a, int &j, int &oct, bool & overide_acc)
{
    // octave
    oct =default_octave;
    parse_octave(a,j,oct);
	
    // accidental
    overide_acc = false;
    
    if (a[j] == '!')
	{	
	overide_acc = true;
	j++;
	}

    mtor << "oct " << oct;
    mtor << "override: " << overide_acc;        
}


Voice_element *
get_note_element(String pitch, int * notename, int * duration )
{
    Voice_element*v = new Voice_element;
    int i=0;
    
    int dur = duration[0];
    int dots=duration[1];
    
    Note_req * rq = new Note_req;

    if (dur >= 2) {
	Stem_req * st = new Stem_req(dur);
	v->add(st);
    }
    
    int oct;
    bool forceacc;
    parse_pitchmod(pitch, i, oct, forceacc);
    rq->name =notename[0];
    rq->accidental = notename[1];
    rq->octave = oct;
    rq->forceacc = forceacc;
    rq->balltype = dur;
    rq->dots = dots;
    
    rq->print();

    v->add(rq);

    return v;
}

Voice_element *
get_rest_element(String,  int * duration )
{    
    Voice_element*v = new Voice_element;

    Rest_req * rq = new Rest_req;
  
    rq->balltype = duration[0];
    rq->dots = duration[1];    
    rq->print();
    v->add(rq);

    return v;
}

void
get_default_duration(int *p)
{
   *p++ = default_duration;
    *p = default_dots;
}

void
set_default_duration(int *p)
{
     default_duration = *p++;
     default_dots = *p++;
}


void
set_default_octave(String d)
{
    int i=0;
    default_octave=0;
    parse_octave(d, i, default_octave);
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
