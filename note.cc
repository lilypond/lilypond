#include <ctype.h>
#include "string.hh"
#include "real.hh"
#include "debug.hh"
#include "request.hh"
#include "voice.hh"
#include "notename.hh"

int default_duration = 4;

void
parse_duration(const char *a, int &j, int &intdur, int &dots)
{    
    String durstr;    
    while (isdigit(a[j])) 
	{
	durstr += a[j++];
	}

    dots=0;
    
    while (a[j] == '.') 
	{
	j++;
	dots++;
	}
    intdur = (durstr.len()) ?
	durstr.value():default_duration;

    if (debug_flags & DEBUGTOKEN) 
	mtor << "dur " << intdur << "dots " << dots<<eol;
}


void 
parse_pitch( const char *a, int &j, int &oct, bool & overide_acc,
	     int & large, int & small)
{
    // octave
 oct =0;
    
    while (1) 
	{	
	if (a[j] == '\'')
	    oct ++;
	else 	if (a[j] == '`')
	    oct --;
	else
	    break;
	j++;
	
	}
    if (debug_flags & DEBUGTOKEN) mtor << "oct " << oct;
	
    // accidental
    overide_acc = false;
    
    if (a[j] == '!')
	{	
	overide_acc = true;
	j++;
	}

    if (debug_flags & DEBUGTOKEN)
	mtor << "ov " << overide_acc;
    
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
    if (debug_flags & DEBUGTOKEN)
	mtor << "int "<< large <<" "<<small<<" ";    
}


Voice_element *
get_note_element(String pitch, String durstr)
{
    Voice_element*v = new Voice_element;
    int i=0;
    
    int dur, dots;
    parse_duration(durstr, i, dur, dots);
    i=0;

    Note_req * rq = new Note_req( v);

    int oct, pit, acc;
    bool forceacc;
    parse_pitch(pitch, i, oct, forceacc, pit, acc);

    rq->octave = oct;
    rq->accidental = acc;
    rq->forceacc = forceacc;
    rq->balltype = dur;
    rq->dots = dots;
    

    v->add(rq);
    return v;
}

Voice_element *
get_rest_element(String type, String durstr)
{    
    Voice_element*v = new Voice_element;
    int i=0;
    
    int dur, dots;
    parse_duration(durstr, i, dur, dots);
    i=0;

    Rest_req * rq = new Rest_req(v);
  
    rq->balltype = dur;
    rq->dots = dots;    

    v->add(rq);
    return v;
}
