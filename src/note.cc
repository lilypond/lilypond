#include <ctype.h>
#include "lexer.hh"
#include "string.hh"
#include "real.hh"
#include "debug.hh"
#include "request.hh"
#include "voice.hh"
#include "notename.hh"
#include "identparent.hh"
#include "varray.hh"
#include "textdef.hh"

int default_duration = 4, default_dots=0, default_octave=0;
int default_plet_type = 1, default_plet_dur = 1;
String textstyle="roman";		// in lexer?

/* triplet is '2/3' */
void set_plet(int num,int den)
{
    assert(num >0&& den>0);
    default_plet_dur = num;
    default_plet_type = den;
}

Text_def*
get_text(String s) return t;
{
    t= new Text_def;
    t->text= s;
    t->style = textstyle;
    return t;
}

void
set_text_style(String s)
{
    textstyle = s;
}

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
    mtor << "override: " << overide_acc<<'\n';
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
    rq->plet_factor = Moment(default_plet_dur, default_plet_type);
    rq->print();

    v->add(rq);

    return v;
}

Voice_element *
get_rest_element(String,  int * duration )
{    
    Voice_element*v = new Voice_element;

    Rest_req * rq = new Rest_req;
    rq->plet_factor = Moment(default_plet_dur, default_plet_type);
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
    case '|':
	ret = new Barcheck_req;
	break;
    case '[':
    case ']':
    {
	Beam_req*b = new Beam_req;
	if (default_plet_type != 1)
	    b->nplet = default_plet_type;
	ret = b;
    }
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

	break;
    }

    return ret;
}

void
add_requests(Voice_element *v, Array<Request*> &req)
{
    for (int i = 0; i < req.size(); i++) {
	v->add(req[i]);
    }
    req.set_size(0);
}

Script_def*
get_scriptdef(char c)
{
    String s;
    switch (c) {
    case '^' : s = "marcato";
	break;
    case  '+' : s = "stopped";
	break;
    case '-' : s = "tenuto";
	break;
    case  '|':  s = "staccatissimo";
	break;
    case  'o' : s = "";
	break;
    case '>' : s = "accent";
	break;
    case  'v' : s = ""; 
	break;
    case  '.' : s = "staccato";
	break;
    }
    return lexer->lookup_identifier(s)->script(1);
}

Request*
get_script_req(int d , Script_def*def)
{
    return new Script_req(d, def);
}


Request*
get_text_req(int d , Text_def*def)
{
    return new Text_req(d, def);
}

Voice_element*
get_mark_element(String s)
{
    Voice_element*v_p = new Voice_element;
    v_p->add( new Mark_req(s));
    
    return v_p;
}
