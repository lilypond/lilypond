/*
  could use cleanup
 */
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
#include "parseconstruct.hh"

int default_duration = 4, default_dots=0, default_octave=0;
int default_plet_type = 1, default_plet_dur = 1;
String textstyle="roman";		// in lexer?

bool last_duration_mode = false;

void
set_duration_mode(String s)
{
    s.upper();
    last_duration_mode = (s== "LAST");
}

void
last_duration(int n)
{
    if (last_duration_mode)
	default_duration = n;
}

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
    t->text_str_= s;
    t->style_str_ = textstyle;
    t->defined_ch_c_l_m = defined_ch_c_l;
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
    v->defined_ch_c_l_m = defined_ch_c_l;
    int i=0;
    
    int dur = duration[0];
    int dots=duration[1];

    if (dur >= 2) {
	Stem_req * stem_req_p = new Stem_req(dur,dots);
	stem_req_p->plet_factor = Moment(default_plet_dur, default_plet_type);
	stem_req_p->defined_ch_c_l_m = defined_ch_c_l;
	v->add(stem_req_p);
    }
    
    if ( !defined_ch_c_l )
        defined_ch_c_l = lexer->here_ch_c_l();

    Note_req * rq = new Note_req;
    
    int oct;
    bool forceacc;
    parse_pitchmod(pitch, i, oct, forceacc);
    rq->notename =notename[0];
    rq->accidental = notename[1];
    rq->octave = oct + notename[2];
    rq->forceacc = forceacc;
    rq->balltype = dur;
    rq->dots = dots;
    rq->plet_factor = Moment(default_plet_dur, default_plet_type);
    rq->defined_ch_c_l_m = defined_ch_c_l;
    rq->print();

    v->add(rq);

    return v;
}

Voice_element*
get_word_element(Text_def* tdef_p, int* duration)
{
    Voice_element* velt_p = new Voice_element;
    velt_p->defined_ch_c_l_m = defined_ch_c_l;
    
    int dur = duration[0];
    int dots=duration[1];
    
    tdef_p->defined_ch_c_l_m = defined_ch_c_l;
#if 0
    char buf[ 21 ];
    strncpy( buf, tdef_p->defined_ch_c_l_m, 20 );
    buf[ 20 ] = 0;
    cout << hex << (void*)tdef_p->defined_ch_c_l_m << dec << buf << endl;
#endif
    Lyric_req* lreq_p = new Lyric_req(tdef_p);

    lreq_p->balltype = dur;
    lreq_p->dots = dots;
    lreq_p->plet_factor = Moment(default_plet_dur, default_plet_type);
    lreq_p->print();
    lreq_p->defined_ch_c_l_m = defined_ch_c_l;

    velt_p->add(lreq_p);

    return velt_p;
}

Voice_element *
get_rest_element(String,  int * duration )
{    
    Voice_element* velt_p = new Voice_element;
    velt_p->defined_ch_c_l_m = defined_ch_c_l;

    Rest_req * rest_req_p = new Rest_req;
    rest_req_p->plet_factor = Moment(default_plet_dur, default_plet_type);
    rest_req_p->balltype = duration[0];
    rest_req_p->dots = duration[1];    
    rest_req_p->print();
    rest_req_p->defined_ch_c_l_m = defined_ch_c_l;

    velt_p->add(rest_req_p);

    return velt_p;
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
    Request* req_p=0;
    switch (c) {
    case '|':
	req_p = new Barcheck_req;
	break;

    case '[':
    case ']':
    {
	Beam_req*b = new Beam_req;
	if (default_plet_type != 1)
	    b->nplet = default_plet_type;
	req_p = b;
    }
	break;


    case ')':
    case '(':
	req_p = new Slur_req;
	break;
    default:
	assert(false);
	break;
    }
    
    switch (c) {
    case '(':
    case '[':
	req_p->span()->spantype = Span_req::START;
	break;
    case ')':
    case ']':
	req_p->span()->spantype = Span_req::STOP;
	break;
	
    default:
	break;
    }

    req_p->defined_ch_c_l_m = req_defined_ch_c_l;
    return req_p;
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
    Script_req* script_req_p = new Script_req(d, def);
    // all terminal symbols, rather set directly here:
    script_req_p->defined_ch_c_l_m = lexer->here_ch_c_l();
    return script_req_p;
}

Request*
get_text_req(int d , Text_def*def)
{
    Text_req* text_req_p = new Text_req(d, def);
    text_req_p->defined_ch_c_l_m = defined_ch_c_l;
    return text_req_p;
}

Voice_element*
get_mark_element(String s)
{
    Voice_element*v_p = new Voice_element;
    v_p->defined_ch_c_l_m = defined_ch_c_l;
    Mark_req* mark_req_p = new Mark_req(s);
    mark_req_p->defined_ch_c_l_m = defined_ch_c_l;
    v_p->add(mark_req_p); 
    return v_p;
}
Voice_element*
get_command_element(Input_command*com_p)
{
    Voice_element *velt_p = new Voice_element;
    velt_p->defined_ch_c_l_m = defined_ch_c_l;
    Staff_command_req* scommand_req_p = new Staff_command_req(com_p);
    scommand_req_p->defined_ch_c_l_m = defined_ch_c_l;
    velt_p->add(scommand_req_p);
    return velt_p;
}
Voice_element*
get_barcheck_element()
{
    Voice_element* velt_p = new Voice_element;
    velt_p->defined_ch_c_l_m = req_defined_ch_c_l;
    Barcheck_req* barcheck_req_p = new Barcheck_req;
    barcheck_req_p->defined_ch_c_l_m = req_defined_ch_c_l;
    velt_p->add(barcheck_req_p);
    return velt_p;
}

Voice_element*
get_stemdir_element(int d)
{
    Voice_element*v_p = new Voice_element;
    v_p->defined_ch_c_l_m = req_defined_ch_c_l;
    Group_feature_req * gfreq_p = new Group_feature_req;
    gfreq_p->stemdir_i_ =d; 
    gfreq_p->defined_ch_c_l_m = req_defined_ch_c_l;
    v_p->add(gfreq_p);
    return v_p;
}
