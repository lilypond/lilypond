/*
  my-lily-parser.cc -- implement My_lily_parser

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "my-lily-parser.hh"
#include "my-lily-lexer.hh"
#include "debug.hh"
#include "main.hh"
#include "music-list.hh"
#include "musical-request.hh"
#include "command-request.hh"
#include "parser.hh"

void
My_lily_parser::clear_notenames()
{
    lexer_p_->clear_notenames();
}
void
My_lily_parser::set_version_check (bool ig)
{
    ignore_version_b_ = ig;
}
void
My_lily_parser::set_debug()
{
#ifndef NPRINT
    String s = "";
    if (init_parse_b_) 
	s = "Init";
    set_yydebug (!monitor->silence (s+"Parser") && check_debug);
    lexer_p_->set_debug (!monitor->silence (s+"Lexer") && check_debug);
#endif
}

void
My_lily_parser::print_declarations()
{
#ifndef NPRINT
    String s = "";
    
    if (init_parse_b_) 
	s = "Init";
    if (!monitor->silence (s+"Declarations") && check_debug) {
	lexer_p_->print_declarations (init_parse_b_);
    }
#endif   
}

void
My_lily_parser::parse_file (String init, String s)
{
    lexer_p_ = new My_lily_lexer;
    init_str_ = init;
    
    *mlog << "Parsing ... ";
    
    init_parse_b_ = true;
    lexer_p_->new_input (init, source_l_);
    do_yyparse();
    print_declarations();

    init_parse_b_ = false;
    lexer_p_->new_input (s , source_l_);
    do_yyparse();
    print_declarations();

    
    if (!define_spot_array_.empty())
	warning ("Braces don't match.");
}

My_lily_parser::~My_lily_parser()
{
    delete lexer_p_;
}
    
void
My_lily_parser::remember_spot()
{
    define_spot_array_.push (here_input());
}

char const * 
My_lily_parser::here_ch_C()const
{
    return lexer_p_->here_ch_C();
}

void
My_lily_parser::parser_error (String s)
{
    here_input().error (s);
    if ( fatal_error_i_)
	exit (fatal_error_i_);
    error_level_i_ = 1;
}

void
My_lily_parser::set_duration_mode (String s)
{
    s = s.upper_str();
    last_duration_mode_b_ = (s== "LAST");
}

void
My_lily_parser::set_default_duration (Duration const *d)
{
    last_duration_mode_b_ = false;
    default_duration_ = *d;
}


void
My_lily_parser::set_last_duration (Duration const *d)
{
    if (last_duration_mode_b_)
	default_duration_ = *d;
}


Chord*
My_lily_parser::get_word_element (Text_def* tdef_p, Duration * duration_p)
{
    Chord* velt_p = new Request_chord;
    
    Lyric_req* lreq_p = new Lyric_req (tdef_p);

    lreq_p->duration_ = *duration_p;
    lreq_p->set_spot (here_input());

    velt_p->add (lreq_p);

    delete  duration_p;
    return velt_p;
}

Chord *
My_lily_parser::get_rest_element (String s,  Duration * duration_p)
{    
    Chord* velt_p = new Request_chord;
    velt_p->set_spot (here_input());

    if (s=="s") { /* Space */
	Skip_req * skip_p = new Skip_req;
	skip_p->duration_ = *duration_p;

	skip_p->set_spot (here_input());
	velt_p->add (skip_p);
    }
    else {
	Rest_req * rest_req_p = new Rest_req;
	rest_req_p->duration_ = *duration_p;
	rest_req_p->set_spot (here_input());
	
	velt_p->add (rest_req_p);
    }
    Stem_req * stem_p = new Stem_req;
    stem_p->duration_ = *duration_p;
    stem_p->set_spot ( here_input());
    velt_p->add (stem_p);

    delete duration_p;
    return velt_p;
}

Chord *
My_lily_parser::get_note_element (Note_req *rq, Duration * duration_p)
{
    Chord*v = new Request_chord;
    v->set_spot (here_input());

    v->add (rq);
    
    if (duration_p->type_i_ >= 2) {
	Stem_req * stem_req_p = new Stem_req();
	stem_req_p->duration_ = *duration_p;
	
	stem_req_p->set_spot (here_input());
	v->add (stem_req_p);
    }

    rq->set_duration (*duration_p);
    rq->set_spot (here_input());
    delete duration_p ;
    return v;
}

Request*
My_lily_parser::get_parens_request (char c)
{
    Request* req_p=0;
    switch (c) {

    case '~':
	req_p = new Tie_req;
	break;
    case '[':
    case ']':
    {
	Beam_req*b = new Beam_req;
	int p_i=plet_.type_i_ ; // ugh . Should junk?
	if (p_i!= 1)
	    b->nplet = p_i;
	req_p = b;
    }
    break;

    case '>':
    case '!':
    case '<':
	req_p = new Span_dynamic_req;
	break;
    
    case ')':
    case '(':
	req_p = new Slur_req;
	break;
    default:
	assert (false);
	break;
    }
    
    switch (c) {
    case '<':
    case '>':
    case '(':
    case '[':
	req_p->span()->spantype = Span_req::START;
	break;
    case '!':
    case ')':
    case ']':
	req_p->span()->spantype = Span_req::STOP;
	break;
	
    default:
	break;
    }

   if (req_p->musical()->span_dynamic ()) {
	Span_dynamic_req* s_l= (req_p->musical()->span_dynamic ()) ;
	s_l->dynamic_dir_i_ = (c == '<') ? 1:-1;
    }

    req_p->set_spot (here_input());
    return req_p;
}

My_lily_parser::My_lily_parser (Sources * source_l)
{
    first_b_ = true;
    source_l_ = source_l;
    lexer_p_ = 0;
    default_duration_.type_i_ = 4;
    default_octave_i_ = 0;
    textstyle_str_="roman";		// in lexer?
    error_level_i_ = 0;
    last_duration_mode_b_ = true;
    fatal_error_i_ = 0;
}

void
My_lily_parser::add_requests (Chord*v)
{
    for (int i = 0; i < pre_reqs.size(); i++) {
	v->add (pre_reqs[i]);
    }
    pre_reqs.clear();
    for (int i = 0; i <post_reqs.size(); i++) {
	v->add (post_reqs[i]);
    }
    post_reqs.clear();
}

Input
My_lily_parser::pop_spot()
{
    return define_spot_array_.pop();
}

Input
My_lily_parser::here_input()const
{
    Source_file * f_l= lexer_p_->source_file_l();
    return Input (f_l, here_ch_C());
}

void
My_lily_parser::add_notename (String s, Melodic_req * m_p)
{
    lexer_p_->add_notename (s, m_p);
}
