/*
  my-lily-parser.cc -- implement My_lily_parser

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "my-lily-parser.hh"
#include "my-lily-lexer.hh"
#include "debug.hh"
#include "main.hh"
#include "music-list.hh"
#include "musical-request.hh"
#include "command-request.hh"
#include "parser.hh"
#include "header.hh"
#include "file-results.hh"

My_lily_parser::My_lily_parser (Sources * source_l)
{
  first_b_ = true;
  source_l_ = source_l;
  lexer_p_ = 0;
  abbrev_beam_type_i_ = 0;
  default_duration_.durlog_i_ = 2;
  default_octave_i_ = 0;
  textstyle_str_="roman";		// in lexer?
  error_level_i_ = 0;
  last_duration_mode_b_ = true;
  fatal_error_i_ = 0;
  default_header_p_ =0;

  relative_octave_mode_b_ = false;

  last_melodic_ = new Melodic_req;
  last_melodic_->octave_i_ = 0; // -1; // huh?
  last_melodic_->notename_i_ = 0;
  last_melodic_->accidental_i_ = 0;
}

My_lily_parser::~My_lily_parser()
{
  delete lexer_p_;
  delete default_header_p_;
}


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
My_lily_parser::parse_file (String init, String s)
{
  lexer_p_ = new My_lily_lexer;
  init_str_ = init;
  lexer_p_->main_input_str_ = s;

  *mlog << _("Parsing ... ");

  init_parse_b_ = false;
  set_yydebug (!monitor->silent_b ("Parser") && check_debug);
  lexer_p_->new_input (init, source_l_);
  do_yyparse ();


  if (!define_spot_array_.empty())
    {
      warning (_("Braces don't match."));
      error_level_i_ = 1;
    }

  inclusion_global_array = lexer_p_->filename_str_arr_;
}

void
My_lily_parser::remember_spot()
{
  define_spot_array_.push (here_input());
}

char const *
My_lily_parser::here_ch_C() const
{
  return lexer_p_->here_ch_C();
}

void
My_lily_parser::parser_error (String s)
{
  here_input().error (s);
  if (fatal_error_i_)
    exit (fatal_error_i_);
  error_level_i_ = 1;
  exit_status_i_ = 1;
}

void
My_lily_parser::set_duration_mode (String s)
{
  s = s.upper_str();
  last_duration_mode_b_ = (s== "LAST");
}

void
My_lily_parser::set_octave_mode (String s)
{
  s = s.upper_str();
  if (s == "RELATIVE")
    {
      relative_octave_mode_b_ = true;
      // must reset these
      last_melodic_ = new Melodic_req;
      last_melodic_->octave_i_ = 0; // -1; // huh?
      last_melodic_->notename_i_ = 0;
      last_melodic_->accidental_i_ = 0;
    }
  else
    relative_octave_mode_b_ = false;
}

void
My_lily_parser::set_abbrev_beam (int type_i)
{
  abbrev_beam_type_i_ = type_i;
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
    {
      default_duration_ = *d;
      /* 
        forget plet part,
        sticky plet factor only within plet brackets
       */  
      default_duration_.set_plet (1, 1);
    }
}

String
My_lily_parser::notename_str (Melodic_req* melodic)
{
  // ugh
  String str ((char)('a' + ((melodic->notename_i_ + 2) % 7)));
  int i = melodic->accidental_i_;
  while (i-- > 0) 
    str += "is";
  i++;
  while (i++ < 0)
    str += "es";
  return str;
}

Melodic_req* 
My_lily_parser::get_melodic_req (Melodic_req* melodic, int quotes)
{
  if (relative_octave_mode_b_)
    {
      set_nearest (melodic);
      int d = melodic->pitch () - last_melodic_->pitch ();
      if (quotes && (sign (d) == sign (quotes)))
	quotes -= sign (quotes);
      melodic->octave_i_ += quotes;
    }
  else
    {
      Melodic_req nearest (*melodic);
      set_nearest (&nearest);
      melodic->octave_i_ += quotes;

      if (find_quarts_global_b)
	{
	  int e = melodic->pitch () - nearest.pitch ();
	  if (e)
	    {
	      int d = melodic->pitch () - last_melodic_->pitch ();
	      String str = _("Interval bigger than quart");
	      int n = 1 + (abs (d) - 1) / 12;
	      String quote_str ('\'', n);
#if 0
	      str += d < 0 ? _(", prepend: ") : _(", append: ");
	      str += quote_str;
#else
	      str += _(", relative: ");
	      String name_str = notename_str (melodic);
	      str += d < 0 ? quote_str + name_str : name_str + quote_str;
#endif
	      melodic->warning (str);
	    }
	}
    }
  delete last_melodic_;
  last_melodic_ = melodic->clone ()->musical ()->melodic ();
  return melodic;
}

void
My_lily_parser::set_nearest (Melodic_req* melodic)
{
  melodic->octave_i_ = last_melodic_->octave_i_;
  int d = melodic->pitch () - last_melodic_->pitch ();
  if (abs (d) > 6)
    melodic->octave_i_ -= sign (d);
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

  if (s=="s")
    { /* Space */
      Skip_req * skip_p = new Skip_req;
      skip_p->duration_ = *duration_p;

      skip_p->set_spot (here_input());
      velt_p->add (skip_p);
    }
  else
    {
      Rest_req * rest_req_p = new Rest_req;
      rest_req_p->duration_ = *duration_p;
      rest_req_p->set_spot (here_input());

      velt_p->add (rest_req_p);
    }

  delete duration_p;
  return velt_p;
}

Chord *
My_lily_parser::get_note_element (Note_req *rq, Duration * duration_p)
{
  Chord*v = new Request_chord;
  v->set_spot (here_input ());

  v->add (rq);

  // too bad parser reads (default) duration via member access,
  // this hack will do for now..
  if (abbrev_beam_type_i_)
    {
      assert (!duration_p->plet_b ());
      duration_p->set_plet (1, 2);
    }
  rq->set_duration (*duration_p);
  rq->set_spot (here_input ());
  delete duration_p ;
  return v;
}

Array<Request*>*
My_lily_parser::get_parens_request (int t)
{
  Array<Request*>& reqs = *new Array<Request*>;
  switch (t)
    {
    case '~':
      reqs.push (new Tie_req);
      break;
    case BEAMPLET:
    case MAEBTELP:
      {
	Plet_req* p = new Plet_req;
	p->plet_i_ = plet_.type_i_;
	reqs.push (p);
      }
      /* fall through */
    case '[':
    case ']':
      {
	if (!abbrev_beam_type_i_)
	  {
	    reqs.push (new Beam_req);
	  }
	else
	  {
	    Abbreviation_beam_req* a = new Abbreviation_beam_req;
	    a->type_i_ = abbrev_beam_type_i_;
	    if (t==']')
	      abbrev_beam_type_i_ = 0;
	    reqs.push (a);
	  }
      }
      break;

    case '>':
    case '!':
    case '<':
      reqs.push (new Span_dynamic_req);
      break;

    case PLET:  
    case TELP:
      {
	Plet_req* p = new Plet_req;
	p->plet_i_ = plet_.type_i_;
	reqs.push (p);
      }
      break;
    case ')':
    case '(':
      {
	reqs.push (new Slur_req);
      }
      break;
    default:
      assert (false);
      break;
    }

  switch (t)
    {
    case BEAMPLET:
      reqs.top ()->span()->spantype = Span_req::START;
      /* fall through */
    case '<':
    case '>':
    case '(':
    case '[':
    case PLET:
      reqs[0]->span ()->spantype = Span_req::START;
      break;
    case MAEBTELP:
      reqs.top ()->span()->spantype = Span_req::STOP;
      /* fall through */
    case '!':
    case ')':
    case ']':
      reqs[0]->span ()->spantype = Span_req::STOP;
      break;

    default:
      break;
    }

  for (int i = 0; i < reqs.size (); i++)
    if (reqs[i]->musical ()->span_dynamic ())
      {
	Span_dynamic_req* s_l= (reqs[i]->musical ()->span_dynamic ()) ;
	s_l->dynamic_dir_ = (t == '<') ? UP:DOWN;
      }

  // ugh? don't we do this in the parser too?
  reqs[0]->set_spot (here_input());
  return &reqs;
}

void
My_lily_parser::add_requests (Chord*v)
{
  for (int i = 0; i < pre_reqs.size(); i++)
    {
      v->add (pre_reqs[i]);
    }
  pre_reqs.clear();
  for (int i = 0; i <post_reqs.size(); i++)
    {
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
My_lily_parser::here_input() const
{
  Source_file * f_l= lexer_p_->source_file_l();
  return Input (f_l, here_ch_C());
}

void
My_lily_parser::add_notename (String s, Melodic_req * m_p)
{
  lexer_p_->add_notename (s, m_p);
}

