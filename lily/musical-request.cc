/*
  request.cc -- implement all musical requests.

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "musical-request.hh"
#include "misc.hh"
#include "debug.hh"
#include "script-def.hh"
#include "text-def.hh"
#include "music-list.hh"



IMPLEMENT_IS_TYPE_B1(Musical_req,Request);
void
Musical_req::do_print() const{}
void
Tie_req::do_print() const{}


/* *************** */




IMPLEMENT_IS_TYPE_B1(Span_req,Musical_req);

void
Span_req::do_print() const
{
#ifndef NPRINT
  DOUT << spantype ;
#endif
}

IMPLEMENT_IS_TYPE_B1(Spacing_req,Request);

Spacing_req::Spacing_req()
{
  next = 0;
  distance = 0;
  strength = 0;
}

void
Spacing_req::do_print() const
{
#ifndef NPRINT
  DOUT << "next " << next << "dist " << distance << "strength\n";
#endif
}

IMPLEMENT_IS_TYPE_B1 (Abbreviation_req, Musical_req);

Abbreviation_req::Abbreviation_req ()
{
  type_i_ = 0;
}

void
Abbreviation_req::do_print() const
{
#ifndef NPRINT
  DOUT << "type " << type_i_ << "\n";
#endif
}


IMPLEMENT_IS_TYPE_B2(Blank_req,Spacing_req,Rhythmic_req);

void
Blank_req::do_print() const
{
  Spacing_req::do_print();
}
/* *************** */

Melodic_req::Melodic_req()
{
  notename_i_ = 0;
  octave_i_ = 0;
  accidental_i_ = 0;
}

void
Melodic_req::transpose (Melodic_req const * delta)
{
  int old_pitch = pitch();
  int delta_pitch = delta->pitch();
  octave_i_ += delta->octave_i_;
  notename_i_ += delta->notename_i_;
  while  (notename_i_ >= 7)
    {
	notename_i_ -= 7;
	octave_i_ ++;
    }

  int new_pitch = pitch();
  int delta_acc = new_pitch - old_pitch - delta_pitch;

  accidental_i_ -= delta_acc;
  if (abs (accidental_i_) > 2)
    {
	delta->warning (_("transposition makes accidental larger than 2"));
    }
}

IMPLEMENT_IS_TYPE_B1(Melodic_req,Musical_req);

bool
Melodic_req::do_equal_b (Request*r) const
{
  Melodic_req* m= r->musical()->melodic ();
  return !compare (*m, *this);
}

int
Melodic_req::compare (Melodic_req const &m1 , Melodic_req const&m2)
{
  int o=  m1.octave_i_ - m2.octave_i_;
  int n = m1.notename_i_ - m2.notename_i_;
  int a = m1.accidental_i_ - m2.accidental_i_;

  if (o)
	return o;
  if (n)
	return n;
  if (a)
	return a;
  return 0;
}

void
Melodic_req::do_print() const
{
#ifndef NPRINT
  DOUT << "notename: " << notename_i_
	 << " acc: " <<accidental_i_<<" oct: "<< octave_i_;
#endif
}

int
Melodic_req::height() const
{
  return  notename_i_ + octave_i_*7;
}

/*
  should be settable from input to allow "viola"-mode
 */
static Byte pitch_byte_a[  ] = { 0, 2, 4, 5, 7, 9, 11 };

int
Melodic_req::pitch() const
{
  return  pitch_byte_a[ notename_i_ % 7 ] + accidental_i_ + octave_i_ * 12;
}

/* *************** */
int
Rhythmic_req::compare (Rhythmic_req const &r1, Rhythmic_req const &r2)
{
  return (r1.duration() - r2.duration ());
}

bool
Rhythmic_req::do_equal_b (Request*r) const
{
  Rhythmic_req* rh = r->musical()->rhythmic ();

  return !compare (*this, *rh);
}

void
Rhythmic_req::set_duration (Duration d)
{
  duration_ = d;
}

Rhythmic_req::Rhythmic_req()
{
}


IMPLEMENT_IS_TYPE_B1(Rhythmic_req,Musical_req);

void
Rhythmic_req::do_print() const
{
#ifndef NPRINT
  DOUT << "duration { " <<duration_.str() << "}";
#endif
}


Moment
Rhythmic_req::duration() const
{
  return duration_.length();
}
/* *************** */

Lyric_req::Lyric_req (Text_def* def_p)
  :Text_req (0, def_p)
{
  def_p->align_i_ = CENTER;	// centre
  dir_ = DOWN;		// lyrics below (invisible) staff
}


IMPLEMENT_IS_TYPE_B2(Lyric_req,Musical_req,Rhythmic_req);

void
Lyric_req::do_print() const
{
  Rhythmic_req::do_print();
  Text_req::do_print();
}

/* *************** */
bool
Note_req::do_equal_b (Request*r) const
{
  return Rhythmic_req::do_equal_b (r) && Melodic_req::do_equal_b (r);
}


Note_req::Note_req()
{
  forceacc_b_ = false;
}

IMPLEMENT_IS_TYPE_B2(Note_req,Melodic_req,Rhythmic_req);

void
Note_req::do_print() const
{
#ifndef NPRINT
  Melodic_req::do_print();
  if (forceacc_b_)
    {
	DOUT << " force accidental\n";
    }
  Rhythmic_req::do_print();
#endif
}
/* *************** */

IMPLEMENT_IS_TYPE_B1(Rest_req,Rhythmic_req);

void
Rest_req::do_print() const
{
      Rhythmic_req::do_print();
}

/* *************** */

IMPLEMENT_IS_TYPE_B1(Beam_req,Span_req);

Beam_req::Beam_req()
{
}

void
Beam_req::do_print() const
{
}

/* *************** */

IMPLEMENT_IS_TYPE_B1 (Abbreviation_beam_req, Span_req);

Abbreviation_beam_req::Abbreviation_beam_req ()
{
  type_i_ = 0;
}

void
Abbreviation_beam_req::do_print () const
{
}

IMPLEMENT_IS_TYPE_B1(Slur_req,Span_req);
void
Slur_req::do_print() const
{
}

IMPLEMENT_IS_TYPE_B1(Plet_req,Span_req);

Plet_req::Plet_req ()
{
  plet_i_ = 0;
}

void
Plet_req::do_print () const
{
}

/* *************** */

bool
Span_req:: do_equal_b (Request*r) const
{
  Span_req * s = r->span();
  return spantype == s->spantype;
}

Span_req::Span_req()
{
  spantype = NOSPAN;
}

/* *************** */
Script_req::Script_req (Script_req const&s)
{
  dir_ = s.dir_;
  scriptdef_p_ = s.scriptdef_p_ ? s.scriptdef_p_->clone() : 0;
}

/*
  don't check dirs?

  (d1.dir_ == d2.dir_)
 */
bool
Script_req::do_equal_b (Request*r) const
{
  Script_req * s = r->script();

  return  scriptdef_p_->equal_b (*s->scriptdef_p_);
}

Script_req::Script_req()
{
  dir_ = CENTER;
  scriptdef_p_ = 0;
}


IMPLEMENT_IS_TYPE_B1(Script_req,Request);

void
Script_req::do_print() const
{
#ifndef NPRINT
  DOUT << " dir " << dir_ ;
  scriptdef_p_->print();
#endif
}

void
Musical_script_req::do_print() const
{
  Script_req::do_print();
}


IMPLEMENT_IS_TYPE_B2(Musical_script_req,Musical_req, Script_req);


Script_req::~Script_req()
{
  delete scriptdef_p_;
}
/* *************** */


Text_req::~Text_req()
{
  delete tdef_p_;
  tdef_p_ = 0;
}

Text_req::Text_req (Text_req const& src)
{
  tdef_p_ = new Text_def (*src.tdef_p_);
  dir_ = src.dir_;
}

Text_req::Text_req (int dir_i, Text_def* tdef_p)
{
  dir_ = Direction(dir_i);
  tdef_p_ = tdef_p;
}


IMPLEMENT_IS_TYPE_B1(Text_req,Musical_req);

void
Text_req::do_print() const
{
#ifndef NPRINT
  DOUT << " dir " << dir_ ;
  tdef_p_->print();
#endif
}

/* *************** */


IMPLEMENT_IS_TYPE_B1(Skip_req,Musical_req);

void
Skip_req::do_print() const
{
#ifndef NPRINT

  DOUT << "duration: " << duration();
#endif
}



IMPLEMENT_IS_TYPE_B1(Dynamic_req,Musical_req);

void
Dynamic_req::do_print() const
{
  Musical_req::do_print();
}


IMPLEMENT_IS_TYPE_B1(Absolute_dynamic_req,Musical_req);

void
Absolute_dynamic_req::do_print() const
{
#ifndef NPRINT
  Dynamic_req::do_print();
  DOUT << " loudness " <<loudness_str ();
#endif
}


bool
Absolute_dynamic_req::do_equal_b (Request *r) const
{
  Absolute_dynamic_req *a = r->musical ()->dynamic ()->absdynamic ();
  return loudness_ == a->loudness_;
}

String
Dynamic_req::loudness_static_str (Loudness l)
{
  switch (l)
    {
    case FFF: return "fff";
    case FF: return "ff";
    case F: return "f";
    case MF: return "mf";
    case MP: return "mp";
    case P: return "p";
    case PP: return "pp";
    case PPP: return "ppp";
    case FP: return "fp";
    case SF: return "sf";
    case SFZ: return "sfz";
    }
  return "";
}

String
Absolute_dynamic_req::loudness_str () const
{
  String s = loudness_static_str (loudness_);
  if (s.empty_b ())
    {
      s = "mf";
      warning (String (_("Never heard of dynamic scale "))
	       + loudness_ + _(" assuming mf"));
    }
  return s;
}


Absolute_dynamic_req::Absolute_dynamic_req()
{
  loudness_ = MF;
}


Span_dynamic_req::Span_dynamic_req()
{
  dynamic_dir_  = CENTER;
}


IMPLEMENT_IS_TYPE_B1(Span_dynamic_req,Musical_req);

void
Span_dynamic_req::do_print() const
{
#ifndef NPRINT
  Span_req::do_print();
  DOUT << "softer/louder: " <<dynamic_dir_;
#endif
}


IMPLEMENT_IS_TYPE_B1(Tie_req,Musical_req);
