/*
  request.cc -- implement all musical requests.

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "musical-request.hh"
#include "misc.hh"
#include "debug.hh"
#include "script-def.hh"
#include "text-def.hh"
#include "music-list.hh"



IMPLEMENT_IS_TYPE_B1 (Musical_req,Request);
void
Musical_req::do_print () const{}
void
Tie_req::do_print () const{}



IMPLEMENT_IS_TYPE_B1(Span_req,Request);

IMPLEMENT_IS_TYPE_B2(Musical_span_req, Span_req, Musical_span_req);

void
Musical_span_req::do_print () const
{
  Span_req::do_print ();
}
	     

void
Span_req::do_print () const
{
#ifndef NPRINT
  DOUT << spantype;
#endif
}

IMPLEMENT_IS_TYPE_B1 (Spacing_req,Request);

Spacing_req::Spacing_req ()
{
  next = 0;
  distance = 0;
  strength = 0;
}

void
Spacing_req::do_print () const
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
Abbreviation_req::do_print () const
{
#ifndef NPRINT
  DOUT << "type " << type_i_ << '\n';
#endif
}


IMPLEMENT_IS_TYPE_B2 (Blank_req,Spacing_req,Rhythmic_req);

void
Blank_req::do_print () const
{
  Spacing_req::do_print ();
}

Melodic_req::Melodic_req ()
{
}

void
Melodic_req::transpose (Musical_pitch delta)
{
  pitch_.transpose (delta);
  
  if (abs (pitch_.accidental_i_) > 2)
    {
	warning (_f ("transposition by %s makes accidental larger than two",
	  delta.str ()));
    }
}

IMPLEMENT_IS_TYPE_B1 (Melodic_req,Musical_req);

bool
Melodic_req::do_equal_b (Request*r) const
{
  Melodic_req* m= r->access_Musical_req ()->access_Melodic_req ();
  return !compare (*m, *this);
}

int
Melodic_req::compare (Melodic_req const &m1 , Melodic_req const&m2)
{
  return Musical_pitch::compare (m1.pitch_, m2.pitch_);
}

void
Melodic_req::do_print () const
{
pitch_.print ();
}

int
Rhythmic_req::compare (Rhythmic_req const &r1, Rhythmic_req const &r2)
{
  return (r1.duration () - r2.duration ());
}

bool
Rhythmic_req::do_equal_b (Request*r) const
{
  Rhythmic_req* rh = r->access_Musical_req ()->access_Rhythmic_req ();

  return !compare (*this, *rh);
}


Rhythmic_req::Rhythmic_req ()
{
}


IMPLEMENT_IS_TYPE_B1 (Rhythmic_req,Musical_req);

void
Rhythmic_req::do_print () const
{
#ifndef NPRINT
  DOUT << "duration { " <<duration_.str () << "}";
#endif
}


Moment
Rhythmic_req::duration () const
{
  return duration_.length ();
}

void
Rhythmic_req::compress (Moment m)
{
  duration_.compress (m);
}
  



IMPLEMENT_IS_TYPE_B1 (Lyric_req,Rhythmic_req);

void
Lyric_req::do_print () const
{
  Rhythmic_req::do_print ();
}


bool
Note_req::do_equal_b (Request*r) const
{
  return Rhythmic_req::do_equal_b (r) && Melodic_req::do_equal_b (r);
}


Note_req::Note_req ()
{
  forceacc_b_ = false;
}

IMPLEMENT_IS_TYPE_B2 (Note_req,Melodic_req,Rhythmic_req);

void
Note_req::do_print () const
{
#ifndef NPRINT
  Melodic_req::do_print ();
  if (forceacc_b_)
    {
	DOUT << " force accidental\n";
    }
  Rhythmic_req::do_print ();
#endif
}

IMPLEMENT_IS_TYPE_B1 (Rest_req, Rhythmic_req);

void
Rest_req::do_print () const
{
      Rhythmic_req::do_print ();
}




IMPLEMENT_IS_TYPE_B1 (Multi_measure_rest_req, Rhythmic_req);

void
Multi_measure_rest_req::do_print () const
{
      Rhythmic_req::do_print ();
}



IMPLEMENT_IS_TYPE_B1 (Beam_req,Span_req);

Beam_req::Beam_req ()
{
}

void
Beam_req::do_print () const
{
}


IMPLEMENT_IS_TYPE_B1 (Abbreviation_beam_req, Span_req);

Abbreviation_beam_req::Abbreviation_beam_req ()
{
  type_i_ = 0;
}

void
Abbreviation_beam_req::do_print () const
{
}

IMPLEMENT_IS_TYPE_B1 (Slur_req,Span_req);
void
Slur_req::do_print () const
{
}

IMPLEMENT_IS_TYPE_B1 (Plet_req,Span_req);

Plet_req::Plet_req ()
{
  plet_i_ = 0;
}

void
Plet_req::do_print () const
{
}


bool
Span_req:: do_equal_b (Request*r) const
{
  Span_req * s = r->access_Span_req ();
  return spantype == s->spantype;
}

Span_req::Span_req ()
{
  spantype = NOSPAN;
}

Script_req::Script_req (Script_req const&s)
{
  dir_ = s.dir_;
  scriptdef_p_ = s.scriptdef_p_ ? s.scriptdef_p_->clone () : 0;
}

/*
  don't check dirs?

  (d1.dir_ == d2.dir_)
 */
bool
Script_req::do_equal_b (Request*r) const
{
  Script_req * s = r->access_Script_req ();

  return  scriptdef_p_->equal_b (*s->scriptdef_p_);
}

Script_req::Script_req ()
{
  dir_ = CENTER;
  scriptdef_p_ = 0;
}


IMPLEMENT_IS_TYPE_B1 (Script_req,Request);

void
Script_req::do_print () const
{
#ifndef NPRINT
  DOUT << " dir " << dir_;
  scriptdef_p_->print ();
#endif
}

void
Musical_script_req::do_print () const
{
  Script_req::do_print ();
}


IMPLEMENT_IS_TYPE_B2 (Musical_script_req,Musical_req, Script_req);


Script_req::~Script_req ()
{
  delete scriptdef_p_;
}


Text_req::~Text_req ()
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
  dir_ = Direction (dir_i);
  tdef_p_ = tdef_p;
}


IMPLEMENT_IS_TYPE_B1 (Text_req,Musical_req);

void
Text_req::do_print () const
{
#ifndef NPRINT
  DOUT << " dir " << dir_;
  tdef_p_->print ();
#endif
}



IMPLEMENT_IS_TYPE_B1 (Skip_req,Musical_req);

void
Skip_req::do_print () const
{
#ifndef NPRINT

  DOUT << "duration: " << duration ();
#endif
}



IMPLEMENT_IS_TYPE_B1 (Dynamic_req,Musical_req);

void
Dynamic_req::do_print () const
{
  Musical_req::do_print ();
}


IMPLEMENT_IS_TYPE_B1 (Absolute_dynamic_req,Musical_req);

void
Absolute_dynamic_req::do_print () const
{
#ifndef NPRINT
  Dynamic_req::do_print ();
  DOUT << " loudness " <<loudness_str ();
#endif
}


bool
Absolute_dynamic_req::do_equal_b (Request *r) const
{
  Absolute_dynamic_req *a = r->access_Musical_req ()->access_Dynamic_req ()->access_Absolute_dynamic_req ();
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
  String str = loudness_static_str (loudness_);
  if (str.empty_b ())
    {
      String s = "mf";
      warning (_f ("never heard of dynamic scale `\%s\', assuming %s",
	str, s));
      str = s;
    }
  return str;
}


Absolute_dynamic_req::Absolute_dynamic_req ()
{
  loudness_ = MF;
}



bool
Span_dynamic_req::do_equal_b (Request *req) const
{
  Span_dynamic_req * s = req->access_Musical_req ()->access_Span_dynamic_req ();

  return Span_req::do_equal_b (req) && s->dynamic_dir_ == dynamic_dir_;
}

Span_dynamic_req::Span_dynamic_req ()
{
  dynamic_dir_  = CENTER;
}


IMPLEMENT_IS_TYPE_B1 (Span_dynamic_req,Musical_req);

void
Span_dynamic_req::do_print () const
{
#ifndef NPRINT
  Span_req::do_print ();
  DOUT << "softer/louder: " << dynamic_dir_;
#endif
}


IMPLEMENT_IS_TYPE_B1 (Tie_req,Musical_req);
