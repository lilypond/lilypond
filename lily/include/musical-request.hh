/*
  musical-request.hh -- declare Musical requests

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef MUSICALREQUESTS_HH
#define MUSICALREQUESTS_HH

#include "lily-proto.hh"
#include "request.hh"
#include "duration.hh"
#include "musical-pitch.hh"

/**
  A request which is coupled to a note (that has duration).
  Base class only
 */
class Musical_req  : public virtual Request  {
public:

  DEFAULTACCESSOR(Lyric_req)
  DEFAULTACCESSOR(Note_req )
  DEFAULTACCESSOR(Stem_req )
  DEFAULTACCESSOR(Melodic_req )
  DEFAULTACCESSOR(Slur_req )
  DEFAULTACCESSOR(Beam_req )
  DEFAULTACCESSOR(Abbreviation_beam_req)
  DEFAULTACCESSOR(Rhythmic_req)
  DEFAULTACCESSOR(Musical_script_req)
  DEFAULTACCESSOR(Text_req)
  DEFAULTACCESSOR(Rest_req )
  DEFAULTACCESSOR(Skip_req)
  DEFAULTACCESSOR(Dynamic_req)
  DEFAULTACCESSOR(Absolute_dynamic_req )
  DEFAULTACCESSOR(Tie_req)
  DEFAULTACCESSOR(Musical_span_req)
  DEFAULTACCESSOR(Plet_req)
  DEFAULTACCESSOR(Span_dynamic_req )
  DEFAULTACCESSOR(Abbreviation_req)
  DEFAULTACCESSOR(Multi_measure_rest_req)
  REQUESTMETHODS(Musical_req);
};



/** a request with a duration.
  This request is used only used as a base class.
 */
class Rhythmic_req  : public virtual Musical_req  {
public:
  Duration duration_;
    
  bool do_equal_b (Request*) const;
  void compress (Moment);
  virtual Moment duration() const;
  Rhythmic_req();
  static int compare (Rhythmic_req const&,Rhythmic_req const&);
  REQUESTMETHODS(Rhythmic_req);
};

class Skip_req  : public Rhythmic_req  {
public:
  REQUESTMETHODS(Skip_req);
};

struct Spacing_req :virtual Request {
  Moment next;
  Real distance;
  Real strength;
  Spacing_req();
  REQUESTMETHODS(Spacing_req);
};

struct Abbreviation_req : public Musical_req {
  REQUESTMETHODS (Abbreviation_req);
  Abbreviation_req ();
  int type_i_;
};

class Blank_req  : public Spacing_req, Rhythmic_req  {
public:
  REQUESTMETHODS(Spacing_req);
};

/// Put a text above or below (?) this staff.
class Text_req  : public virtual Musical_req  {
public:
  /// preferred position (above/below)
  Direction dir_;
  /// the characteristics of the text
  Text_def *tdef_p_;

  Text_req (int d, Text_def*);
  ~Text_req();
  Text_req (Text_req const&);

  REQUESTMETHODS(Text_req);
};

/** a syllable  or lyric is a string with rhythm.
  */
class Lyric_req  : public  Rhythmic_req  {
public:
  String text_str_;
  REQUESTMETHODS(Lyric_req);
};



/// request which has some kind of pitch
struct Melodic_req :virtual Musical_req
{
  Musical_pitch pitch_;
  /// transpose. #delta# is relative to central c.
  virtual void transpose (Musical_pitch delta);
  Melodic_req();
  bool do_equal_b (Request*) const;
  static int compare (Melodic_req const&,Melodic_req const&);
  REQUESTMETHODS(Melodic_req);
};

/// Put a note of specified type, height, and with accidental on the staff.
class Note_req  : public Rhythmic_req, virtual public Melodic_req  {
public:
    
  /// force/supress printing of accidental.
  bool forceacc_b_;
  Note_req();
  bool do_equal_b (Request*) const;
  Rhythmic_req* access_Rhythmic_req () { return Rhythmic_req::access_Rhythmic_req (); }
  REQUESTMETHODS(Note_req);
};

/**
Put a rest on the staff. Why a request? It might be a good idea to not typeset the rest, if the paper is too crowded.
*/
class Rest_req : public Rhythmic_req {
public:
  REQUESTMETHODS(Rest_req);
};

/**
 Part: typeset a measure with the number of measures rest
 Score: typeset all individual measures ass full rests
 */
class Multi_measure_rest_req : public Rhythmic_req  {
public:
  REQUESTMETHODS(Multi_measure_rest_req);

};

class Musical_span_req : public Span_req, public virtual Musical_req
{
public:
  REQUESTMETHODS(Musical_span_req);
  
};

/** Start / stop a beam at this note */
class Beam_req  : public Musical_span_req  {
public:
  REQUESTMETHODS(Beam_req);

  Beam_req();
};

/** 
 Start / stop an abbreviation beam at this note. 
 */
class Abbreviation_beam_req : public Musical_span_req  {
public:
  REQUESTMETHODS (Abbreviation_beam_req);

  Abbreviation_beam_req ();

  int type_i_;
};

/**
  Start a tie at this note, end it at the next
 */
class Tie_req : public Musical_req {
public:
  REQUESTMETHODS(Tie_req);
};

/// a slur
class Slur_req  : public Musical_span_req  {
public:
  REQUESTMETHODS(Slur_req);

};

/// a plet (bracket with) number
class Plet_req : public Musical_span_req  {
public:
  int plet_i_;

  REQUESTMETHODS(Plet_req);
  Plet_req ();
};

class Musical_script_req : public Musical_req,  public Script_req {
public:
  REQUESTMETHODS(Musical_script_req);
};


class Dynamic_req  : public virtual Musical_req  {
public:
  /**
    for absolute dynamics

    This sux. We'd want increasing numbers for FFF till PPP, but not 
    for FP, SF, SFZ (FP is *not* louder than FFF)
   */
  enum Loudness {
    FFF, FF, F, MF, MP, P, PP, PPP, FP, SF, SFZ
  };
  static String loudness_static_str (Loudness);
  REQUESTMETHODS(Dynamic_req);
};

class Absolute_dynamic_req  : public Dynamic_req  {
public:
  Loudness loudness_;
  virtual bool do_equal_b (Request*) const;
  String loudness_str () const;
  Absolute_dynamic_req();
  REQUESTMETHODS(Absolute_dynamic_req);
};

class Span_dynamic_req  : public Dynamic_req, public Musical_span_req  {
public:
  virtual bool do_equal_b (Request*) const;
  /// Grow or shrink the volume: 1=cresc, -1 = decresc 
  Direction dynamic_dir_;
  Span_dynamic_req();
  REQUESTMETHODS(Span_dynamic_req);
};

#endif // MUSICALREQUESTS_HH
