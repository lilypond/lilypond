/*
  musical-request.hh -- declare Musical requests

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef MUSICALREQUESTS_HH
#define MUSICALREQUESTS_HH

#include "lily-proto.hh"
#include "request.hh"
#include "duration.hh"
#include "musical-pitch.hh"


/** a request with a duration.
  This request is used only used as a base class.
 */
class Rhythmic_req  : public virtual Request  {
public:
  Duration duration_;
  virtual void do_print () const;


  bool do_equal_b (Request*) const;
  void compress (Moment);
  virtual Moment length_mom () const;
  static int compare (Rhythmic_req const&,Rhythmic_req const&);
  VIRTUAL_COPY_CONS(Music);
};

class Skip_req  : public Rhythmic_req  {
public:
  VIRTUAL_COPY_CONS(Music);
};


struct Abbreviation_req : public Request {
  VIRTUAL_COPY_CONS (Abbreviation_req);
  Abbreviation_req ();
  int type_i_;
  virtual void do_print () const;
};


/** a syllable  or lyric is a string with rhythm.
  */
class Lyric_req  : public  Rhythmic_req  {
public:
  virtual void do_print () const;
  String text_str_;
  VIRTUAL_COPY_CONS(Music);
};


class Articulation_req : public G_script_req
{
public:
  String articulation_str_;
protected:
  virtual bool do_equal_b (Request*) const;
  virtual void do_print () const;
  VIRTUAL_COPY_CONS(Music);
};

class Text_script_req : public G_script_req {
public:
  String text_str_;

  // should be generic property of some kind.. 
  String style_str_;
protected:
  VIRTUAL_COPY_CONS(Music);
  virtual void do_print () const;
};


/// request which has some kind of pitch
struct Melodic_req :virtual Request
{
  Musical_pitch pitch_;

  static int compare (Melodic_req const&,Melodic_req const&);
  
protected:
  /// transpose. #delta# is relative to central c.
  virtual void transpose (Musical_pitch delta);
  virtual bool do_equal_b (Request*) const;
  virtual void do_print () const;
  VIRTUAL_COPY_CONS(Music);
};

/// specify tonic of a chord
struct Tonic_req : public Melodic_req
{
  VIRTUAL_COPY_CONS(Music);
};

/// Put a note of specified type, height, and with accidental on the staff.
class Note_req  : public Rhythmic_req, virtual public Melodic_req  {
public:
    
  /// force/supress printing of accidental.
  bool forceacc_b_;
  /// Cautionary, i.e. parenthesized accidental.
  bool cautionary_b_;
  Note_req();
protected:
  virtual void do_print () const;
  bool do_equal_b (Request*) const;
  VIRTUAL_COPY_CONS(Music);
};

/**
Put a rest on the staff. Why a request? It might be a good idea to not typeset the rest, if the paper is too crowded.
*/
class Rest_req : public Rhythmic_req {
public:
  VIRTUAL_COPY_CONS(Music);
};

/**
 Part: typeset a measure with the number of measures rest
 Score: typeset all individual measures ass full rests
 */
class Multi_measure_rest_req : public Rhythmic_req  {
public:
  VIRTUAL_COPY_CONS(Music);

};


/** 
 Start / stop an abbreviation beam at this note. 
 */
class Abbreviation_beam_req : public Span_req  {
public:
  VIRTUAL_COPY_CONS(Abbreviation_beam_req);

  Abbreviation_beam_req ();

  int type_i_;
};



/// a slur
class Slur_req  : public Span_req  {
public:
  VIRTUAL_COPY_CONS(Music);

};

/// an extender line
class Extender_req : public Request  {
public:
  VIRTUAL_COPY_CONS(Music);

};


class Dynamic_req  : public virtual Request  {
public:
  VIRTUAL_COPY_CONS(Music);
};
/*
   merge with Articulation_req? 
 */
class Absolute_dynamic_req  : public Dynamic_req  {
public:
  String loudness_str_;
  Absolute_dynamic_req();

protected:
  virtual void do_print () const;
  virtual bool do_equal_b (Request*) const;
  VIRTUAL_COPY_CONS(Music);
};

class Span_dynamic_req  : public Dynamic_req, public Span_req  {
public:

  /// Grow or shrink the volume: 1=cresc, -1 = decresc 
  Direction dynamic_dir_;

  Span_dynamic_req();
protected:
  virtual bool do_equal_b (Request*) const;
  virtual void do_print () const;
  VIRTUAL_COPY_CONS(Music);
};

#endif // MUSICALREQUESTS_HH
