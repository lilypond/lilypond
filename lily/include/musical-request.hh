/*
  musical-request.hh -- declare Musical requests

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef MUSICALREQUESTS_HH
#define MUSICALREQUESTS_HH

#include "lily-proto.hh"
#include "request.hh"
#include "duration.hh"
#include "pitch.hh"
#include "array.hh"

/** a request with a duration.
  This request is used only used as a base class.
 */
class Rhythmic_req  : public virtual Request  {
public:
  void compress (Moment);
  virtual Moment length_mom () const;
  static int compare (Rhythmic_req const&,Rhythmic_req const&);
  VIRTUAL_COPY_CONS (Music);
};



struct Tremolo_req : public Request {
  VIRTUAL_COPY_CONS (Music);
  Tremolo_req ();
};

struct Chord_tremolo_notify_req : public Request
{
  Rational factor_;
  VIRTUAL_COPY_CONS(Chord_tremolo_notify_req);
  Chord_tremolo_notify_req();
};


/**
   a syllable or lyric is a string with rhythm.
  */
class Lyric_req : public Rhythmic_req
{
protected:
  VIRTUAL_COPY_CONS (Music);
};


class Articulation_req : public Script_req
{
public:
  String get_articulation_string ();
protected:
  virtual bool do_equal_b (Request const*) const;
  VIRTUAL_COPY_CONS (Music);
};

class Text_script_req : public Script_req
{
protected:
  VIRTUAL_COPY_CONS (Music);
  virtual bool do_equal_b (Request const*) const;
};

/// request which has some kind of pitch
struct Melodic_req :virtual Request
{
  static int compare (Melodic_req const&,Melodic_req const&);
  
protected:
  virtual bool do_equal_b (Request const*) const;

  VIRTUAL_COPY_CONS (Music);
};



#endif // MUSICALREQUESTS_HH
