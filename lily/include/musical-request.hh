/*
  musical-request.hh -- declare Musical requests

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  bool do_equal_b (Request const*) const;
  void compress (Moment);
  virtual Moment length_mom () const;
  static int compare (Rhythmic_req const&,Rhythmic_req const&);
  VIRTUAL_COPY_CONS(Music);
};

class Skip_req  : public Rhythmic_req  {
public:
  VIRTUAL_COPY_CONS(Music);
};


struct Tremolo_req : public Request {
  VIRTUAL_COPY_CONS (Music);
  Tremolo_req ();

  void set_type (int);
  int get_type () const;
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
  String get_articulation_str();
protected:
  virtual bool do_equal_b (Request const*) const;

  VIRTUAL_COPY_CONS(Music);
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
  /// transpose. #delta# is relative to central c.
  virtual void transpose (Pitch delta);
  virtual bool do_equal_b (Request const*) const;

  VIRTUAL_COPY_CONS(Music);
};

/*
   Put a note of specified type, height, and with accidental on the staff.
    /// force/supress printing of accidental.
  bool forceacc_b_;
  /// Cautionary, i.e. parenthesized accidental.
  bool cautionary_b_;

 */
class Note_req  : public Rhythmic_req, virtual public Melodic_req  {
public:
    
  Note_req();
protected:

  bool do_equal_b (Request const*) const;
  VIRTUAL_COPY_CONS(Music);
};

/**
Put a rest on the staff. Why a request? It might be a good idea to not typeset the rest, if the paper is too crowded.
*/
class Rest_req : public Rhythmic_req {
public:
  VIRTUAL_COPY_CONS(Music);
};


/// an extender line
class Extender_req : public Request  {
public:
  VIRTUAL_COPY_CONS(Music);
};

/// a centred hyphen
class Hyphen_req : public Request  {
public:
  VIRTUAL_COPY_CONS(Music);
};

/** is anyone  playing a note?
    Used for communication between Music & Lyrics
 */
class Busy_playing_req : public Request
{
public:
  VIRTUAL_COPY_CONS (Music);
};



/**
   instruct lyric context to alter typesetting (unimplemented).  */
class Melisma_req : public Span_req
{
public:
  VIRTUAL_COPY_CONS(Music);
};


/**
   Helping req to signal start of a melisma from within a context, and
   to   */
class Melisma_playing_req : public Request
{
public:
  VIRTUAL_COPY_CONS (Music);
};

class Arpeggio_req : public Request
{
public:
  VIRTUAL_COPY_CONS (Music);
};

class Glissando_req : public Request
{
  VIRTUAL_COPY_CONS (Music);
};

#endif // MUSICALREQUESTS_HH
