/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef MUSIC_ITERATOR_HH
#define MUSIC_ITERATOR_HH

#include "std-vector.hh"
#include "diagnostics.hh"
#include "moment.hh"
#include "virtual-methods.hh"
#include "context-handle.hh"

/**
   ---

   Music_iterator is an object type that traverses the Music structure and
   reports the events it finds to interpretation contexts. It is not yet
   user-serviceable.


   ---

   Conceptually, a music-iterator traverses a queue of pending musical events.
   This way of viewing and traversing music-expressions does not require an
   actual queue.


   ok () -- processing is incomplete: the queue still holds events or the
   iterator wants to continue regardless

   pending_moment () -- the time of the next event in the queue (+infinity
   once the queue is empty)

   process (M) -- process all at M (Precondition: no events exist
   before M, ok () holds).  Side-effects:

   * This removes all events at M from the pending queue.

   * Typically this reports the music to an interpretation context,
   thus changing the state of the interpretation context.

   run_always () -- when true, process (M) should be called even if M is
   earlier than pending_moment (); when false, process (M) should not be
   called until M reaches pending_moment ()

   TODO:

   merge pending_moment and process?
*/
class Music_iterator : public Smob<Music_iterator>, public Diagnostics
{
public:
  int print_smob (SCM, scm_print_state *) const;
  SCM mark_smob () const;
  static const char *const type_p_name_;
  virtual ~Music_iterator ();
  Input *origin () const override;

  VIRTUAL_CLASS_NAME (Music_iterator);
private:
  Music_iterator (Music_iterator const &) = delete;
  Music_iterator &operator = (Music_iterator const &) = delete;

public:
  const Moment &music_get_length () const { return music_length_; }
  // music_start_mom () is calculated relative to the time where the
  // iterator occurs in the music stream, so it will usually be
  // non-zero only for expressions starting with grace notes.
  const Moment &music_start_mom () const { return start_mom_; }
  void report_event (Music *);
  virtual Context *get_outlet () const;
  virtual void set_context (Context *);
  static SCM get_static_get_iterator (Music *mus);
  void init_context (Context *);
  void quit ();
  void substitute_outlet (Context *from, Context *to);
  void descend_to_bottom_context ();
  virtual void derived_substitute (Context *, Context *);
  virtual Moment pending_moment () const;
  bool ok () const
  {
    return (pending_moment () < Moment (Rational::infinity ()))
           || run_always ();
  }
  virtual bool run_always () const;
  // process is called with a time relative to the iterator start, so
  // usually the last processed moment is the same as music_get_length.
  virtual void process (Moment until);
  virtual void derived_mark () const;
  virtual void construct_children () {}
  DECLARE_SCHEME_CALLBACK (constructor, ());
  SCM get_iterator (Music *) const;

  Music *get_music () const;
protected:
  Music_iterator ();
  virtual void do_quit ();
  void descend_to_child (Context *);

private:
  Context_handle handle_;
  Music *music_;
  Moment music_length_;
  Moment start_mom_;
};

bool is_child_context (Context *me, Context *child);

#define IMPLEMENT_CTOR_CALLBACK(Class)                                  \
  LY_DEFINE_MEMBER_FUNCTION (Class, constructor, \
                             mangle_cxx_identifier (std::string (#Class) + "::constructor").c_str(), \
                             0, 0, 0,                                   \
                             (),                                        \
                             "")                                        \
  {                                                                     \
    Class *c = (new Class);                                             \
    return c->unprotect ();                                             \
  }

#endif // MUSIC_ITERATOR_HH
