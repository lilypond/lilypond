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

#include "context-handle.hh"
#include "moment.hh"
#include "std-vector.hh"
#include "virtual-methods.hh"

/**
   ---

   Music_iterator is an object type that traverses the Music structure and
   reports the events it finds to interpretation contexts. It is not yet
   user-serviceable.


   ---

   Conceptually a music-iterator operates on a queue of musical events
   that are pending. This queue does not actually exist, but it is a
   way of viewing and traversing music-expressions.


   ok () -- events left ?

   pending_mom () -- time tag of the next event to be processed.
   PRECONDITION: ok () holds.

   process (M) -- process all at M (Precondition: no events exist
   before M, ok () holds).  Side-effects:

   * This removes all events at M from the pending queue.

   * Typically this reports the music to an interpretation context,
   thus changing the state of the interpretation context.


   TODO:

   merge pending_moment and process?
*/
class Music_iterator : public Smob<Music_iterator>
{
public:
  int print_smob (SCM, scm_print_state *) const;
  SCM mark_smob () const;
  static const char *const type_p_name_;
  virtual ~Music_iterator ();

protected:
  Moment music_length_;
  Moment start_mom_;

  VIRTUAL_CLASS_NAME (Music_iterator);

private:
  Music_iterator (Music_iterator const &) = delete;
  Music_iterator &operator= (Music_iterator const &) = delete;

public:
  Moment music_get_length () const;
  // music_start_mom () is calculated relative to the time where the
  // iterator occurs in the music stream, so it will usually be
  // non-zero only for expressions starting with grace notes.
  Moment music_start_mom () const;
  Music_iterator ();
  void report_event (Music *);
  virtual Context *get_outlet () const;
  virtual void set_context (Context *);
  static SCM get_static_get_iterator (Music *mus);
  void init_context (Music *, Context *);
  void quit ();
  void substitute_outlet (Context *from, Context *to);
  void descend_to_bottom_context ();
  virtual void derived_substitute (Context *, Context *);
  virtual Moment pending_moment () const;
  virtual bool ok () const;
  virtual bool run_always () const;
  // process is called with a time relative to the iterator start, so
  // usually the last processed moment is the same as music_get_length.
  virtual void process (Moment until);
  virtual void derived_mark () const;
  virtual void construct_children ();
  DECLARE_SCHEME_CALLBACK (constructor, ());
  SCM get_iterator (Music *) const;

  Music *get_music () const;

protected:
  virtual void do_quit ();
  void descend_to_child (Context *);

private:
  Context_handle handle_;
  Music *music_;
};

bool is_child_context (Context *me, Context *child);

#define IMPLEMENT_CTOR_CALLBACK(Class)                                         \
  LY_DEFINE_MEMBER_FUNCTION (                                                  \
      Class, constructor,                                                      \
      mangle_cxx_identifier (std::string (#Class) + "::constructor").c_str (), \
      0, 0, 0, (), "")                                                         \
  {                                                                            \
    Class *c = (new Class);                                                    \
    return c->unprotect ();                                                    \
  }

#endif // MUSIC_ITERATOR_HH
