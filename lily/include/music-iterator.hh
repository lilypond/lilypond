/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "diagnostics.hh"
#include "moment.hh"
#include "virtual-methods.hh"
#include "context-handle.hh"

#include <functional>

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
  Music_iterator &operator= (Music_iterator const &) = delete;

public:
  const Moment &music_get_length () const { return music_length_; }
  // music_start_mom () is calculated relative to the time where the
  // iterator occurs in the music stream, so it will usually be
  // non-zero only for expressions starting with grace notes.
  const Moment &music_start_mom () const { return start_mom_; }
  void report_event (Music *);

  // N.B. Subclasses can modify the behavior of these to address a child
  // iterator.  Where it is important to avoid that, use get_own_context () and
  // set_own_context () instead.
  virtual Context *get_context () const { return get_own_context (); }
  virtual void set_context (Context *c) { set_own_context (c); }

  // Circumvent the virtual get_context () and set_context () to address the
  // context of this very iterator.
  Context *get_own_context () const { return handle_.get (); }
  void set_own_context (Context *c) { handle_ = c; }

  // Replace this iterator's references to one context with another.  This is
  // not recursive.  Subclasses that track multiple contexts should override
  // this to consider them all.
  virtual void substitute_context (Context *from, Context *to);

  // Create an iterator that has no parent.
  static SCM create_top_iterator (Music *mus)
  {
    return create_iterator (nullptr, mus);
  }

  // Create an iterator that has this iterator as its parent.
  SCM create_child (Music *mus) { return create_iterator (this, mus); }

  void init_context (Context *);
  void quit ();

  virtual Moment pending_moment () const;
  bool ok () const
  {
    return (pending_moment () < Moment::infinity ()) || run_always ();
  }
  virtual bool run_always () const;
  // process is called with a time relative to the iterator start, so
  // usually the last processed moment is the same as music_get_length.
  virtual void process (Moment until);
  virtual void derived_mark () const;
  DECLARE_SCHEME_CALLBACK (constructor, ());

  Music *get_music () const;

  // Call the given function on this iterator and then on all of its children.
  //
  // Subclasses with child iterators must override this method, call the
  // inherited implementation first, and then call preorder_walk for each
  // child.
  virtual void
  preorder_walk (const std::function<void (Music_iterator *)> &visit)
  {
    if (visit)
      visit (this);
  }

  Music_iterator *get_parent () const { return parent_; }

  // Scoped search for music by type: look in this iterator's music, then
  // search the ancestors up to the top.  Return the first iterator whose music
  // has the given type (null if none does).
  Music_iterator *find_above_by_music_type (SCM type_sym) const;

  // Scoped property access: look in this iterator's music, then search the
  // ancestors up to the top.
  SCM internal_get_property (SCM name_sym) const
  {
    SCM value = SCM_EOL;
    internal_where_defined (name_sym, &value);
    return value;
  }

  // Scoped property access: look in this iterator's music, then search the
  // ancestors up to the top.  Return the iterator whose music has the named
  // property (null if none does).  Optionally get the value of the property.
  Music_iterator *internal_where_defined (SCM name_sym, SCM *value) const;
  Music_iterator *internal_where_defined (SCM name_sym) const
  {
    SCM value;
    return internal_where_defined (name_sym, &value);
  }

  // Scoped search for taggged music: look in this iterator's music, then
  // search the ancestors up to the top.  Return the first iterator whose music
  // is tagged with the given tag.  If none is found, return this iterator.
  Music_iterator *where_tagged (SCM tag_sym) const;

protected:
  Music_iterator ();

  // During the staged initialization of this iterator, this method is called
  // after the iterator has received its music, but before it receives its
  // context.  This is the first chance to initialize state that depends on
  // music properties, and the last chance to initialize state required for
  // timing.  When this method is overridden, it is usually because the
  // iterator manages child iterators, which must be created to contribute to
  // timing calculations.
  virtual void create_children () {}

  // During the staged initialization of this iterator, this method is called
  // after the iterator's own context has been initialized.  It must initialize
  // the contexts of any child iterators.
  //
  // Think twice before reading context properties here.  Other iterators'
  // process () methods may run after this call and before this iterator's
  // process () method runs (and may change the context properties).
  virtual void create_contexts () {}

  void descend_to_bottom_context ();
  void descend_to_child (Context *);
  void descend_to_user_accessible_context ();

  virtual void do_quit ();

private:
  static SCM create_iterator (Music_iterator *parent, Music *mus);

private:
  Music_iterator *parent_ = nullptr;
  Context_handle handle_;
  Music *music_ = nullptr;
  Moment music_length_;
  Moment start_mom_;
};

bool is_child_context (Context *me, Context *child);

#define IMPLEMENT_CTOR_CALLBACK(Class)                                         \
  LY_DEFINE_MEMBER_FUNCTION (                                                  \
    Class, constructor,                                                        \
    mangle_cxx_identifier (#Class "::constructor").c_str (), 0, 0, 0, (), "")  \
  {                                                                            \
    Class *c = (new Class);                                                    \
    return c->unprotect ();                                                    \
  }

#endif // MUSIC_ITERATOR_HH
