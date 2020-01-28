/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2020 Erik Sandberg <mandolaerik@gmail.com>

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

#ifndef LISTENER_HH
#define LISTENER_HH

/*
  Listeners

  Listeners are used for stream event dispatching.  The usual way to
  work with them is to use the GET_LISTENER macro which combines the
  basic Listener algorithm with a Callback_wrapper structure providing
  a Scheme handle into a member function.

  To register a member function of Foo as an event handler in a
  dispatcher, you must:

  - declare the function:
  class Foo
  {
    void method (SCM);
    ...
  };

  - implement the method::
  void Foo::method (SCM e)
  {
    write ("Foo hears an event!");
  }

  - Extract a listener using GET_LISTENER (Foo, method)
  - Register the method to the dispatcher using Dispatcher::register

  Example:

  Foo *foo = (...);
  Dispatcher *d = (...);
  Listener l = foo->GET_LISTENER (Foo, method);
  d->add_listener (l, ly_symbol2scm ("EventClass"));

  Whenever d hears a stream-event ev of class "EventClass",
  the implemented procedure is called.

  GET_LISTENER actually makes use of a member function
  get_listener (SCM) available in every Smob<...>-derived class.
  get_listener receives a function getting an object instance and an
  event and will turn it into a Listener that will (after turning into
  Scheme), behave as a function receiving an event as its sole
  argument, with the object instance being the object from which
  get_listener was called as a member.

  So (p->get_listener (f)).smobbed_copy () is roughly equivalent to
  (lambda (ev) (f p->self_scm() ev))

  Limitations:

  The Callback_wrapper mechanism used in GET_LISTENER works only for
  classes derived from Smob<...>.
*/

#include "callback.hh"
#include "smobs.hh"

// A listener is essentially any procedure accepting a single argument
// (namely an event).  The class Listener (or rather a smobbed_copy of
// it) behaves like such a procedure and is composed of a generic
// callback function accepting two arguments, namely a "target"
// (usually an engraver instance) and the event.  Its Scheme
// equivalent would be
//
// (define (make-listener callback target)
//   (lambda (event) (callback target event)))
//
// The class construction is lightweight: as a Simple_smob, this is
// only converted into Scheme when a smobbed_copy is created.

class Listener : public Simple_smob<Listener>
{
private:
  SCM callback_;
  SCM target_;

public:
  static const char *const type_p_name_;

  Listener (SCM callback, SCM target) : callback_ (callback), target_ (target)
  {
  }

  LY_DECLARE_SMOB_PROC (&Listener::listen, 1, 0, 0)
  SCM listen (SCM ev)
  {
    scm_call_2 (callback_, target_, ev);
    return SCM_UNSPECIFIED;
  }

  SCM mark_smob () const
  {
    scm_gc_mark (callback_);
    return target_;
  }

  bool operator== (Listener const &other) const
  {
    return scm_is_eq (callback_, other.callback_)
           && scm_is_eq (target_, other.target_);
  }

  static SCM equal_p (SCM a, SCM b)
  {
    return *unchecked_unsmob (a) == *unchecked_unsmob (b) ? SCM_BOOL_T
                                                          : SCM_BOOL_F;
  }

  template <class T, void (T::*callback) (SCM)>
  static SCM trampoline (SCM target, SCM ev)
  {
    T *t = unsmob<T> (target);
    LY_ASSERT_SMOB (T, target, 1);

    (t->*callback) (ev);
    return SCM_UNDEFINED;
  }
};

#define GET_LISTENER(cl, proc)                                                 \
  get_listener (                                                               \
      Callback_wrapper::make_smob<Listener::trampoline<cl, &cl::proc>> ())

#endif /* LISTENER_HH */
