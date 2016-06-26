/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2015 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef TRANSLATOR_HH
#define TRANSLATOR_HH

#include "global-ctor.hh"
#include "lily-proto.hh"
#include "virtual-methods.hh"
#include "callback.hh"
#include "input.hh"             // for error reporting
#include "smobs.hh"
#include "stream-event.hh"
#include "std-vector.hh"
#include "protected-scm.hh"

#define TRANSLATOR_FAMILY_DECLARATIONS(NAME)                            \
  public:                                                               \
  VIRTUAL_COPY_CONSTRUCTOR (Translator, NAME);                          \
  virtual void fetch_precomputable_methods (SCM methods[]);             \
  DECLARE_TRANSLATOR_CALLBACKS (NAME);                                  \
  TRANSLATOR_INHERIT (Translator);                                      \
  /* end #define */

#define TRANSLATOR_INHERIT(BASE)                                        \
  using BASE::method_finder

#define DECLARE_TRANSLATOR_CALLBACKS(NAME)                              \
  template <void (NAME::*mf)()>                                         \
  static SCM method_finder ()                                           \
  {                                                                     \
    return Callback0_wrapper::make_smob<NAME, mf> ();                   \
  }                                                                     \
  template <void (NAME::*mf)(Stream_event *)>                           \
  static SCM method_finder ()                                           \
  {                                                                     \
    return Callback_wrapper::make_smob<trampoline<NAME, mf> > ();       \
  }                                                                     \
  template <void (NAME::*mf)(Grob_info)>                                \
  static SCM method_finder () {                                         \
    return Callback2_wrapper::make_smob<trampoline <NAME, mf> > ();     \
  }                                                                     \
  /* end #define */

/*
  Each translator class has a static alist of event class symbols
  mapping to callbacks that are called with a translator instance and
  a stream event when an event of the appropriate event class is
  announced in a context.
*/

#define TRANSLATOR_DECLARATIONS(NAME)                                   \
  public:                                                               \
  TRANSLATOR_FAMILY_DECLARATIONS (NAME);                                \
  static Drul_array<Protected_scm> acknowledge_static_array_drul_;      \
  static SCM static_description_;                                       \
  static Protected_scm listener_list_;                                  \
  static SCM static_get_acknowledger (SCM sym, Direction start_end);    \
  virtual SCM get_acknowledger (SCM sym, Direction start_end)           \
  {                                                                     \
    return static_get_acknowledger (sym, start_end);                    \
  }                                                                     \
public:                                                                 \
  NAME ();                                                              \
  static void boot ();                                                  \
  virtual SCM static_translator_description () const;                   \
  virtual SCM translator_description () const;                          \
  virtual SCM get_listener_list () const                                \
  {                                                                     \
    return listener_list_;                                              \
  }                                                                     \
  /* end #define */

enum Translator_precompute_index
{
  START_TRANSLATION_TIMESTEP,
  STOP_TRANSLATION_TIMESTEP,
  PROCESS_MUSIC,
  PROCESS_ACKNOWLEDGED,
  TRANSLATOR_METHOD_PRECOMPUTE_COUNT,
};

/*
  Translate music into grobs.
*/
class Translator : public Smob<Translator>
{
public:
  int print_smob (SCM, scm_print_state *) const;
  SCM mark_smob () const;
  static const char * const type_p_name_;
  virtual ~Translator ();
private:
  void init ();

public:
  Context *context () const { return daddy_context_; }

  Translator ();
  Translator (Translator const &);

  SCM internal_get_property (SCM symbol) const;

  virtual Output_def *get_output_def () const;
  virtual Translator_group *get_daddy_translator ()const;
  virtual Moment now_mom () const;
  virtual bool must_be_last () const;

  virtual void initialize ();
  virtual void finalize ();

  virtual void connect_to_context (Context *c);
  virtual void disconnect_from_context (Context *c);

  void stop_translation_timestep ();
  void start_translation_timestep ();
  void process_music ();
  void process_acknowledged ();

  Context *get_score_context () const;
  Global_context *get_global_context () const;

  DECLARE_CLASSNAME (Translator);
  virtual Translator *clone () const = 0;
  virtual void fetch_precomputable_methods (SCM methods[]) = 0;
  virtual SCM get_listener_list () const = 0;
  virtual SCM translator_description () const = 0;
  virtual SCM get_acknowledger (SCM sym, Direction start_end) = 0;

protected:                      // should be private.
  Context *daddy_context_;
  void protect_event (SCM ev);

  template <class T, void (T::*callback)(Stream_event *)>
  static SCM trampoline (SCM target, SCM event)
  {
    T *t = unsmob<T> (target);
    LY_ASSERT_SMOB (T, target, 1);
    LY_ASSERT_SMOB (Stream_event, event, 2);

    t->protect_event (event);
    (t->*callback) (unsmob<Stream_event> (event));
    return SCM_UNSPECIFIED;
  }

  // Fallback for non-overriden callbacks for which &T::x degrades to
  // &Translator::x
  template <void (Translator::*)()>
  static SCM
  method_finder () { return SCM_UNDEFINED; }

  virtual void derived_mark () const;
  static SCM event_class_symbol (const char *ev_class);
  SCM static_translator_description (const char *grobs,
                                     const char *desc,
                                     SCM listener_list,
                                     const char *read,
                                     const char *write) const;

  friend class Translator_group;
};

void add_translator (Translator *trans);

Translator *get_translator (SCM s);

SCM
generic_get_acknowledger (SCM sym, SCM ack_hash);

Moment get_event_length (Stream_event *s, Moment now);
Moment get_event_length (Stream_event *s);

/*
  This helper is only meaningful inside listen_* methods.
*/
extern bool internal_event_assignment (Stream_event **old_ev, Stream_event *new_ev, const char *function);
#define ASSIGN_EVENT_ONCE(o,n) internal_event_assignment (&o, n, __FUNCTION__)

#endif // TRANSLATOR_HH
