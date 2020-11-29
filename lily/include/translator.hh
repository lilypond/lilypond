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

#ifndef TRANSLATOR_HH
#define TRANSLATOR_HH

#include "global-ctor.hh"
#include "lily-proto.hh"
#include "virtual-methods.hh"
#include "callback.hh"
#include "input.hh"             // for error reporting
#include "smobs.hh"
#include "stream-event.hh"
#include "protected-scm.hh"

// The Translator_creator class is only for translators defined in C.
// Its elements are callable entities taking a context argument and
// returning a corresponding translator.
//
// Other translator-creating entities may be alists and functions returning
// such alists.  Information for those, such as created grobs/properties
// is attached via object properties.

// Smob rather than Simple_smob since we want an entity for
// property lookup.

class Translator_creator : public Smob<Translator_creator>
{
  Translator_creator (Translator_creator const &); // don't define
  Translator *(*allocate_) (Context *);
  template <class T>
  static Translator *allocate (Context *ctx);

  Translator_creator (Translator * (*allocate) (Context *))
    : allocate_ (allocate)
  {
    smobify_self ();
  }
public:
  // This is stupid, but constructors cannot have explicit template
  // argument lists.
  template <class T>
  static Translator_creator *alloc ()
  {
    return new Translator_creator (&allocate<T>);
  }
  SCM call (SCM ctx);
  LY_DECLARE_SMOB_PROC (&Translator_creator::call, 1, 0, 0);
};

template <class T> Translator *
Translator_creator::allocate (Context *ctx)
{
  return new T (ctx);
}

#define TRANSLATOR_FAMILY_DECLARATIONS(NAME)                            \
  public:                                                               \
  OVERRIDE_CLASS_NAME (NAME);                                           \
  void fetch_precomputable_methods (SCM methods[]) override;            \
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
  static Protected_scm listener_list_;                                  \
  static SCM static_get_acknowledger (SCM sym, Direction start_end);    \
  SCM get_acknowledger (SCM sym, Direction start_end) override          \
  {                                                                     \
    return static_get_acknowledger (sym, start_end);                    \
  }                                                                     \
public:                                                                 \
  NAME (Context *);                                                     \
  static void boot ();                                                  \
  static SCM static_translator_description ();                          \
  SCM get_listener_list () const override                               \
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
  static const char *const type_p_name_;
  virtual ~Translator ();

  Context *context () const { return context_; }

protected:
  Translator (Context *);

  Global_context *find_global_context () const;
  Context *find_score_context () const;

private:
  Translator (Translator const &) = delete;
  Translator &operator = (Translator const &) = delete;
public:

  SCM internal_get_property (SCM symbol) const;

  virtual Output_def *get_output_def () const;
  Translator_group *get_group ()const;
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

  VIRTUAL_CLASS_NAME (Translator);

  virtual void fetch_precomputable_methods (SCM methods[]) = 0;
  virtual SCM get_listener_list () const = 0;
  virtual SCM get_acknowledger (SCM sym, Direction start_end) = 0;

private:
  Context *context_;

protected:
  void protect_event (SCM ev);
  template <class T, void (T::*callback) (Stream_event *)>
  friend SCM Callbacks::trampoline (SCM, SCM);

  virtual void derived_mark () const;
  static SCM event_class_symbol (const char *ev_class);
  static SCM
  static_translator_description (const char *grobs,
                                 const char *desc,
                                 SCM listener_list,
                                 const char *read,
                                 const char *write);

  friend class Translator_group;
};

template <class T, void (T::*callback) (Stream_event *)>
SCM Callbacks::trampoline (SCM target, SCM event)
{
  T *t = LY_ASSERT_SMOB (T, target, 1);
  Stream_event *ev = LY_ASSERT_SMOB (Stream_event, event, 2);

  t->protect_event (event);
  (t->*callback) (ev);
  return SCM_UNSPECIFIED;
}

SCM
generic_get_acknowledger (SCM sym, SCM ack_hash);

void add_translator_creator (SCM creator, SCM name, SCM description);

SCM get_translator_creator (SCM s);
Moment get_event_length (Stream_event *s, Moment now);
Moment get_event_length (Stream_event *s);

#endif // TRANSLATOR_HH
