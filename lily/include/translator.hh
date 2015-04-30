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
#include "input.hh"             // for error reporting
#include "smobs.hh"
#include "std-vector.hh"
#include "protected-scm.hh"

#define TRANSLATOR_DECLARATIONS_NO_LISTENER(NAME)                       \
  public:                                                               \
  NAME ();                                                              \
  VIRTUAL_COPY_CONSTRUCTOR (Translator, NAME);                          \
  static SCM static_description_;                                       \
  static Drul_array<vector<Acknowledge_information> > acknowledge_static_array_drul_; \
  virtual void fetch_precomputable_methods (Callback methods[]); \
  virtual SCM static_translator_description () const;                   \
  virtual SCM translator_description () const;                          \
  static Grob_info_callback static_get_acknowledger (SCM sym);          \
  static Grob_info_callback static_get_end_acknowledger(SCM);           \
  virtual Grob_info_callback get_acknowledger (SCM sym)                 \
  {                                                                     \
    return static_get_acknowledger (sym);                               \
  }                                                                     \
  virtual Grob_info_callback get_end_acknowledger (SCM sym)             \
  {                                                                     \
    return static_get_end_acknowledger (sym);                           \
  } \
  /* end #define */

/*
  Each translator class has a static alist of event class symbols
  mapping to callbacks that are called with a translator instance and
  a stream event when an event of the appropriate event class is
  announced in a context.
*/

#define TRANSLATOR_DECLARATIONS(NAME)                                   \
  TRANSLATOR_DECLARATIONS_NO_LISTENER(NAME)                             \
private:                                                                \
  static Protected_scm listener_list_;                                  \
public:                                                                 \
  virtual SCM get_listener_list () const                                \
  {                                                                     \
    return listener_list_;                                              \
  }                                                                     \
  /* end #define */

#define DECLARE_TRANSLATOR_LISTENER(m)                  \
public:                                                 \
inline void listen_ ## m (Stream_event *);              \
/* Should be private */                                 \
static void _internal_declare_ ## m ();

#define DECLARE_ACKNOWLEDGER(x) public : void acknowledge_ ## x (Grob_info); protected:
#define DECLARE_END_ACKNOWLEDGER(x) public : void acknowledge_end_ ## x (Grob_info); protected:

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
  // We don't make Grob_info_callback specific to Engraver since we
  // otherwise get into a circular mess with regard to the definitions
  // as the timing of Engraver is exercised from within Translator
  typedef void (Translator::*Grob_info_callback) (Grob_info);
  typedef void (Translator::*Callback) (void);
  int print_smob (SCM, scm_print_state *);
  SCM mark_smob ();
  static const char type_p_name_[];
  virtual ~Translator ();
private:
  void init ();

public:
  Context *context () const { return daddy_context_; }

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

  TRANSLATOR_DECLARATIONS (Translator);

protected:                      // should be private.
  Context *daddy_context_;
  void protect_event (SCM ev);
  friend class Callback_wrapper;
  virtual void derived_mark () const;
  static SCM event_class_symbol (const char *ev_class);
  SCM static_translator_description (const char *grobs,
                                     const char *desc,
                                     SCM listener_list,
                                     const char *read,
                                     const char *write) const;

  friend class Translator_group;
};

struct Acknowledge_information
{
  SCM symbol_;
  Translator::Grob_info_callback function_;

  Acknowledge_information ()
  {
    symbol_ = SCM_EOL;
    function_ = 0;
  }
};


void add_translator (Translator *trans);

Translator *get_translator (SCM s);
Moment get_event_length (Stream_event *s, Moment now);
Moment get_event_length (Stream_event *s);

/*
  This helper is only meaningful inside listen_* methods.
*/
extern bool internal_event_assignment (Stream_event **old_ev, Stream_event *new_ev, const char *function);
#define ASSIGN_EVENT_ONCE(o,n) internal_event_assignment (&o, n, __FUNCTION__)

#endif // TRANSLATOR_HH
