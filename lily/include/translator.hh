/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "input.hh"		// for error reporting
#include "smobs.hh"
#include "std-vector.hh"
#include "protected-scm.hh"

struct Acknowledge_information
{
  SCM symbol_;
  Engraver_void_function_engraver_grob_info function_;

  Acknowledge_information () {
    symbol_ = SCM_EOL;
    function_ = 0;
  }
};


/*
  Each translator class has a static list of listener records. Each
  record makes one explains how to register one of the class's stream event
  listeners to a context.
*/
typedef struct translator_listener_record {
  Listener (*get_listener_) (void *, SCM event_class);
  SCM event_class_;
  struct translator_listener_record *next_;

  translator_listener_record () {
    next_ = 0;
    event_class_ = SCM_EOL;
    get_listener_ = 0;
  }
    
} translator_listener_record;


#define TRANSLATOR_DECLARATIONS_NO_LISTENER(NAME)                       \
private:								\
  public:								\
  NAME ();								\
  VIRTUAL_COPY_CONSTRUCTOR (Translator, NAME);				\
  static SCM static_description_;					\
  static Drul_array<vector<Acknowledge_information> > acknowledge_static_array_drul_; \
  virtual void fetch_precomputable_methods (Translator_void_method_ptr methods[]); \
  virtual SCM static_translator_description () const;			\
  virtual SCM translator_description () const;				\
  static Engraver_void_function_engraver_grob_info static_get_acknowledger (SCM sym); \
  static Engraver_void_function_engraver_grob_info static_get_end_acknowledger(SCM); \
  virtual Engraver_void_function_engraver_grob_info get_acknowledger (SCM sym) \
  {									\
    return static_get_acknowledger (sym);				\
  }									\
  virtual Engraver_void_function_engraver_grob_info get_end_acknowledger (SCM sym) \
  {									\
    return static_get_end_acknowledger (sym);				\
  } \
  /* end #define */

#define TRANSLATOR_DECLARATIONS(NAME)					\
  TRANSLATOR_DECLARATIONS_NO_LISTENER(NAME)				\
private:								\
  static translator_listener_record *listener_list_;			\
public:									\
  virtual translator_listener_record *get_listener_list () const	\
  {									\
    return listener_list_;						\
  }									\
  /* end #define */

#define DECLARE_TRANSLATOR_LISTENER(m)			\
public:							\
inline void listen_ ## m (Stream_event *);		\
/* Should be private */					\
static void _internal_declare_ ## m ();			\
private:						\
 static Listener _get_ ## m ## _listener (void *, SCM);	\
DECLARE_LISTENER (_listen_scm_ ## m);

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
class Translator
{
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

  /* should maybe be virtual */
  void connect_to_context (Context *c);
  void disconnect_from_context (Context *c);

  void stop_translation_timestep ();
  void start_translation_timestep ();
  void process_music ();
  void process_acknowledged ();

  Context *get_score_context () const;
  Global_context *get_global_context () const;

  TRANSLATOR_DECLARATIONS (Translator);
  DECLARE_SMOBS (Translator);

protected:			// should be private.
  Context *daddy_context_;
  void protect_event (SCM ev);
  virtual void derived_mark () const;
  static void add_translator_listener (translator_listener_record **listener_list,
				       translator_listener_record *r,
				       Listener (*get_listener) (void *, SCM),
				       const char *ev_class);
  SCM static_translator_description (const char *grobs, 
				     const char *desc,
				     translator_listener_record *listener_list,
				     const char *read, 
				     const char *write) const;

  friend class Translator_group;
};

void add_translator (Translator *trans);

Translator *get_translator (SCM s);
Moment get_event_length (Stream_event *s, Moment now);
Moment get_event_length (Stream_event *s);
DECLARE_UNSMOB (Translator, translator);


/*
  This helper is only meaningful inside listen_* methods.
*/
extern bool internal_event_assignment (Stream_event **old_ev, Stream_event *new_ev, const char *function);
#define ASSIGN_EVENT_ONCE(o,n) internal_event_assignment (&o, n, __FUNCTION__)



#endif // TRANSLATOR_HH
