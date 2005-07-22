/*
  translator.hh -- declare Translator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef TRANSLATOR_HH
#define TRANSLATOR_HH

#include "global-ctor.hh"
#include "string.hh"
#include "lily-proto.hh"
#include "virtual-methods.hh"
#include "input.hh"
#include "smobs.hh"



typedef void (*Translator_void_method_ptr)(Translator*);


struct Acknowledge_information
{
  SCM symbol_;
  Translator_void_method_ptr function_;
};


#define TRANSLATOR_DECLARATIONS(NAME)			\
  public:						\
  NAME ();						\
  VIRTUAL_COPY_CONSTRUCTOR (Translator, NAME);		\
  static SCM static_description_;			\
  static Array<Acknowledge_information> acknowledge_static_array_; \
  virtual void fetch_precomputable_methods (Translator_void_method_ptr methods[]);\
  virtual SCM static_translator_description () const;	\
  virtual SCM translator_description () const; \
  virtual Translator_void_method_ptr get_acknowledger (SCM sym) { \
    return static_get_acknowledger (sym);\
  }\
  static Translator_void_method_ptr static_get_acknowledger (SCM sym);

enum Translator_precompute_index {
  START_TRANSLATION_TIMESTEP,
  STOP_TRANSLATION_TIMESTEP,
  PROCESS_MUSIC,
  PROCESS_ACKNOWLEDGED,
  TRANSLATOR_METHOD_PRECOMPUTE_COUNT,
};

/* nothing */
#define PRECOMPUTED_VIRTUAL 


/*
  Translate music into grobs.
*/
class Translator
{
  void init ();
  
protected:
  bool must_be_last_;

public:
  bool must_be_last () const;

  Context *context () const { return daddy_context_; }

  Translator (Translator const &);

  SCM internal_get_property (SCM symbol) const;

  virtual Output_def *get_output_def () const;
  virtual Translator_group *get_daddy_translator ()const;
  virtual Moment now_mom () const;
  
  virtual bool try_music (Music *req);
  virtual void initialize ();
  virtual void finalize ();

  PRECOMPUTED_VIRTUAL void stop_translation_timestep ();
  PRECOMPUTED_VIRTUAL void start_translation_timestep ();
  PRECOMPUTED_VIRTUAL void process_music ();
  PRECOMPUTED_VIRTUAL void process_acknowledged ();
  
  Score_context *get_score_context () const;
  Global_context *get_global_context () const;

  TRANSLATOR_DECLARATIONS (Translator);
  DECLARE_SMOBS (Translator, dummy);
protected:			// should be private.
  Context *daddy_context_;
  virtual void derived_mark () const;

  friend class Context_def;
  friend class Context;
};
void add_translator (Translator *trans);

Translator *get_translator (SCM s);
DECLARE_UNSMOB (Translator, translator);
#endif // TRANSLATOR_HH
