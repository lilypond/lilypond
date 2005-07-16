/*
  translator-group.hh -- declare Translator_group

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef TRANSLATOR_GROUP_HH
#define TRANSLATOR_GROUP_HH



#include "translator.hh"
#include "parray.hh"

typedef void (Translator:: *Translator_method) (void);

class Translator_group : public virtual Translator
{
public:
  VIRTUAL_COPY_CONSTRUCTOR (Translator, Translator_group);
  virtual Translator_group *get_daddy_translator ()const;
  virtual SCM get_simple_trans_list ();
  virtual bool try_music (Music *req);
  virtual void initialize ();
  Translator_group ();

protected:
  SCM simple_trans_list_;

  friend class Context_def;
  virtual void derived_mark () const;
};

SCM names_to_translators (SCM namelist, Context *tg);
void recurse_over_translators (Context *c, Translator_method ptr, Direction);
void translator_each (SCM list, Translator_method method);

#endif // TRANSLATOR_GROUP_HH
