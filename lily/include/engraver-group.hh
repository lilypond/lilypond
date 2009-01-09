/*
  engraver-group.hh -- declare Engraver_group

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef ENGRAVER_GROUP_HH
#define ENGRAVER_GROUP_HH

#include "engraver.hh"
#include "translator-group.hh"

class Engraver_group : public virtual Translator_group
{
protected:
  vector<Grob_info> announce_infos_;
  Drul_array<SCM> acknowledge_hash_table_drul_;
  DECLARE_LISTENER (override);
  DECLARE_LISTENER (revert);
public:
  VIRTUAL_COPY_CONSTRUCTOR (Translator_group, Engraver_group);

  Engraver_group ();
  virtual void derived_mark () const;
  void do_announces ();
  virtual void connect_to_context (Context *c);
  virtual void disconnect_from_context ();
  virtual void announce_grob (Grob_info);
  int pending_grob_count () const;
private:
  virtual void acknowledge_grobs ();
};

typedef void (Engraver:: *Engraver_method) (void);

void engraver_each (SCM list, Engraver_method method);

#endif /* ENGRAVERGROUP_HH */


