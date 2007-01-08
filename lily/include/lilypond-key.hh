/*
  lilypond-key.hh -- declare Lilypond_{grob, context}_key

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef LILYPOND_KEY_HH
#define LILYPOND_KEY_HH

#include "object-key.hh"
#include "lily-proto.hh"
#include "moment.hh"

class Lilypond_grob_key : public Object_key
{
  Object_key const *context_;
  Moment creation_moment_;
  string grob_name_;
  int disambiguation_count_;

public:
  Lilypond_grob_key (Object_key const *context,
		     Moment start,
		     string name, int);

  static Object_key *from_scheme (SCM);
protected:
  virtual int get_type () const;
  virtual void derived_mark () const;
  virtual int do_compare (Object_key const *a) const;
  virtual SCM as_scheme () const;
};

class Lilypond_context_key : public Object_key
{
  Object_key const *parent_context_;
  Moment start_moment_;
  string context_name_;
  string id_;
  int disambiguation_count_;

public:
  Lilypond_context_key (Object_key const *parent,
			Moment start,
			string type,
			string id,
 			int count);

  static Object_key *from_scheme (SCM);
protected:
  virtual int get_type () const;
  virtual int do_compare (Object_key const *a) const;
  virtual void derived_mark () const;
  virtual SCM as_scheme () const;
};

class Lilypond_general_key : public Object_key
{
  Object_key const *parent_;
  string name_;
  int disambiguation_count_;
public:
  Lilypond_general_key (Object_key const *parent, string name,
			int count);

  static Object_key *from_scheme (SCM);
protected:
  virtual int get_type () const;
  virtual int do_compare (Object_key const *a) const;
  virtual void derived_mark () const;
  virtual SCM as_scheme () const;
};

#endif /* LILYPOND_KEY_HH */

