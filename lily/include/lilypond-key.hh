/*
  lilypond-key.hh -- declare Lilypond_{grob,context}_key

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef LILYPOND_KEY_HH
#define LILYPOND_KEY_HH

#include "object-key.hh"
#include "lily-proto.hh"
#include "moment.hh"
#include "string.hh"

class Lilypond_grob_key : public Object_key
{
  Object_key const *context_;
  Moment creation_moment_;
  String grob_name_;
  int disambiguation_count_;

public:
  Lilypond_grob_key(Object_key const *context,
		    Moment start,
		    String name, int);

protected:
  virtual int get_type () const;
  virtual void derived_mark () const; 
  virtual int do_compare (Object_key const* a) const;
};

class Lilypond_context_key : public Object_key
{
  Object_key const *parent_context_;
  Moment start_moment_;
  String context_name_;
  String id_;
  int disambiguation_count_;

public:
  Lilypond_context_key (Object_key const * parent,
			Moment start,
			String type,
			String id,
  			int count);

protected:
  virtual int get_type () const;
  virtual int do_compare (Object_key const* a) const;
  virtual void derived_mark () const; 
};


#endif /* LILYPOND_KEY_HH */

