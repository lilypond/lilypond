/*
  object-key.hh -- declare Object_key

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef OBJECT_KEY_HH
#define OBJECT_KEY_HH

#include "smobs.hh"

/*
  Object_keys are read-only values, suitable for storing references to
  transient objects (such as grobs or contexts) on disk.

  In the future, they might also act as handles for external processes
  requesting notation to be drawn.
 */
class Object_key
{
  DECLARE_SMOBS(Object_key,);

protected:
  Object_key();
  virtual void derived_mark () const;
  virtual int get_type () const;
  virtual int do_compare (Object_key const * other) const;

public:
  int compare (Object_key const *other) const;
};

enum Object_key_type {
  GENERAL_KEY, 
  GROB_KEY,
  CONTEXT_KEY,
  COPIED_KEY,
};

class Copied_key : public Object_key
{

private:
  Object_key const * original_;
  int copy_count_;

protected:
  virtual void derived_mark () const;
  virtual int get_type () const;
  virtual int do_compare (Object_key const * other) const;

public:
  Copied_key (Object_key const*, int); 
};

DECLARE_UNSMOB(Object_key, key);

#endif /* OBJECT_KEY_HH */
