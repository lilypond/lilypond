/*
  keyword.hh -- part of GNU LilyPond

  (c) 1996--2002 Han-Wen Nienhuys
*/

#ifndef KEYWORD_HH
#define KEYWORD_HH

#include "array.hh"

/* for the keyword table */
struct Keyword_ent
{
  char const *name_;
  int     tokcode_;
};

/*
  junkme, use  hash table.
 */
struct Keyword_table
{
  Array<Keyword_ent> table_;

  Keyword_table (Keyword_ent *);
  int     lookup (char const *s) const;
};


#endif // KEYWORD_HH

