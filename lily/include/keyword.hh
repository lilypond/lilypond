/*
  keyword.hh -- part of GNU LilyPond

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef KEYWORD_HH
#define KEYWORD_HH

/* for the keyword table */
struct Keyword_ent
{
  char const *name;
  int     tokcode;
};

struct Keyword_table
{
  Keyword_ent *table;
  int     maxkey;
  Keyword_table (Keyword_ent *);
  int     lookup (char const *s) const;
};


#endif // KEYWORD_HH

