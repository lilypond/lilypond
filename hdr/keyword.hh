/*
  keyword.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef KEYWORD_HH
#define KEYWORD_HH

/* for the keyword table */
struct Keyword_ent
{
    const char   *name;
    int     tokcode;
};

struct Keyword_table
{
    Keyword_ent *table;
    int     maxkey;
    Keyword_table(Keyword_ent *);
    int     lookup(const char *s) const;
};


#endif // KEYWORD_HH

