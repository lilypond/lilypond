#ifndef SYMBOL_HH
#define SYMBOL_HH

#include "string.hh"
#include "boxes.hh"
#include "lily-proto.hh"

struct Symbol {
    String tex;
    Box dim;

    Symbol (String, Box);
    Symbol();
    String str()const;		// for printing.
};

#endif
