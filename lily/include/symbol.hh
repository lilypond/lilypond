/*
  symbol.hh -- declare Symbol, Atom

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

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
    String str() const;		// for printing.
};


/// a symbol which can be translated, and freely copied
struct Atom {
    Offset off_;
    Symbol sym_;

    /* *************** */
    
    void translate (Offset o) {
	off_ += o;
    }
    void translate (Real r,Axis a){
	off_[a] += r;
    }
    /// how big is #this#?
    Box extent() const;
    Atom (Symbol s);

    void print() const;

    String TeX_string() const;
};
#endif
