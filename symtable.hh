struct  Symtable : public Assoc<String, Symbol> {
    const Symbol * lookup(String)const;
};


struct Symtables : private Assoc<String, Symtable*> {
    String fname;
    bool done_reading;
    Symtables(String s) : fname (s) {
	done_reading = false;
    }
    void read() ;
    Symtable* operator()(String s);

};

