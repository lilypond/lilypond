#ifndef LGETOPT_HH
#define LGETOPT_HH

#include <string.h>


class ostream;

/**
  a struct this for initialising the commandline options.
 */
struct Long_option_init {
    bool take_arg;
    char const * longname;
    char        shortname;

    ostream &printon(ostream &errorout);
};


/** C++ for version of long_getopt.  For processing GNU style command
  line arguments.  No pointer (return values, arguments) contents are
  copied.  */
class Getopt_long {
public:
    /** errorcodes: no error, argument expected, no argument expected,
      unknown option, illegal argument (eg. int expected).  */
    enum Errorcod { E_NOERROR = 0, E_ARGEXPECT, E_NOARGEXPECT, E_UNKNOWNOPTION,
		E_ILLEGALARG } ;


private:

    /// the option info.
    Long_option_init *the_opts;
    int table_len;
    
    /// if doing short option, argv[optind][optindind] is processed next.
    int optindind;

    /// the option found
    Long_option_init *beet;

    /// get ready for processing next error.
    bool next();
    Long_option_init *parselong();
    Long_option_init *parseshort();
    
    ostream *errorout;

    /// report an error and abort
    void report(Errorcod c);
public:

    /// argument. Set to 0 if not present
    char* optarg;

    /// current error status
    Errorcod error;

    /// return an integer (with err. detect)
    long intarg();
    /// argv[optind] will be processed next.
    int optind;

    /// the arguments
    char **argv;

    /// the arg. count
    int argc;
    
    /* *************** */
    
    /**
      What to do with  errors. 
       report messages on  #*os#, and abort.
       if #os# is null, then do not report nor abort, just set #error#
      */
       
    void seterror(ostream *os);

    /// construct: pass arguments and option info.
    Getopt_long(int c,  char **v, Long_option_init *lo);

    /**  get the next option. 
      @return pointer to next option found.
      0 if error occurred, or next argument is no option.
      */
    Long_option_init *operator()();

    char *current_arg();
    char * get_next_arg();
};
#endif
