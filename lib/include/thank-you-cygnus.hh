//
// windhoos-suck-suck-suck-thank-you-cygnus.hh
//
// mmap () should work now (cygnus beta 18), but let's keep it here
// for people using old cygnus releases
#if 0 //def _WINDOWS32
#ifndef WINDHOOS_SUCK_SUCK_SUCK_HH
#define WINDHOOS_SUCK_SUCK_SUCK_HH

caddr_t mmap (caddr_t addr, size_t len, int prot, int flags, int fd, off_t offset);

int munmap (caddr_t addr, size_t len);

#endif // WINDHOOS_SUCK_SUCK_SUCK_HH
#endif // _WINDOWS32 //
