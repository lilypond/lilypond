default:
	$(MAKE) -C ..

dist:
	ln Makefile $(mycc) $(othersrc) $(DDIR)/src

TAGS: $(mycc)
	etags -CT $(mycc) 

clean:
	rm -f $(gencc)