
LIBINTL=$(outdir)/libintl.a
check-lib-intl:
	$(MAKE) -C $(depth)/intl lib

include $(stepdir)/C.make

