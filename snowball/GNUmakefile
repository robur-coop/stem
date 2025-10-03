include $(LIBSTEMMER)/mkinc.mak

all: srcs includes

srcs: $(LIBSTEMMER)
	@echo $(snowball_sources)

includes: $(LIBSTEMMER)
	@echo $(snowball_headers)

# build.sh extract $(LIBSTEMMER).tar.gz because GNUmakefile requires mkinc.mak.
# This rule is not really used to build libstemmer.a but it can help an use who
# wants to run make
$(LIBSTEMMER): $(LIBSTEMMER).tar.gz
	@tar xzf $(LIBSTEMMER).tar.gz
