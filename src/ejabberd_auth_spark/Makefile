# $Id$

CC = 
CFLAGS = 
CPPFLAGS = 
LDFLAGS = 
LIBS =  

ERLANG_CFLAGS = 
ERLANG_LIBS = 

# Assume Linux-style dynamic library flags
DYNAMIC_LIB_CFLAGS = -
ifeq ($(shell uname),Darwin)
    DYNAMIC_LIB_CFLAGS = 
endif
ifeq ($(shell uname),SunOs)
    DYNAMIC_LIB_CFLAGS = 
endif


EFLAGS += -I ..
EFLAGS += -pz ..

# make debug=true to compile Erlang module with debug informations.
ifdef debug
	EFLAGS+=+debug_info
endif

ERLSHLIBS = 
OUTDIR = ..
SOURCES = $(wildcard *.erl)
BEAMS = $(addprefix $(OUTDIR)/,$(SOURCES:.erl=.beam))

all:    $(BEAMS) $(ERLSHLIBS)

$(OUTDIR)/%.beam:       %.erl
	/usr/local/bin/erlc -W $(EFLAGS) -o $(OUTDIR) $<

#all:	$(ERLSHLIBS)
#	erl -s make all report "{outdir, \"..\"}" -noinput -s erlang halt

$(ERLSHLIBS):	../%.so:	%.c
	$(CC) $(INCLUDES) $(CFLAGS) $(LDFLAGS) \
		$(subst ../,,$(subst .so,.c,$@)) \
		$(LIBS) \
		$(ERLANG_CFLAGS) \
		$(ERLANG_LIBS) \
		-o $@ \
		$(DYNAMIC_LIB_CFLAGS)

clean:
	rm -f $(BEAMS) $(ERLSHLIBS)

distclean: clean
	rm -f Makefile

TAGS:
	etags *.erl
