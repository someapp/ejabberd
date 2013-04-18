## mod_spark_msgarchive Makefile
## To make life easier for development, release packing and local development testing
##

ERLANG_HOME := /opt/erlang/lib/erlang
ERLANG_LIB := /opt/erlang/lib/erlang/lib
PATH:= $(PATH):$(ERLANG_HOME)/bin

REPO=mod_spark_msgarchive
REBAR=./rebar -v

display:
	@echo "Erlang home" $(ERLANG_HOME)
	@echo "Erlang Lib" $(ERLANG_LIB)
	@echo "Repo name" $(REPO)
	@echo "Rebar location" $(REBAR)


get-deps: 
	@echo "Getting depedency"
	$(REBAR) get-deps

compile:
	@echo "Compilation"
	$(REBAR) compile

eunit:
	@echo "Running eunit test"
	$(REBAR) skip-deps=true eunit

doc:
	@echo "Making document"
	$(REBAR) skip-deps=true doc

rel: 	compile rel/$(REPO)
	##@echo "Making release package"
	compile rel/$(REPO)

relclean:
	@echo "Cleaning repo"
	$(REBAR) clean 
	##rm -vrf
test:
	@echo "Compilation and running common tests "
	eunit common_test

all: 	display
	relclean
	compile


 
