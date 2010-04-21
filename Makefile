OCAMLMAKEFILE=OCamlMakefile

SOURCES = util.ml api.mli api.ml \
	  namespace.mli namespace.ml \
	  fql.mli fql.ml \
	  users.mli users.ml \
	  dashboard.mli dashboard.ml
PREDS = camlp4o
PACKS = json-tc.syntax lwt.syntax lwt.unix cohttp
RESULT = facebook
LIB_PACK_NAME = facebook
ANNOTATE = yes

.PHONY: all
all: pack-byte-code pack-native-code facebook.cma facebook.cmxa
	@ :

DISTVERSION = 0.1

META: META.in
	cat META.in | sed -e 's/@DISTVERSION@/$(DISTVERSION)/' > META

LIBINSTALL_FILES = META facebook.cma facebook.cmxa facebook.a facebook.cmi

install: libinstall
uninstall: libuninstall
reinstall: uninstall install

-include $(OCAMLMAKEFILE)
