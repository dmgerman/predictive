
all: predictive auto-overlays completion-ui



# Note: Currently tarballs will be rebuild every time make is run, since we depend on <package>.tar.gz but generate <package>-<version>.tar.gz

predictive: predictive.tar.gz predictive.tar

auto-overlays: auto-overlays.tar.gz auto-overlays.tar

completion-ui: completion-ui.tar.gz completion-ui.tar



PREDICTIVE-TARBALL-DIR=predictive
PREDICTIVE-ELPA-DIR=predictive-elpa
AUTO-OVERLAYS-TARBALL-DIR=auto-overlays
AUTO-OVERLAYS-ELPA-DIR=auto-overlays-elpa
COMPLETION-UI-TARBALL-DIR=completion-ui
COMPLETION-UI-ELPA-DIR=completion-ui-elpa

VPATH=./:$(PREDICTIVE-TARBALL-DIR):$(PREDICTIVE-ELPA-DIR):$(AUTO-OVERLAYS-TARBALL-DIR):$(AUTO-OVERLAYS-ELPA-DIR):$(COMPLETION-UI-TARBALL-DIR):$(COMPLETION-UI-ELPA-DIR):../


OB :=(
CB :=)



.PHONY: clean

clean:
	rm -f *.tar.gz
	rm -f *.tar
	rm -f *-pkg.el




# -pkg.el file
%-pkg.el: %.el
	./generate-pkg-file $<

# keep generated -pkg.el files
.PRECIOUS: %-pkg.el


# ELPA package
%.tar: %-pkg.el
	$(eval VERSION := $(shell sed 's/$(OB)define-package "\$(OB)[0-9.]\+\$(CB)".*/\1/' $*-pkg.el))
	$(if $(VERSION),,$(error Could not determine $* package version))
	ln -snf $*-elpa $*-$(VERSION)
	tar --exclude-vcs -cvhf $*-$(VERSION).tar $*-$(VERSION)/
	rm $*-$(VERSION)


# tarball package
%.tar.gz: %-pkg.el
	$(eval VERSION := $(shell sed 's/$(OB)define-package "\$(OB)[0-9.]\+\$(CB)".*/\1/' $<))
	$(if $(VERSION),,$(error Could not determine $* package version))
	tar --exclude-vcs -cvzhf $*-$(VERSION).tar.gz $*
