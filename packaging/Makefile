
all: predictive auto-overlays completion-ui


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

GPATH=./:$(PREDICTIVE-TARBALL-DIR):$(PREDICTIVE-ELPA-DIR):$(AUTO-OVERLAYS-TARBALL-DIR):$(AUTO-OVERLAYS-ELPA-DIR):$(COMPLETION-UI-TARBALL-DIR):$(COMPLETION-UI-ELPA-DIR)



.PHONY: clean

clean:
	rm -f *.tar.gz
	rm -f *.tar
	rm -f *-pkg.el




# version number extraction and testing

PREDICTIVE-VERSION != sed -n '/Package-Version: / { s/.*Package-Version: \([0-9.]\+\).*/\1/g ; P }' predictive-elpa/predictive.el

AUTO-OVERLAYS-VERSION != sed -n '/Package-Version: / { s/.*Package-Version: \([0-9.]\+\).*/\1/g ; P }' auto-overlays-elpa/auto-overlays.el

COMPLETION-UI-VERSION != sed -n '/Package-Version: / { s/.*Package-Version: \([0-9.]\+\).*/\1/g ; P }' completion-ui-elpa/completion-ui.el


.PHONY : predictive-version auto-overlays-version completion-ui-version

predictive-version:
	$(if $(PREDICTIVE-VERSION),,$(error Could not determine predictive package version))

auto-overlays-version:
	echo auto-overlays package version: $(AUTO-OVERLAYS-VERSION)
	$(if $(AUTO-OVERLAYS-VERSION),,$(error Could not determine auto-overlays package version))

completion-ui-version:
	$(if $(COMPLETION-UI-VERSION),,$(error Could not determine completion-ui package version))




# tarball targets

#.PHONY: predictive.tar.gz auto-overlays.tar.gz completion-ui.tar.gz

predictive.tar.gz: predictive-version $(PREDICTIVE-TARBALL-DIR)
	tar --exclude '*/.svn*' --exclude '*/.git*/' -cvzhf predictive-$(PREDICTIVE-VERSION).tar.gz predictive/


auto-overlays.tar.gz: auto-overlays-version $(AUTO-OVERLAYS-TARBALL-DIR)
	tar --exclude '*/.svn*' --exclude '*/.git*/' -cvzhf auto-overlays-$(AUTO-OVERLAYS-VERSION).tar.gz auto-overlays/


completion-ui.tar.gz: completion-ui-version $(COMPLETION-UI-TARBALL-DIR)
	tar --exclude '*/.svn*' --exclude '*/.git*/' -cvzhf completion-ui-$(COMPLETION-UI-VERSION).tar.gz completion-ui/



# ELPA package targets

predictive.tar: predictive-version predictive-pkg.el
	ln -s predictive-elpa predictive-$(PREDICTIVE-VERSION)
	tar --exclude '*/.svn*' --exclude '*/.git*/' -cvhf predictive-$(PREDICTIVE-VERSION).tar predictive-$(PREDICTIVE-VERSION)/
	rm predictive-$(PREDICTIVE-VERSION)



auto-overlays.tar: auto-overlays-version auto-overlays-pkg.el
	ln -s auto-overlays-elpa auto-overlays-$(AUTO-OVERLAYS-VERSION)
	tar --exclude '*/.svn*' --exclude '*/.git*/' -cvhf auto-overlays-$(AUTO-OVERLAYS-VERSION).tar auto-overlays-$(AUTO-OVERLAYS-VERSION)/
	rm auto-overlays-$(AUTO-OVERLAYS-VERSION)



completion-ui.tar: completion-ui-version completion-ui-pkg.el
	ln -s completion-ui-elpa completion-ui-$(COMPLETION-UI-VERSION)
	tar --exclude '*/.svn*' --exclude '*/.git*/' -cvhf completion-ui-$(COMPLETION-UI-VERSION).tar completion-ui-$(COMPLETION-UI-VERSION)/
	rm completion-ui-$(COMPLETION-UI-VERSION)



# implicit rules for building version-specific ELPA *-pgk.el files

%-pkg.el: %.el
	./generate-pkg-file $<
