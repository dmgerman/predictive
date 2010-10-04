
all: predictive.tar.gz auto-overlays.tar.gz completion-ui.tar.gz


.PHONY: predictive.tar.gz auto-overlays.tar.gz completion-ui.tar.gz

predictive.tar.gz: predictive
	tar --exclude '*/.svn*' --exclude '*/.git*/' -cvzhf predictive.tar.gz predictive/


auto-overlays.tar.gz: auto-overlays
	tar --exclude '*/.svn*' --exclude '*/.git*/' -cvzhf auto-overlays.tar.gz auto-overlays/


completion-ui.tar.gz: completion-ui
	tar --exclude '*/.svn*' --exclude '*/.git*/' -cvzhf completion-ui.tar.gz completion-ui/
