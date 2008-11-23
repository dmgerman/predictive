
all: predictive.tar.gz auto-overlays.tar.gz


predictive.tar.gz: predictive
	tar --exclude '*/.svn*' -cvzhf predictive.tar.gz predictive/


auto-overlays.tar.gz: auto-overlays
	tar --exclude '*/.svn*' -cvzhf auto-overlays.tar.gz auto-overlays/


completion-ui.tar.gz: completion-ui
	tar --exclude '*/.svn*' -cvzhf completion-ui.tar.gz completion-ui/
