all:

emacs:
	sudo apt install build-essential
	sudo apt build-dep emacs25
	(cd ~/apps/emacs && ./autogen.sh && ./configure && make)
	./install emacs

i3:
	sudo apt install i3
	sudo make -C i3/apps/i3-gnome install

.PHONY: all emacs i3
