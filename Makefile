all:

APT := sudo apt

emacs:
	$(APT) install build-essential mu4e isync
	$(APT) build-dep emacs25
	(cd ~/apps/emacs && ./autogen.sh && ./configure && make -j)
	./install emacs

# https://fontawesome.com/v4.7.0
fonts:
	wget https://fontawesome.com/v4.7.0/assets/font-awesome-4.7.0.zip -P /tmp
	xdg-open /tmp/font-awesome-4.7.0.zip

i3:
	$(APT) install i3 i3blocks compton rofi gnome-flashback gnome-power-manager gnome-screensaver feh session-shortcuts
	sudo make -C i3/apps/i3-gnome install

haskell:
	$(APT) install haskell-stack
	stack upgrade
	stack install hindent hlint

python:
	$(APT) install virtualenv virtualenvwrapper

term:
	wget -O gogh https://git.io/vQgMr && chmod +x gogh && ./gogh && rm gogh

.PHONY: all emacs fonts haskell i3 python term
