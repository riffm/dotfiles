install: install-vim install-bash install-git install-hg

install-vim:
	rm -rf ~/.vim ~/.vimrc
	ln -s `pwd`/vim ~/.vim
	ln -s ~/.vim/vimrc ~/.vimrc

install-bash:
	rm -f ~/.profile
	ln -s `pwd`/bash/bashrc ~/.profile

install-git:
	rm -f ~/.gitconfig
	ln -s `pwd`/git/config ~/.gitconfig

install-hg:
	rm -f ~/.hgrc
	ln -s `pwd`/hg/hgrc ~/.hgrc
