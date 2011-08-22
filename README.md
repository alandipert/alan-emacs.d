### The Goods

* Clojure stuff
* Latest `org-mode`
* Latest `magit`
* Sweet color theme, `color-theme-miami-vice`, based on `color-theme-molokai`

### Prepare

* Emacs HEAD, GUI or CLI, on Mac or Linux
  * For instructions building Emacs source, see Debian/Ubuntu and Mac instructions below.
* texinfo (to build magit): on Ubuntu, install with `sudo apt-get install texinfo`

### Install

1. Clone this repository into `~/.emacs.d`
2. Run Emacs.  When you see the message "Congrats, el-get is installed and ready to serve!", restart Emacs.
3. Emacs will do stuff and then you'll see warning messages.  Keep restarting Emacs until the messages go away.
4. Done!

## Installing Emacs

### Linux: Debian-based distro (Debian, Ubuntu)

* Install required packages:

      sudo apt-get install build-essential texinfo libncurses5-dev autoconf automake shtool libgtk2.0-dev libtiff4-dev libgif-dev libjpeg62-dev libpng12-dev libxpm-dev libtool

* Clone emacs from its git mirror:

      git clone git://git.savannah.gnu.org/emacs.git
      cd emacs

* Build and install:

      make && sudo make install

### Mac

* Install the [homebrew](https://github.com/mxcl/homebrew) package manager

* Install Emacs

      brew install emacs --cocoa --use-git-head --HEAD
      mv /usr/local/Cellar/emacs/HEAD/Emacs.app /Applications/
