;;
;; package business
;;

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(ac-slime
    align-cljlet
    auto-complete
    auto-indent-mode
    clojure-mode
    clojurescript-mode
    dired-details
    find-file-in-project
    go-mode
    hl-sexp
    magit
    markdown-mode
    nrepl
    org
    paredit
    ruby-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;
;; visual settings
;;

(setq inhibit-splash-screen t
      initial-scratch-message nil
      truncate-partial-width-windows nil
      initial-major-mode 'clojure-mode
      linum-format "%d  "
      visual-bell t)

(line-number-mode 1)
(column-number-mode 1)

;;
;; quirk fixes, behaviors
;;

;;; compiz fix
(add-to-list 'default-frame-alist '(alpha . 100))

(setq x-select-enable-clipboard t
      make-backup-files nil
      auto-save-default nil
      diff-switches "-u -w"
      whitespace-style '(trailing lines space-before-tab
                                  face indentation space-after-tab)
      auto-indent-modes '(ruby-mode
                          java-mode
                          javascript-mode
                          emacs-lisp-mode))

(setq-default tab-width 2
              indent-tabs-mode nil
              c-basic-offset 2
              js-indent-level 2)

(dolist (mode auto-indent-modes)
  (add-hook (intern (format "%s-hook" mode)) 'auto-indent-minor-mode))

(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)
(show-paren-mode t)
(auto-compression-mode t)
(recentf-mode 1)
(setq diff-switches "-u -w")
(menu-bar-mode 0)

;;
;; custom.el
;;

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;;
;; creature comforts
;;

(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last"
      ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-show-dot-for-dired t)

(defun recentf-ido-find-file ()
  "Find a recent file using ido.
   From Phil Hagelberg's emacs-starter-kit."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "M-/") 'hippie-expand)

;;; :set wrapscan emulation
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

;;; vim dt emulation
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))

;;
;; org settings
;;

(define-key global-map "\C-ca" 'org-agenda)
(setq org-hide-leading-stars t
      org-src-window-setup 'current-window
      org-todo-keywords (quote ((sequence "TODO" "ONGOING" "DONE")))
      org-todo-keyword-faces
      '(("ONGOING" . "orange")))

;;
;; linux fullscreen
;;

(defvar my-fullscreen-p t "Check if fullscreen is on or off")

(defun my-non-fullscreen ()
  (interactive)
  (progn
    (set-frame-parameter nil 'width 82)
    (set-frame-parameter nil 'fullscreen 'fullheight)))

(defun my-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullboth))

(defun toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
      (my-non-fullscreen)
    (my-fullscreen)))

;;
;; window-system specific
;;

(when window-system
  (global-set-key (kbd "M-m") 'toggle-fullscreen)
  (if (boundp 'tool-bar-mode) (tool-bar-mode -1))
  (load-theme 'tango-dark)
  (set-fringe-style -1)
  (tooltip-mode -1)
  (scroll-bar-mode -1)
  (modify-frame-parameters (selected-frame)
                           (list (cons 'cursor-type 'hollow))))

;;
;; lisp jockeying
;;

(defcustom clj-dir "/home/alan/projects/clojure"
  "Path to Clojure source directory."
  :type 'string
  :group 'cljrepl)

(defun cljrepl ()
  "Launch a Clojure REPL."
  (interactive)
  (let ((clj-jar (concat clj-dir "/clojure.jar")))
    (if (file-exists-p clj-jar)
        (inferior-lisp (concat "java -cp " clj-jar " clojure.main"))
      (when (yes-or-no-p "clojure.jar not found.  Build?")
        (if (shell-command (concat "cd " clj-dir " && ant"))
            (cljrepl)
          (message "Building Clojure failed."))))))

;;
;; change font size
;;

(defun increase-font-size ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (ceiling (* 1.10
                                  (face-attribute 'default :height)))))

(defun decrease-font-size ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (floor (* 0.9
                                (face-attribute 'default :height)))))

;;
;; key bindings
;;

(global-set-key (kbd "C-+") 'increase-font-size)
(global-set-key (kbd "C--") 'decrease-font-size)
(global-set-key (kbd "M-j") 'join-line)
(global-set-key (kbd "RET") 'newline-and-indent)

;; Behave like */# in Vim, jumping to symbols under point.
(global-set-key (kbd "C-x *") 'highlight-symbol-next)
(global-set-key (kbd "C-*") 'highlight-symbol-prev)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;;
;; mvnrepl
;;

(defgroup mvnrepl nil
  "run mvn clojure:repl from emacs"
  :prefix "mvnrepl-"
  :group 'applications)

(defcustom mvnrepl-mvn "mvn"
  "Maven 'mvn' command."
  :type 'string
  :group 'mvnrepl)

(defun mvnrepl-project-root ()
  "Look for pom.xml file to find project root."
  (let ((cwd default-directory)
        (found nil)
        (max 10))
    (while (and (not found) (> max 0))
      (if (file-exists-p (concat cwd "pom.xml"))
          (setq found cwd)
        (setq cwd (concat cwd "../") max (- max 1))))
    (and found (expand-file-name found))))

(defun mvnrepl ()
  "From a buffer with a file in the project open, run M-x mvn-repl to get a project inferior-lisp"
  (interactive)
  (let ((project-root (mvnrepl-project-root)))
    (if project-root
        (inferior-lisp (concat mvnrepl-mvn " -f " project-root "/pom.xml clojure:repl"))
      (message (concat "Maven project not found.")))))

(provide 'mvnrepl)

;;
;; package-specific customizations
;;

;;; find-file-in-project

(setq ffip-patterns '("*")
      ffip-use-project-cache nil)

(defun ffip-toggle-use-project-cache ()
  "Toggles project file caching for find-file-in-project.el."
  (interactive)
  (setq ffip-use-project-cache (not ffip-use-project-cache))
  (message (concat "Project caching "
                   (if ffip-use-project-cache
                       "enabled."
                     "disabled."))))

(global-set-key (kbd "C-x M-f") 'find-file-in-project)
(global-set-key (kbd "C-x M-F") 'ffip-toggle-use-project-cache)

;; paredit

(defvar paredit-modes
  '(clojure
    emacs-lisp
    lisp
    lisp-interaction
    ielm))

(dolist (mode paredit-modes)
  (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
            (lambda ()
              (paredit-mode +1)
              (define-key paredit-mode-map (kbd "M-)")
                'paredit-forward-slurp-sexp))))

;;; ibuffer

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "home")))

;;; misc

(defvar slime-words-of-encouragement
  `("When eating an elephant, take one bite at a time."
    "A memorandum is written not to inform the reader but to protect the writer."
    "Power tends to corrupt; absolute power corrupts absolutely."
    "Anybody can win -- unless there happens to be a second entry."
    "When the plane you are on is late, the plane you want to transfer to is on time."
    "Social innovations tend to the level of minimum tolerable well being."
    "Almost anything is easier to get into than out of."
    "I'd rather have a free bottle in front of me than a prefrontal lobotomy."
    "Justice always prevails ... three times out of seven."
    "Attila the Hun came from a broken home."
    "Don't force it, get a larger hammer."
    "Those whose approval you seek the most give you the least."
    "What the gods get away with, the cows don't."
    "Any order that can be misunderstood has been misunderstood."
    "If it moves, salute it; if it doesn't move, pick it up; if you can't pick it up, paint it."
    "It's always the wrong time of the month."
    "No books are lost by loaning except those you particularly wanted to keep."
    "If it can be borrowed and it can be broken, you will borrow it and you will break it."
    "When you are over the hill, you pick up speed."
    "Misery no longer loves company.  Nowadays it insists on it."
    "Some of it plus the rest of it is all of it."
    "The more ridiculous a belief system, the higher the probability of its success."
    "Old age is always fifteen years older than I am."
    "When you're up to your nose, keep your mouth shut."
    "All people are cremated equal."
    "all ignorance toboggans into know"
    "It is much harder to find a job than to keep one."
    "The ratio of time involved in work to time available for work is usually about 0.6."
    "No matter which way you ride, it's uphill and against the wind."
    "The conclusions of most good operations research studies are obvious."
    "Live within your income, even if you have to borrow to do so."
    "Established technology tends to persist in spite of new technology."
    "If you want your name spelled wrong, die."
    "If you think education is expensive -- try ignorance."
    "If you're feeling good, don't worry.  You'll get over it."
    "Never go to a doctor whose office plants have died."
    "In any household, junk accumulates to fill the space available for its storage."
    "An ounce of application is worth a ton of abstraction."
    "The conventional wisdom is that power is an aphrodisiac. In truth, it's exhausting."
    "You always find something the last place you look."
    "A bird in the hand is dead."
    "If everything seems to be coming your way, you're probably in the wrong lane."
    "It's always the partner's fault."
    "Never offend people with style when you can offend them with substance."
    "Our customer's paperwork is profit.  Our own paperwork is loss."
    "At any level of traffic, any delay is intolerable."
    "As the economy gets better, everything else gets worse."
    "Nothing is ever accomplished by a reasonable man."
    "VM systems programmers do it virtually all the time."
    "Overdoing things is harmful in all cases, even when it comes to efficiency."
    "If the assumptions are wrong, the conclusions aren't likely to be very good."
    "When all else fails, read the instructions."
    "A coup that is known in advance is a coup that does not take place."
    "Nature abhors a vacuous experimenter."
    "It's morally wrong to allow suckers to keep their money."
    "A Smith and Wesson beats four aces."
    "The leak in the roof is never in the same location as the drip."
    "If it's in stock, we have it!"
    "All kookies are not in a jar."
    "People don't change; they only become more so."
    "In matters of dispute, the bank's balance is always smaller than yours."
    "Nothing ever gets built on schedule or within budget."
    "I have seen the truth and it makes no sense."
    "If your next pot of chili tastes better, it probably is because of something left out, rather than added."
    "When things are going well, something will go wrong."
    "The other line moves faster."
    "Necessity is the mother of strange bedfellows."
    "Never, ever, play leapfrog with a unicorn."
    "Whoever has the gold makes the rules."
    "There's no such thing as a small whiskey."
    "One good turn gets most of the blanket."
    "Chicken Little only has to be right once."
    "If a project is not worth doing at all, it is not worth doing well."
    "Inside every large problem there is a small problem struggling to get out."
    "A collision at sea can ruin your entire day."
    "Every man has a scheme that will not work."
    "If they give you ruled paper, write the other way."
    "Friends may come and go, but enemies accumulate."
    "Mahr's Law of Restrained Involvement: Don't get any on you."
    "If the facts do not conform to the theory, they must be discarded."
    "Kickbacks must always exceed bribes."
    "Left to themselves, things tend to go from bad to worse."
    "Every solution breeds new problems."
    "Nature always sides with the hidden flaw."
    "Mother nature is a bitch."
    "If you fool around with a thing for very long,"
    "you will screw it up."
    "Fixing a thing takes longer and costs more than you thought."
    "No amount of genius can overcome a preoccupation for detail."
    "Complex problems have simple, easy-to-understand wrong answers."
    "As the economy gets better, everything else gets worse."
    "Most jobs are marginally better than daytime TV."
    "Any given program, when running, is obsolete"
    "A ship on the beach is a lighthouse to the sea."
    "A man with one watch knows what time it is.  A man with two watches is never sure."
    "An object will fall so as to do the most damage."
    "Badness comes in waves."
    "THINK!"
    "Anything important loses its value soon after being Xeroxed."
    "The insult of an enemy is better then the flattery of a friend."
    "Law of Cybernetic Entomology: There's always one more bug.")
  "Scientifically-proven optimal words of hackerish encouragement.")
