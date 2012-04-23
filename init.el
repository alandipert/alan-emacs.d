;;
;; package business
;;

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(setq my-pkgs
      '(auto-complete
        (autopair . (lambda ()
                      (require 'autopair)
                      (autopair-global-mode)))
        clojure-mode
        clojurescript-mode
        dired-details
        (find-file-in-project . (lambda ()
                                  ;; We don't care about no stinkin' patterns!
                                  (setq ffip-patterns '("*"))

                                  ;; Do not cache by default.
                                  (setq ffip-use-project-cache nil)

                                  (defun ffip-toggle-use-project-cache ()
                                    "Toggles project file caching for find-file-in-project.el."
                                    (interactive)
                                    (setq ffip-use-project-cache (not ffip-use-project-cache))
                                    (message (concat "Project caching "
                                                     (if ffip-use-project-cache
                                                         "enabled."
                                                       "disabled."))))

                                  ;; C-x M-f everywhere, Cmd-T on Mac GUI
                                  ;; Shift-(key) to toggle project caching
                                  (global-set-key (kbd "C-x M-f") 'find-file-in-project)
                                  (global-set-key (kbd "C-x M-F") 'ffip-toggle-use-project-cache)
                                  (when (and (eq system-type 'darwin)
                                             window-system)
                                    (global-set-key (kbd "s-t") 'find-file-in-project)
                                    (global-set-key (kbd "s-T") 'ffip-toggle-use-project-cache))))
        magit
        (paredit . (lambda ()
                     (let ((paredit-modes '(clojure
                                            emacs-lisp
                                            lisp
                                            lisp-interaction
                                            ielm)))
                       (dolist (mode paredit-modes)
                         (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
                                   (lambda ()
                                     (paredit-mode +1)
                                     ;; a binding that works in the terminal
                                     (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)))))))
        markdown-mode
        coffee-mode
        hl-sexp))

(defun install-idempotent (name)
  "Install package name unless already installed."
  (when (not (package-installed-p name))
    (package-install name)))

(defun pkg-name (pkg)
  "Given a pair or symbol, return a symbol."
  (if (symbolp pkg) pkg (car pkg)))

(defun install-stuff (pkgs)
  "Spin through pkgs list and install every package."
  (package-refresh-contents)
  (dolist (pkg pkgs)
    (install-idempotent (pkg-name pkg))))

(defun config-stuff (pkgs)
  "Spin through my pkgs list and run each configuration function."
  (dolist (pkg pkgs)
    (when (not (symbolp pkg))
      (funcall (eval (cdr pkg))))))

(defun doit (pkgs)
  "Install packages if they haven't been, and run their
configuration lambdas."
  (when (not (package-installed-p (pkg-name (last pkgs))))
    (install-stuff pkgs))
  (config-stuff pkgs))

;;; doit
(doit my-pkgs)

;;
;; visual settings
;;

(setq inhibit-splash-screen t
      initial-scratch-message nil
      truncate-partial-width-windows nil)

(when (locate-library "clojure-mode")   ;*scratch* is clojure-mode by default
  (setq initial-major-mode 'clojure-mode))

(line-number-mode 1) ; have line numbers and
(column-number-mode 1) ; column numbers in the mode line
(global-linum-mode 1) ; add line numbers on the left
(setq linum-format "%d  ") ; throw a bit of padding on there
(setq visible-bell t)

;;
;; quirk fixes, behaviors
;;

(add-to-list 'default-frame-alist '(alpha . 100))  ; compiz fix
(setq x-select-enable-clipboard t
      make-backup-files nil
      auto-save-default nil
      diff-switches "-u -w"
      whitespace-style '(trailing lines space-before-tab
                                  face indentation space-after-tab))
(setq-default tab-width 2
              indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)                          ; typed text replaces active selection
;; (blink-cursor-mode t)
(show-paren-mode t)
(auto-compression-mode t)
(recentf-mode 1)
(setq diff-switches "-u -w")
(menu-bar-mode 0)

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

;;; use ibuffer
;;; http://martinowen.net/blog/2010/02/tips-for-emacs-ibuffer.html
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "home")))

;;
;; org settings
;;

(define-key global-map "\C-ca" 'org-agenda)
(setq org-hide-leading-stars t
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
  (if (not (eq system-type 'darwin))
      (global-set-key (kbd "M-m") 'toggle-fullscreen))
  (progn
    (when (boundp 'tool-bar-mode)
      (tool-bar-mode -1)) ; no tool bar with icons
    (load-theme 'tango-dark)
    (set-fringe-style -1)
    (tooltip-mode -1)
    (scroll-bar-mode -1) ; no scroll bars
    (modify-frame-parameters (selected-frame)
                             (list (cons 'cursor-type 'hollow)))))

;;
;; os x specific
;;

(when (eq system-type 'darwin)
  (progn
    (setq grep-find-use-xargs 'exec
          ispell-program-name "aspell"
          magit-git-executable "/usr/local/bin/git")
    (when window-system
      (progn
        (set-frame-font "Menlo-14")
        (menu-bar-mode 1)
        (global-set-key (kbd "s-m") 'ns-toggle-fullscreen)))))

;;
;; lisp jockeying
;;

(defun quicklisp ()
  "Launch SBCL with quicklisp."
  (interactive)
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "/usr/local/bin/sbcl"))

(defcustom clj-dir "/home/alan/projects/opensource/clojure"
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

(defun leinrepl ()
  "Launch a Leiningen REPL for current file's project. Runs only
   if lib exists.  Requires cl."
  (interactive)
  (if (not (executable-find "lein"))
      (message "lein command not found.")
    (labels ((locate-project (file name)
                             ;; adapted from https://github.com/technomancy/emacs-starter-kit/blob/master/dominating-file.el
                             (let* ((file (abbreviate-file-name file))
                                    (stop-dir-regexp "\\`\\(?:[\\/][\\/][^\\/]+\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'")
                                    (root nil)
                                    (prev-file file)
                                    try)
                               (while (not (or root
                                               (null file)
                                               (string-match stop-dir-regexp file)))
                                 (setq try (file-exists-p (expand-file-name name file)))
                                 (cond (try (setq root file))
                                       ((equal file (setq prev-file file
                                                          file (file-name-directory
                                                                (directory-file-name file))))
                                        (setq file nil))))
                               root)))
      (let ((project-dir (locate-project buffer-file-name "project.clj")))
        (if (file-exists-p (concat project-dir "lib"))
            (inferior-lisp "lein repl")
          (message "lib directory not found.  Have you run lein deps?"))))))

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
