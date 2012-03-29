;;
;; path
;;

(defcustom exec-paths '("/usr/local/bin" "~/.local/bin")
  "Directories to be added to exec-path"
  :type 'string)

(defun add-to-path (dir)
  "Adds a dir to PATH if dir exists."
  (when (file-exists-p dir)
    (progn (add-to-list 'exec-path dir)
           (setenv "PATH" (concat (getenv "PATH") (concat ":" dir))))))

(defun initialize-exec-path ()
  (interactive)
  (dolist (dir exec-paths) (add-to-path dir)))

(initialize-exec-path)

;;
;; init el-get, installing if necessary
;;

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

;; stuff we need

(require 'cl)
(require 'el-get)

;;
;; packages
;;

(require 'package)
(dolist (archive '(("marmalade" . "http://marmalade-repo.org/packages/")
                   ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives archive))
(package-initialize)


;; local sources
(setq el-get-sources
      '((:name fuzzy-format
               :after (lambda ()
                        (require 'fuzzy-format)
                        (setq fuzzy-format-default-indent-tabs-mode nil)
                        (global-fuzzy-format-mode t)))

        (:name magit
               :after (lambda ()
                        (global-set-key (kbd "C-x C-z") 'magit-status)))

        (:name highlight-symbol
               :after (lambda ()
                        (highlight-symbol-mode 1)))

        (:name paredit
               :after (lambda ()
                        (let ((paredit-modes '(clojure
                                               emacs-lisp
                                               lisp
                                               lisp-interaction
                                               ielm)))
                          (dolist (mode paredit-modes)
                            (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
                                      (lambda () (paredit-mode +1)))))

                        ;; a binding that works in the terminal
                        (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)))

        (:name clojure-mode :type elpa
               :after (lambda ()
                        (add-to-list 'auto-mode-alist '("\\.clj.*$" . clojure-mode))))

        (:name slime-repl :type elpa)

        (:name slime
               :type elpa
               :after (lambda ()
                        (setq slime-protocol-version 'ignore)
                        (setq font-lock-verbose nil)))

        (:name find-file-in-project
               :type git
               :url "git://github.com/dburger/find-file-in-project.git"
               :after (lambda ()

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

        (:name mvnrepl
               :type http
               :url "https://raw.github.com/gist/8b05e405eae6e7d1b9a0/7b10094bc8bab07c2d86aea2eabf8b1d5132ca2b/mvnrepl.el"
               :after (lambda ()
                        (require 'mvnrepl)))

        (:name autopair
               :after (lambda ()
                        (require 'autopair)
                        (autopair-global-mode)))

        (:name ruby-mode
               :after (lambda ()
                        (dolist (f '("Capfile"
                                     "Gemfile"
                                     "Guardfile"
                                     "Vagrantfile"
                                     "config.ru"
                                     "Rakefile"))
                          (add-to-list 'auto-mode-alist `(,f . ruby-mode)))))))

(setq my-packages
      (append
       '(ac-slime
         auto-complete
         coffee-mode
         dired-details
         durendal
         elein
         el-get
         highlight-parentheses
         hl-sexp
         markdown-mode
         org-mode
         ruby-block
         ruby-end
         ruby-mode
         swank-clojure
         textile-mode
         yaml-mode
         yasnippet)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)

(let ((user-custom-file "~/.emacs.d/custom.el"))
  (if (not (file-exists-p user-custom-file))
      (shell-command (concat "touch " user-custom-file)))
  (setq custom-file user-custom-file)
  (load custom-file))

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
(when (boundp 'tool-bar-mode)
  (tool-bar-mode -1)) ; no tool bar with icons
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
(defun zap-until-char (arg char)
  "Kill up to ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "p\ncZap until char: ")
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (kill-region (point)
               (progn
                 (search-forward (char-to-string char) nil nil arg)
                 (1- (point)))
               (backward-char)))

(global-set-key (kbd "M-z") 'zap-until-char)

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
      org-todo-keywords (quote ((sequence "TODO" "ONGOING" "DONE"))))

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
