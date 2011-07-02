;;
;; path
;;

(push "/usr/local/bin" exec-path)
(let ((user-bin "~/.local/bin"))
  (if (file-exists-p user-bin)
      (push user-bin exec-path)))

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
;; to make latest package.el work with Emacs 23
;;

(defconst package-subdirectory-regexp
  "\\([^.].*?\\)-\\([0-9]+\\(?:[.][0-9]+\\|\\(?:pre\\|beta\\|alpha\\)[0-9]+\\)*\\)")

;;
;; packages
;;

(setq el-get-sources
      '(ac-slime
        auto-complete
        coffee-mode
        color-theme
        el-get
        elein
        haml-mode
        highlight-parentheses
        hl-sexp
        markdown-mode
        nav
        php-mode
        sass-mode
        swank-clojure
        textile-mode
        yaml-mode
        yasnippet
        org-mode

        (:name package24
               :after (lambda ()
                        (add-to-list 'package-archives
                                     '("marmalade" . "http://marmalade-repo.org/packages/"))
                        (add-to-list 'package-archives
                                     '("tailrecursion" . "http://tailrecursion.com/~alan/repo/emacs/"))
                        (add-to-list 'package-archives
                                     '("elpa" . "http://tromey.com/elpa/"))))

        (:name fuzzy-format
               :after (lambda ()
                        (require 'fuzzy-format)
                        (setq fuzzy-format-default-indent-tabs-mode nil)
                        (global-fuzzy-format-mode t)))

        (:name smart-tab
               :after (lambda ()
                        (global-smart-tab-mode 1)))

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

        (:name durendal :after (lambda ()
                                 (durendal-enable)))

        (:name slime
               :after (lambda ()
                        (setq slime-protocol-version 'ignore)
                        (setq font-lock-verbose nil)))
        (:name slime-repl :type elpa)

        (:name dired-details
               ;; Hide details in dired buffers.
               ;; '(' to show details, ')' to hide them.
               ;; See http://www.emacswiki.org/emacs/DiredDetails
               :after (lambda ()
                        (require 'dired-details)
                        (dired-details-install)))

        (:name find-file-in-project
               :type git
               :url "git://github.com/dburger/find-file-in-project.git"
               :after (lambda ()
                        (setq ffip-patterns '("*"))

                        ;; C-x M-f everywhere, Cmd-T on Mac GUI
                        (global-set-key (kbd "C-x M-f") 'find-file-in-project)
                        (when (and (eq system-type 'darwin)
                                   window-system)
                          (global-set-key (kbd "s-t") 'find-file-in-project))))

        (:name ruby-mode :type elpa)

        (:name ruby-block
               :type emacswiki
               :features ruby-block)

        (:name ruby-end
               :type http
               :url "https://github.com/rejeep/ruby-end/raw/master/ruby-end.el"
               :features ruby-end)

        (:name autopair
               :after (lambda ()
                        (autopair-global-mode)))

        (:name color-theme-miami-vice
	       :type elpa
	       :after (lambda ()
                  (load "color-theme-miami-vice")
                  (color-theme-miami-vice)))

        (:name mvnrepl :type elpa)))

(el-get 'sync)

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
(line-number-mode 1) ; have line numbers and
(column-number-mode 1) ; column numbers in the mode line
(tool-bar-mode -1) ; no tool bar with icons
(global-linum-mode 1) ; add line numbers on the left

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
(blink-cursor-mode t)
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
;; window-system specific
;;

(when window-system
  (progn
    (set-fringe-style -1)
    (tooltip-mode -1)
    (scroll-bar-mode -1)			  ; no scroll bars
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
    (add-to-list 'exec-path "/usr/local/bin")
    (when window-system
      (progn
        (set-frame-font "Menlo-14")
        (menu-bar-mode 1)))))

;;
;; lisp jockeying
;;

(defun quicklisp ()
  "Launch SBCL with quicklisp."
  (interactive)
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "/usr/local/bin/sbcl"))

(defun cljrepl ()
  "Launch a Clojure REPL."
  (interactive)
  (let* ((clj-dir "/Users/alan/Projects/clojure/clojure/")
         (clj-jar (concat clj-dir "clojure.jar")))
    (if (file-exists-p clj-jar)
        (inferior-lisp (concat "java -cp " clj-jar " clojure.main"))
      (when (yes-or-no-p (concat "clojure.jar not found.  Build?"))
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
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)