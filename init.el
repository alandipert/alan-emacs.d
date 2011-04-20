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
      '(el-get
        elein
        color-theme
        auto-complete
        ac-dabbrev
        ac-slime
        hl-sexp
        highlight-symbol
        highlight-parentheses
        yasnippet
        markdown-mode

        (:name package24
               :after (lambda ()
                        (setq package-archives
                              (concatenate
                               'list
                               package-archives
                               '(("marmalade" . "http://marmalade-repo.org/packages/")
                                 ("tailrecursion" . "http://repo.tailrecursion.com/emacs/"))))))

        (:name fuzzy-format
               :after (lambda ()
                        (require 'fuzzy-format)
                        (setq fuzzy-format-default-indent-tabs-mode nil)
                        (global-fuzzy-format-mode t)))

        (:name smart-tab
               :after (lambda ()
                        (global-smart-tab-mode 1)))

        (:name magit
               :after (lambda () (global-set-key (kbd "C-x C-z") 'magit-status)))

        (:name paredit
               :after (lambda () 
                        (add-hook 'clojure-mode-hook          (lambda () (paredit-mode +1)))
                        (add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
                        (add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
                        (add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
                        ;; a binding that works in the terminal
                        (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)))

        (:name slime-repl :type elpa)
        (:name clojure-mode :type elpa)
        (:name durendal :after (lambda () (durendal-enable)))

        (:name swank-clojure
               :after (lambda () (add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)))

        (:name org
               :type elpa
               :repo ("gnu-elpa" . "http://elpa.gnu.org/packages/")
               :after (lambda ()
                        (let ((re-filter-list
                               (lambda (re lst)
                                 (delq nil
                                       (mapcar
                                        (lambda (x)
                                          (and (not (string-match-p re x)) x)) lst)))))
                          ;; remove built-in org-mode from load-path
                          (setq load-path (funcall re-filter-list "org$" load-path))
                          (define-key global-map "\C-ca" 'org-agenda))))

        ;; stuff from tailrecursion repo
        (:name color-theme-miami-vice :type elpa)
        (:name mvnrepl :type elpa)))

(el-get)

(let ((user-custom-file "~/.emacs.d/custom.el"))
  (if (not (file-exists-p user-custom-file))
      (shell-command (concat "touch " user-custom-file)))
  (setq custom-file user-custom-file)
  (load custom-file))

;; 
;; visual settings
;; 

(setq inhibit-splash-screen t)
(line-number-mode 1)			  ; have line numbers and
(column-number-mode 1)			  ; column numbers in the mode line
(tool-bar-mode -1)			  ; no tool bar with icons
(scroll-bar-mode -1)			  ; no scroll bars
(global-linum-mode 1)			  ; add line numbers on the left
(setq initial-scratch-message nil)        ; empty scratch buffer
(setq truncate-partial-width-windows nil) ; wrap lines for vertically split windows
(highlight-symbol-mode 1)

;; 
;; quirk fixes, behaviors
;; 

(add-to-list 'default-frame-alist '(alpha . 100))  ; compiz fix
(setq x-select-enable-clipboard t)                 ; fix clipboard behavior
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)                          ; typed text replaces active selection
(blink-cursor-mode t)
(show-paren-mode t)
(set-fringe-style -1)
(tooltip-mode -1)
(auto-compression-mode t)
(recentf-mode 1)
(setq diff-switches "-u -w"
      magit-diff-options "-w")

;; 
;; creature comforts
;; 

(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)

(defun recentf-ido-find-file ()
  "Find a recent file using ido.
   From Phil Hagelberg's emacs-starter-kit."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key (kbd "C-x f") 'recentf-ido-find-file)

;; 
;; window-system specific
;; 

(when window-system
  (progn
    (load "color-theme-miami-vice")
    (color-theme-miami-vice)
    (modify-frame-parameters (selected-frame)
                             (list (cons 'cursor-type 'hollow)))
    (menu-bar-mode 0)))

;; 
;; os x specific
;; 

(when (eq system-type 'darwin)
  (progn
    (setq grep-find-use-xargs 'exec)
    (setq ispell-program-name "aspell")
    (add-to-list 'exec-path "/usr/local/bin")
    (setq magit-git-executable "/usr/local/bin/git")
    (menu-bar-mode 1)))

;; 
;; change font size
;; 

(defun increase-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (ceiling (* 1.10
                                  (face-attribute 'default :height)))))
(defun decrease-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (floor (* 0.9
                                (face-attribute 'default :height)))))
;;
;; key bindings
;;

(global-set-key (kbd "C-+") 'increase-font-size)
(global-set-key (kbd "C--") 'decrease-font-size)
