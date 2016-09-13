;; require package managers, but we'll avoid them if at all possible
(require 'package)

(push '("melpa" . "http://melpa.milkbox.net/packages/") package-archives)
(push '("org" . "http://orgmode.org/elpa/") package-archives)

;;=====================================
;;plugins
;;=====================================

(setq package-list '(
   paredit
   ag
   cider
   auto-complete
   sbt-mode
   exec-path-from-shell
   evil
   evil-leader
   evil-matchit
   evil-surround
   color-theme
   ido-vertical-mode
   flx-ido
   osx-clipboard
   expand-region
   emmet-mode
   web-mode
   helm
   helm-projectile
   zencoding-mode
   company
   magit
   evil-magit
   evil-org
   fsharp-mode
   2048-game
   omnisharp
   avy
   markdown-mode
   mmm-mode
   flycheck
   json-mode
   diminish
   hl-line
   hl-line+
   flymake-ruby
   scss-mode
   yasnippet
   editorconfig
   remember
   tern
   typit
   js2-mode
   tern
   company-tern
   csharp-mode
   projectile))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq evil-want-C-i-jump nil)

(require 'auto-complete)
(require 'exec-path-from-shell)
(require 'evil)
(require 'evil-leader)
(require 'evil-matchit)
(require 'evil-surround)
(require 'color-theme)
(require 'ido)
(require 'ido-vertical-mode)
(require 'flx-ido)
(require 'projectile)
(require 'osx-clipboard)
(require 'expand-region)
(require 'helm)
(require 'helm-config)
(require 'zencoding-mode)
(require 'magit)
(require 'evil-magit)
(require 'markdown-mode)
(require 'emmet-mode)
(require 'fsharp-mode)
(require 'avy)
(require 'flycheck)
(require 'json-mode)
(require 'diminish)
(require 'hl-line)
(require 'hl-line+)
(require 'omnisharp)
(require 'company)
(require 'flymake-ruby)
(require 'scss-mode)
(require 'remember)
(require 'editorconfig)
(require 'ag)
(require 'yasnippet)
(require 'evil-org)
(require 'typit)
(require 'js2-mode)
(require 'tern)
(require 'cider)
(require 'csharp-mode)
(require 'mmm-mode)

(color-theme-initialize)
(load "~/.emacs.d/evil-tmux-navigator/navigate.el")
(require 'navigate)
(load "~/.emacs.d/railscasts-theme/railscasts-theme.el")
(osx-clipboard-mode 1)

(add-to-list 'load-path "~/.emacs.d/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)

(global-hl-line-mode 1)

;;Mode line changes to hide minor modes I don't care about.
(diminish 'projectile-mode "")
(diminish 'osx-clipboard-mode "")
(diminish 'undo-tree-mode "")
(diminish 'company-mode "")

;;Misc editor configuration
(menu-bar-mode -1)
(define-key global-map (kbd "RET") 'newline-and-indent)
(setq-default truncate-lines t)
(setq visible-bell 1)
(editorconfig-mode 1)

;;Temp files
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))

;;snippets
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)
(yas-reload-all)
(define-key yas-keymap (kbd "ESC") 'yas-abort-snippet)

(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)
;;Evil
(evil-mode 1)
(global-evil-matchit-mode 1)
(global-evil-surround-mode 1)

;;This is so I can spam the [ESC] key and eventually exit whatever state Emacs has put me in
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(defun evil-send-string-to-terminal (string)
  (unless (display-graphic-p) (send-string-to-terminal string)))

(defun evil-terminal-cursor-change ()
  (when (string= (getenv "TERM_PROGRAM") "iTerm.app")
    (add-hook 'evil-insert-state-entry-hook (lambda () (evil-send-string-to-terminal "\e]50;CursorShape=1\x7")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (evil-send-string-to-terminal "\e]50;CursorShape=0\x7"))))
  (when (and (getenv "TMUX")  (string= (getenv "TERM_PROGRAM") "iTerm.app"))
    (add-hook 'evil-insert-state-entry-hook (lambda () (evil-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=1\x7\e\\")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (evil-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=0\x7\e\\")))))

(evil-terminal-cursor-change)

;;Evil leader
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  "c" 'comment-dwim
  "e" 'amir/eval-dwim
  "E" 'fsharp-eval-region
  "j" 'avy-goto-line
  "w" 'avy-goto-word-0
  "m" 'amir/split-and-find
  "M" 'amir/hsplit-and-find
  "g" 'projectile-find-file
  "r" 'amir/cider-send-to-repl
  "t" 'amir/touch
  "f" 'next-error
  "d" 'previous-error
  "b" 'ido-switch-buffer
  "." 'ido-dired
  "s" 'magit-status
  "h" 'vc-print-log
  ")" 'next-buffer
  "(" 'previous-buffer
  "`" 'amir/resize-window-dwim
  "TAB" 'amir/resize-equal
  "2" 'amir/resize-window-vertical+
  "3" 'amir/resize-window-vertical-
  "1" 'amir/resize-window-horizontal-
  "4" 'amir/resize-window-horizontal+
  "x" 'amir/write-quit
  "z" 'amir/zoom-buffer)


;; Cider
(add-hook 'cider-repl-mode-hook #'company-mode)

(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

(add-hook
 'clojurescript-mode-hook
 (lambda ()
   (progn
     (show-paren-mode)
     (paredit-mode)
     (modify-syntax-entry ?- "w"))))

;; avy
(setq avy-styles-alist '((avy-goto-word-0 . at-full) (avy-goto-line . at-full)))
(setq avy-all-windows nil)

;; Emac's Ctrl+P
(projectile-global-mode)

(ido-mode 1)
(ido-vertical-mode 1)

(add-hook 'ido-setup-hook 'vim-like-ido-keys)

(defun vim-like-ido-keys ()
  "Add vim like keybindings for ido."
  (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-k") 'ido-prev-match))

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces t)
(setq ido-enable-flex-matching t)

;; Deletes trailing whitespace
(defun amir/turn-on-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace t))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'amir/turn-on-show-trailing-whitespace)

;; Tmux splits
(defun amir/emacs-or-tmux (dir tmux-cmd)
  (interactive)
  (unless (ignore-errors (funcall (intern (concat "windmove-" dir))))
     (shell-command tmux-cmd)))

(global-set-key (kbd "C-j")
  '(lambda ()
     (interactive)
     (evil-normal-state)
     (amir/emacs-or-tmux "up" "tmux select-pane -U")
     (evil-normal-state)))
(global-set-key (kbd "C-k")
  '(lambda ()
     (interactive)
     (evil-normal-state)
     (amir/emacs-or-tmux "down" "tmux select-pane -D")
     (evil-normal-state)))
(global-set-key (kbd "C-l")
  '(lambda ()
     (interactive)
     (evil-normal-state)
     (amir/emacs-or-tmux "right" "tmux next-window")
     (evil-normal-state)))
(global-set-key (kbd "C-h")
  '(lambda ()
     (interactive)
     (evil-normal-state)
     (amir/emacs-or-tmux "left" "tmux previous-window")
     (evil-normal-state)))

(setq-default indent-tabs-mode nil)

;; tab width 2
(setq-default indent-tabs-mode nil tab-width 2)
(setq css-indent-offset 2)

(defun js2-jsx-mode-hook-settings ()
  "Hooks for Web mode. Adjust indents"
  (setq js-indent-level 2)
  (setq tab-width 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'js2-jsx-mode-hook 'js2-mode-hook-settings)
(add-hook 'js2-jsx-mode-hook (lambda () (tern-mode t)))
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

;; load this mode when this file is opened
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'javascript-mode)

(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;ruby syntax highlighting
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
(setq ruby-deep-indent-paren nil)

;;dont auto complete numbers or things with special characters in it
(push (apply-partially
       #'cl-remove-if
       (lambda (c)
         (or (string-match-p "[^\x00-\x7F]+" c)
             (string-match-p "[0-9]+" c)
             (if (equal major-mode "org")
                 (>= (length c) 15)))))
      company-transformers)

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(company-mode)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.03)
(setq company-minimum-prefix-length 2)
(setq company-show-numbers 't)
(setq omnisharp-company-match-type 'company-match-flx)
(setq gc-cons-threshold 20000000)
(setq company-dabbrev-downcase 'nil)
(eval-after-load 'company
    '(add-to-list 'company-backends 'company-omnisharp))

(eval-after-load 'company
'(add-to-list 'company-backends 'company-tern))

(setq mmm-global-mode 1)

(defun my-mmm-markdown-auto-class (lang &optional submode)
  "Define a mmm-mode class for LANG in `markdown-mode' using SUBMODE.
If SUBMODE is not provided, use `LANG-mode' by default."
  (let ((class (intern (concat "markdown-" lang)))
        (submode (or submode (intern (concat lang "-mode"))))
        (front (concat "^```" lang "[\n\r]+"))
        (back "^```"))
    (mmm-add-classes (list (list class :submode submode :front front :back back)))
    (mmm-add-mode-ext-class 'markdown-mode nil class)))

;; Mode names that derive directly from the language name
(mapc 'my-mmm-markdown-auto-class
      '("awk" "bibtex" "c" "cpp" "css" "html" "latex" "lisp" "makefile"
        "markdown" "python" "r" "ruby" "sql" "stata" "xml" "csharp"))

(defun amir/company-complete-equal-sign ()
  (interactive)
  (progn
    (company-complete-selection)
    (insert " =")))

(defun amir/company-complete-paren ()
  (interactive)
  (progn
    (company-complete-selection)
    (insert "(")))

(defun amir/company-complete-end-paren ()
  (interactive)
  (progn
    (company-complete-selection)
    (insert ")")))

(defun amir/company-complete-. ()
  (interactive)
  (progn
    (company-complete-selection)
    (insert ".")))

(defun amir/company-complete-comma ()
  (interactive)
  (progn
    (company-complete-selection)
    (insert ",")))

(defun amir/company-complete-end-curly ()
  (interactive)
  (progn
    (company-complete-selection)
    (insert "}")))

;; Make autocomplete insert text if code based keys are pressed
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "C-n") 'company-abort)
     (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
     (define-key company-active-map (kbd "TAB") 'company-select-next)
     (define-key company-active-map (kbd ".") 'amir/company-complete-.)
     (define-key company-active-map (kbd ",") 'amir/company-complete-comma)
     (define-key company-active-map (kbd "=") 'amir/company-complete-equal-sign)
     (define-key company-active-map (kbd "}") 'amir/company-complete-end-curly)
     (define-key company-active-map (kbd ")") 'amir/company-complete-end-paren)
     (define-key company-active-map (kbd "(") 'amir/company-complete-paren)))

;;tail log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))

;; magit stuff
(setq magit-last-seen-setup-instructions "1.4.0")

(add-hook 'magit-setup-hook 'vim-like-magit-keys)

(defun vim-like-magit-keys ()
  "Add vim like keybindings for ido."
  (define-key magit-status-mode-map (kbd "k") nil))

;;custom methods
(defun amir/resize-equal ()
  (interactive)
  (balance-windows)
  (redraw-display))

(defun amir/resize-window-dwim ()
  (interactive)
  (let ((maximize (< (window-width) (/ (frame-width) 2))))
    (cond ((< (window-width) 80) (evil-window-set-width 80))
          (maximize
           (progn
             (evil-window-set-width (frame-width))
             (evil-window-set-height (frame-height))
             (redraw-display)))
          ((not maximize) (evil-window-set-width 80)))))

(defun amir/zoom-buffer ()
  (interactive)
  (progn
    (evil-window-set-width (frame-width))
    (evil-window-set-height (frame-height))
    (redraw-display)))

(defun amir/resize-window-vertical+ ()
  (interactive)
  (evil-window-set-height (+ (window-height) 10)))

(defun amir/resize-window-vertical- ()
  (interactive)
  (evil-window-set-height (- (window-height) 10)))

(defun amir/resize-window-horizontal+ ()
  (interactive)
  (evil-window-set-width (+ (window-width) 10)))

(defun amir/resize-window-horizontal- ()
  (interactive)
  (evil-window-set-width (- (window-width) 10)))

(defun amir/touch ()
  "Run touch command on current file."
  (interactive)
  (when buffer-file-name
    (shell-command (concat "touch " (shell-quote-argument buffer-file-name)))
    (clear-visited-file-modtime)))

(defun amir/paredit-wrap-around ()
  "Puts parenthesis around current s-exp. Used in clojure and elisp."
  (interactive)
  (paredit-wrap-round)
  (evil-insert 1))

(defun amir/paredit-splice-sexp-killing-backward ()
  "Deletes parenthesis around current s-exp. Used in clojure and elisp."
  (interactive)
  (paredit-splice-sexp-killing-backward)
  (evil-insert 1))

(defun amir/reload-init-el ()
  "Fast way to reload my init.el file."
  (interactive)
  (load "~/.emacs.d/init.el"))

(defun amir/split-and-find ()
  "Opens a new split window and brings up projectile so I can search for a file."
  (interactive)
  (evil-window-vsplit)
  (projectile-find-file))

(defun amir/hsplit-and-find ()
  "Opens a new split window and brings up projectile so I can search for a file."
  (interactive)
  (evil-window-split)
  (projectile-find-file))

(defun amir/cljs-buffer ()
  (first
   (-filter (lambda (x) (string-match ".*CLJS.*" (buffer-name x)))
            (cider-connections))))

(defun amir/cider-send-to-repl ()
  (interactive)
  (progn
    (evil-append 0)
    (let ((s (buffer-substring-no-properties
             (nth 0 (cider-last-sexp 'bounds))
             (nth 1 (cider-last-sexp 'bounds)))))
     (with-current-buffer (amir/cljs-buffer)
       (insert s)
       (cider-repl-return)))
    (evil-normal-state)))

(defun amir/eval-dwim ()
  "Send the current selected \"stuff\" to the repl."
  (interactive)
  (pcase major-mode
    (`clojure-mode (cider-eval-defun-at-point))
    (`clojurescript-mode (cider-eval-defun-at-point))
    (`fsharp-mode
     (progn
       (fsharp-eval-phrase)
       (evil-next-line 1)))
    (_ (eval-last-sexp nil))))

(defun amir/eval-file-dwim ()
  "Send the current selected \"stuff\" to the repl."
  (interactive)
    (pcase major-mode
      (`clojure-mode (cider-eval-file buffer-file-name))
      (`fsharp-mode (fsharp-eval-region (point) (mark)))
      (_ (eval-last-sexp nil))))

(defun amir/write-quit ()
  ":wq"
  (interactive)
  (evil-save nil t)
  (kill-buffer-and-window))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:background "cyan" :foreground "black"))))
 '(avy-lead-face-0 ((t (:background "cyan" :foreground "black"))))
 '(avy-lead-face-1 ((t (:background "cyan" :foreground "black"))))
 '(avy-lead-face-2 ((t (:background "brightblack" :foreground "white"))))
 '(cider-debug-code-overlay-face ((t (:background "brightblack"))))
 '(cider-result-overlay-face ((t (:background "brightblack" :box (:line-width -1 :color "yellow")))))
 '(col-highlight ((t (:background "color-233"))))
 '(custom-variable-tag ((t (:foreground "cyan" :weight bold))))
 '(diff-added ((t (:inherit diff-changed :background "black" :foreground "#ddffdd"))))
 '(diff-file-header ((t (:background "black" :weight bold))))
 '(diff-header ((t (:background "black" :foreground "grey80"))))
 '(diff-refine-added ((t (:background "color-22" :foreground "white"))))
 '(diff-refine-removed ((t (:inherit nil :background "#ffbbbb" :foreground "black"))))
 '(diff-removed ((t (:inherit diff-changed :background "black" :foreground "#ffdddd"))))
 '(flycheck-warning ((t (:inherit warning :background "black" :underline t))))
 '(flymake-errline ((t (:background "color-52" :foreground "white"))))
 '(flymake-warnline ((t (:background "yellow" :foreground "white"))))
 '(font-lock-keyword-face ((t (:foreground "color-216"))))
 '(fsharp-usage-face ((t (:foreground "color-39"))))
 '(hl-line ((t (:background "color-235"))))
 '(jabber-activity-personal-face ((t (:foreground "red" :weight bold))))
 '(js2-external-variable ((t (:foreground "color-136"))))
 '(js2-function-param ((t (:foreground "color-81"))))
 '(lazy-highlight ((t (:background "black" :foreground "white" :underline t))))
 '(link ((t (:foreground "color-39" :underline t))))
 '(magit-diff-added ((t (:background "black" :foreground "#22aa22"))))
 '(magit-diff-added-highlight ((t (:background "black" :foreground "#22aa22"))))
 '(magit-diff-context-highlight ((t (:background "black" :foreground "brightgreen"))))
 '(magit-diff-hunk-heading-highlight ((t (:background "black" :foreground "white"))))
 '(magit-diff-removed ((t (:background "brightblack" :foreground "#aa2222"))))
 '(magit-diff-removed-highlight ((t (:background "black" :foreground "#aa2222"))))
 '(magit-section-highlight ((t (:background "black"))))
 '(minibuffer-prompt ((t (:foreground "color-143" :weight bold))))
 '(mmm-default-submode-face ((t nil)))
 '(mode-line ((t (:background "color-130" :foreground "white" :box nil))))
 '(mode-line-buffer-id ((t (:background "brightred" :foreground "white"))))
 '(neo-dir-link-face ((t (:foreground "cyan"))))
 '(neo-file-link-face ((t (:foreground "white"))))
 '(neo-header-face ((t (:foreground "color-33"))))
 '(org-agenda-structure ((t (:foreground "color-38"))))
 '(org-date ((t (:foreground "color-81" :underline t))))
 '(org-document-title ((t (:foreground "blue" :weight bold))))
 '(org-done ((t (:foreground "color-28" :weight bold))))
 '(org-scheduled ((t (:foreground "color-34"))))
 '(org-table ((t (:foreground "brightblue"))))
 '(org-todo ((t (:foreground "color-196" :weight bold))))
 '(secondary-selection ((t (:background "color-236"))))
 '(shadow ((t (:foreground "cyan"))))
 '(show-paren-match ((t (:background "cyan"))))
 '(show-paren-mismatch ((t (:background "color-90" :foreground "white"))))
 '(smerge-markers ((t (:background "brightblack" :foreground "white"))))
 '(smerge-mine ((t (:foreground "#ffdddd"))))
 '(smerge-other ((t (:foreground "#ddffdd"))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "#aaffaa" :foreground "black"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "color-250"))))
 '(web-mode-html-tag-face ((t (:foreground "yellow"))))
 '(whitespace-hspace ((t (:foreground "beige"))))
 '(whitespace-newline ((t (:foreground "white" :weight normal))))
 '(whitespace-space ((t (:foreground "beige"))))
 '(whitespace-space-after-tab ((t (:background "red" :foreground "white"))))
 '(whitespace-tab ((t (:foreground "beige")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-use-environment t)
 '(avy-all-windows (quote all-frames))
 '(cider-cljs-lein-repl
   "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")
 '(js2-basic-offset 2)
 '(jsx-indent-level 2)
 '(minibuffer-prompt-properties (quote (read-only t face minibuffer-prompt)))
 '(omnisharp-server-executable-path
   "/Users/amiralirajan/Projects/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe")
 '(org-agenda-files (list "~/.org/life.org"))
 '(ruby-deep-arglist nil)
 '(ruby-deep-indent-paren nil)
 '(safe-local-variable-values
   (quote
    ((cider-cljs-lein-repl . "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))"))))
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(web-mode-code-indent-offset 2))
