(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not. Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed
 'exec-path-from-shell
 'simpleclip
 'undo-fu
 'all-the-icons
 'smartparens
 'vi-tilde-fringe
 'expand-region
 'move-text
 'which-key
 'visual-regexp
 'multiple-cursors
 'windmove
 'ivy
 'swiper
 'counsel
 'smex
 'flx
 'avy
 'ivy-rich
 'counsel-projectile
 'magit
 'git-gutter
 'shell-pop
 'company
 'org
 'evil)

;; Pass system shell environment to Emacs. This is important primarily for shell inside Emacs, but also things like Org mode export to Tex PDF don't work, since it relies on running external command pdflatex, which is loaded from PATH.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Both command keys are 'Super'
(setq mac-right-command-modifier 'super)
(setq mac-command-modifier 'super)

;; Option or Alt is naturally 'Meta'
(setq mac-option-modifier 'meta)

;; Right Alt (option) can be used to enter symbols like em dashes '—' and euros '€' and stuff.
(setq mac-right-option-modifier 'nil)

;; Control is control, and you also need to change Caps Lock to Control in the Keyboard
;; preferences in macOS.

;; Smoother and nicer scrolling
(setq scroll-margin 10
      scroll-step 1
      next-line-add-newlines nil
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Use ESC as universal get me out of here command
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Don't bother with auto save and backups.
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Warn only when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Move file to trash instead of removing.
(setq-default delete-by-moving-to-trash t)

;; Revert (update) buffers automatically when underlying files are changed externally.
(global-auto-revert-mode t)

(setq
 inhibit-startup-message t         ; Don't show the startup message...
 inhibit-startup-screen t          ; ... or screen
 cursor-in-non-selected-windows t  ; Hide the cursor in inactive windows

 echo-keystrokes 0.1               ; Show keystrokes right away, don't show the message in the scratch buffer
 initial-scratch-message nil       ; Empty scratch buffer
 initial-major-mode 'org-mode      ; Org mode by default
 sentence-end-double-space nil     ; Sentences should end in one space, come on!
 confirm-kill-emacs 'y-or-n-p      ; y and n instead of yes and no when quitting
 help-window-select t              ; Select help window so it's easy to quit it with 'q'
 )

(fset 'yes-or-no-p 'y-or-n-p)      ; y and n instead of yes and no everywhere else
(delete-selection-mode 1)          ; Delete selected text when typing
(global-unset-key (kbd "s-p"))     ; Don't print

;; We need Emacs kill ring and system clipboard to be independent. Simpleclip is the solution to that.
(simpleclip-mode 1)

;; Things you'd expect from macOS app.
(global-set-key (kbd "s-s") 'save-buffer)             ;; save
(global-set-key (kbd "s-S") 'write-file)              ;; save as
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs) ;; quit
(global-set-key (kbd "s-a") 'mark-whole-buffer)       ;; select all
(global-set-key (kbd "s-z") 'undo)

;; Delete trailing spaces and add new line in the end of a file on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

;; Linear undo and redo.
(global-set-key (kbd "s-z")   'undo-fu-only-undo)
(global-set-key (kbd "s-Z") 'undo-fu-only-redo)

;; Enable transparent title bar on macOS
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . light)) ;; {light, dark}
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; Font
(when (member "menlo" (font-family-list))
  (set-face-attribute 'default nil :font "Menlo 15"))
(setq-default line-spacing 2)

;; Nice and simple default light theme.
(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'tsdh-light t))
    ('dark (load-theme 'tsdh-dark t))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

;; Set colors to distinguish between active and inactive windows
(set-face-attribute 'mode-line nil :background "SlateGray1")
(set-face-attribute 'mode-line-inactive nil :background "grey93")

;; Hide toolbar and scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Always wrap lines
(global-visual-line-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)

;; Show vi-like tilde in the fringe on empty lines.
(global-vi-tilde-fringe-mode 1)

;; Show full path in the title bar.
(setq-default frame-title-format "%b (%f)")

;; Never use tabs, use spaces instead.
(setq tab-width 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq c-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 2)
(setq-default tab-width 2)
(setq-default c-basic-indent 2)

;; Show keybindings cheatsheet
(which-key-mode)
(setq which-key-idle-delay 0.5)

;; Disable blinking cursor.
(blink-cursor-mode 0)

;; Move around with Cmd+i/j/k/l. This is not for everybody, and it takes away four very well placed
;; key combinations, but if you get used to using these keys instead of arrows, it will be worth it,
;; I promise.
(global-set-key (kbd "s-i") 'previous-line)
(global-set-key (kbd "s-k") 'next-line)
(global-set-key (kbd "s-j") 'left-char)
(global-set-key (kbd "s-l") 'right-char)$1;; Kill line with CMD-Backspace. Note that thanks to Simpleclip, killing doesn't rewrite the system clipboard.
;; Kill one word with Alt+Backspace.
;; Kill forward word with Alt-Shift-Backspace.
(global-set-key (kbd "s-<backspace>") 'kill-whole-line)
(global-set-key (kbd "M-S-<backspace>") 'kill-word)$1;; Use Cmd for movement and selection.
(global-set-key (kbd "s-<right>") (kbd "C-e"))        ;; End of line
(global-set-key (kbd "S-s-<right>") (kbd "C-S-e"))    ;; Select to end of line
(global-set-key (kbd "s-<left>") (kbd "M-m"))         ;; Beginning of line (first non-whitespace character)
(global-set-key (kbd "S-s-<left>") (kbd "M-S-m"))     ;; Select to beginning of line

(global-set-key (kbd "s-<up>") 'beginning-of-buffer)  ;; First line
(global-set-key (kbd "s-<down>") 'end-of-buffer)      ;; Last line$1;; Thanks to Bozhidar Batsov
;; http://emacsredux.com/blog/2013/]05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)
(global-set-key (kbd "s-<left>") 'smarter-move-beginning-of-line)

;; Many commands in Emacs write the current position into mark ring.
;; These custom functions allow for quick movement backward and forward.
;; For example, if you were editing line 6, then did a search with Cmd+f, did something and want to come back,
;; press Cmd+, to go back to line 6. Cmd+. to go forward.
;; These keys are chosen because they are the same buttons as < and >, think of them as arrows.
(defun my-pop-local-mark-ring ()
  (interactive)
  (set-mark-command t))

(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(global-set-key (kbd "s-,") 'my-pop-local-mark-ring)
(global-set-key (kbd "s-.") 'unpop-to-mark-command)

;; Same keys with Shift will move you back and forward between open buffers.
(global-set-key (kbd "s-<") 'previous-buffer)
(global-set-key (kbd "s->") 'next-buffer)

;; Expand-region allows to gradually expand selection inside words, sentences, expressions, etc.
(global-set-key (kbd "s-'") 'er/expand-region)         ;; Cmd+' (apostrophe) to expand
(global-set-key (kbd "s-\"") 'er/contract-region)     ;; Cmd+" (same, but with shift) to contract

;; Move-text lines around with meta-up/down.
(move-text-default-bindings)

;; Quickly insert new lines above or below the current line, with correct indentation.
(defun smart-open-line ()
  "Insert an empty line after the current line. Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun smart-open-line-above ()
  "Insert an empty line above the current line. Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "s-<return>") 'smart-open-line)            ;; Cmd+Return new line below
(global-set-key (kbd "s-S-<return>") 'smart-open-line-above)    ;; Cmd+Shift+Return new line above

;; Upcase and lowercase word or region, if selected.
;; To capitalize or un-capitalize word use Alt+c and Alt+l
(global-set-key (kbd "M-u") 'upcase-dwim)   ;; Alt+u upcase
(global-set-key (kbd "M-l") 'downcase-dwim) ;; Alt-l lowercase

;; Comment line or region.
(global-set-key (kbd "s-/") 'comment-line)

;; Visually find and replace text
(define-key global-map (kbd "M-s-f") 'vr/replace)
(define-key global-map (kbd "s-r") 'vr/replace)  ;; Cmd+r find and replace$1;; Multiple cursors. Similar to Sublime or VS Code.
(setq mc/always-run-for-all 1)
(global-set-key (kbd "s-d") 'mc/mark-next-like-this)        ;; Cmd+d select next occurrence of region
(global-set-key (kbd "s-D") 'mc/mark-all-dwim)              ;; Cmd+Shift+d select all occurrences
(global-set-key (kbd "M-s-d") 'mc/edit-beginnings-of-lines) ;; Alt+Cmd+d add cursor to each line in region
;; (define-key mc/keymap (kbd "<return>") nil)

;; This is rather radical, but saves from a lot of pain in the ass.
;; When split is automatic, always split windows vertically
(setq split-height-threshold 0)
(setq split-width-threshold nil)$1;; Go to other windows easily with one keystroke Cmd-something.
(global-set-key (kbd "s-1") (kbd "C-x 1"))  ;; Cmd-1 kill other windows (keep 1)
(global-set-key (kbd "s-2") (kbd "C-x 2"))  ;; Cmd-2 split horizontally
(global-set-key (kbd "s-3") (kbd "C-x 3"))  ;; Cmd-3 split vertically
(global-set-key (kbd "s-0") (kbd "C-x 0"))  ;; Cmd-0...
(global-set-key (kbd "s-w") (kbd "C-x 0"))  ;; ...and Cmd-w to close current window$1;; Move between windows with Control-Command-Arrow and with =Cmd= just like in iTerm.
(global-set-key (kbd "<C-s-left>")  'windmove-left)  ;; Ctrl+Cmd+left go to left window
(global-set-key (kbd "s-[")  'windmove-left)         ;; Cmd+[ go to left window

(global-set-key (kbd "<C-s-right>") 'windmove-right) ;; Ctrl+Cmd+right go to right window
(global-set-key (kbd "s-]")  'windmove-right)        ;; Cmd+] go to right window

(global-set-key (kbd "<C-s-up>")    'windmove-up)    ;; Ctrl+Cmd+up go to upper window
(global-set-key (kbd "s-{")  'windmove-up)           ;; Cmd+Shift+[ go to upper window

(global-set-key (kbd "<C-s-down>")  'windmove-down)  ;; Ctrl+Cmd+down go to down window
(global-set-key (kbd "s-}")  'windmove-down)        ;; Cmd+Shift+] got to down window$1;; Enable winner mode to quickly restore window configurations
(winner-mode 1)
(global-set-key (kbd "M-s-[") 'winner-undo)
(global-set-key (kbd "M-s-]") 'winner-redo)

;; Use minimalist Ivy for most things.
(ivy-mode 1)                          ;; enable Ivy everywhere
(setq ivy-use-virtual-buffers t)      ;; show bookmarks and recent files in buffer list
(setq ivy-count-format "(%d/%d) ")
(setq enable-recursive-minibuffers t)

(setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t      . ivy--regex-fuzzy)))   ;; enable fuzzy searching everywhere except for Swiper

(global-set-key (kbd "s-b") 'ivy-switch-buffer)  ;; Cmd+b show buffers and recent files
(global-set-key (kbd "M-s-b") 'ivy-resume)      ;; Alt+Cmd+b resume whatever Ivy was doing

;; Swiper is a better local finder.
(global-set-key "\C-s" 'swiper)       ;; Default Emacs Isearch forward...
(global-set-key "\C-r" 'swiper)       ;; ... and Isearch backward replaced with Swiper
(global-set-key (kbd "s-f") 'swiper) ;; Cmd+f find text

;; Better menus with Counsel (a layer on top of Ivy)
(global-set-key (kbd "M-x") 'counsel-M-x)            ;; Alt+x run command
(global-set-key (kbd "s-P") 'counsel-M-x)            ;; Cmd+Shift+p run command
(global-set-key (kbd "C-x C-f") 'counsel-find-file)  ;; Replace built-in Emacs 'find file' (open file) with Counsel
(global-set-key (kbd "s-o") 'counsel-find-file)     ;; Cmd+o open file

;; Make Ivy a bit more friendly by adding information to ivy buffers, e.g. description of commands in Alt-x, meta info when switching buffers, etc.
(ivy-rich-mode 1)
(setq ivy-rich-path-style 'abbrev) ;; Abbreviate paths using abbreviate-file-name (e.g. replace “/home/username” with “~”)

;; Integrate Projectile with Counsel
(counsel-projectile-mode 1)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "s-p") 'counsel-projectile-find-file)         ;; Cmd+p open file in current project
(global-set-key (kbd "s-F") 'counsel-projectile-rg)     ;; Cmd+Shift+F search in current git repository

(setq projectile-completion-system 'ivy)             ;; Use Ivy in Projectile

;; Magit
(global-set-key (kbd "s-g") 'magit-status)   ;; Cmd+g for git status

;; Show changes in the gutter
(global-git-gutter-mode 't)
(set-face-background 'git-gutter:modified 'nil)   ;; background color
(set-face-foreground 'git-gutter:added "green4")
(set-face-foreground 'git-gutter:deleted "red")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application eshell)
      eshell-connection-default-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile
      (eshell-path-env-list))))
 '(package-selected-packages '(evil))
 '(shell-pop-shell-type
   '("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell))))
 '(shell-pop-universal-key "s-="))

(setq company-idle-delay 0.1)
(setq company-global-modes '(not org-mode))
(setq company-minimum-prefix-length 1)
(add-hook 'after-init-hook 'global-company-mode)

;; Some basic Org defaults
(use-package org
  :config
  (setq org-startup-indented t)         ;; Visually indent sections. This looks better for smaller files.
  (setq org-src-tab-acts-natively t)    ;; Tab in source blocks should act like in major mode
  (setq org-src-preserve-indentation t)
  (setq org-log-into-drawer t)          ;; State changes for todos and also notes should go into a Logbook drawer
  (setq org-src-fontify-natively t)     ;; Code highlighting in code blocks
  (setq org-log-done 'time)             ;; Add closed date when todo goes to DONE state
  (setq org-support-shift-select t))    ;; Allow shift selection with arrows.

;; Store all my org files in ~/org.
(setq org-directory "~/org")

;; And all of those files should be in included agenda.
(setq org-agenda-files '("~/org"))

;; Open config file by pressing C-x and then C
(global-set-key (kbd "C-x C") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; Open private config file by pressing C-x and then c
;; Contain custom settings to private.el to ensure easy Castlemacs updates.
(global-set-key (kbd "C-x c") (lambda () (interactive) (find-file "~/.emacs.d/private.el")))

(evil-mode 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
