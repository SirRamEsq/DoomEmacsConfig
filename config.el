; Toggle this to enable / disable debugging on error
; (setq debug-on-error t)
(setq debug-on-error nil)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ryan Lloyd"
      user-mail-address "gmrl95@gmail.com")
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

; XPLM ECAD File Associations
(add-to-list 'auto-mode-alist '("\\.classification\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.datamodel\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.release\\'" . ruby-mode))

; Visual Basic File Associations
(add-to-list 'auto-mode-alist '("\\.vb\\'" . visual-basic-mode))

(require 'gdscript-mode)

(map! :leader
      :desc "Highlight"
      "a h" #'(lambda () (interactive) (highlight-symbol)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org-mode")
(setq org-journal-dir (concat org-directory "/journal"))
(setq obsidian-journal-dir (concat "~/Dropbox/Dropsyncfiles/ObsidianVault/V1/Journal"))


(defun daily-name   (&optional time) (format-time-string "%Y-%m-%d (%a)" time))
(defun weekly-name  (&optional time) (format-time-string "%Y-W%V" time))
(defun monthly-name (&optional time) (format-time-string "%Y-%m" time))
(defun year-name    (&optional time) (format-time-string "%Y" time))


(defun days-from-now (days)
    (let* ((final-date (decode-time)))
        (cl-incf (nth 3 final-date) days)
        (apply #'encode-time final-date)))

; TODO sets day to first of the month so we don't blow past an enitre month with fewer days
; ie. it's Jan 29th and next month is Feb, if we don't change the day encode-time will interperet as March
(defun months-from-now (months)
    (let* ((final-date (decode-time)))
         ; Set day to First of the month
         (setf (nth 3 final-date) 1)
         (cl-incf (nth 4 final-date) months)
         (apply #'encode-time final-date)))

; Journal

(defconst org-mode-daily-file
    (expand-file-name (concat org-journal-dir "/" (year-name) "/" (daily-name) ".org"))
    "Today's org mode journal file")

(defconst org-mode-weekly-file
    (expand-file-name (concat org-journal-dir "/" (year-name) "/" (weekly-name) ".org"))
    "Week's org mode journal file")

(defconst org-mode-monthly-file
    (expand-file-name (concat org-journal-dir "/" (year-name) "/" (monthly-name) ".org"))
    "Month's org mode journal file")

(defconst org-mode-weekly-report-file
    (expand-file-name (concat org-journal-dir "/" (year-name) "/" (weekly-name) " Report.org"))
    "Month's org mode journal file")

(defun org-mode-future-daily-file (days)
    "Future Day's org mode journal file"
    (interactive)
    (expand-file-name (concat org-journal-dir "/" (year-name) "/"  (daily-name (days-from-now days)) ".org")))

(defun org-mode-future-weekly-file (weeks)
    "Future Week's org mode journal file"
    (interactive)
    (expand-file-name (concat org-journal-dir "/" (year-name) "/" (weekly-name (days-from-now (* weeks 7))) ".org")))

(defun org-mode-future-monthly-file (months)
    "Future Month's org mode journal file"
    (interactive)
    (expand-file-name (concat org-journal-dir "/" (year-name) "/"(monthly-name (months-from-now months)) ".org")))

(defconst obsidian-mode-daily-file
    (expand-file-name (concat obsidian-journal-dir "/" (year-name) "/" (daily-name) ".md"))
    "Today's obsidian mode journal file")

(defconst obsidian-mode-weekly-file
    (expand-file-name (concat obsidian-journal-dir "/" (year-name) "/" (weekly-name) ".md"))
    "Week's obsidian mode journal file")

(defconst obsidian-mode-monthly-file
    (expand-file-name (concat obsidian-journal-dir "/" (year-name) "/" (monthly-name) ".md"))
    "Month's obsidian mode journal file")

(defconst obsidian-mode-weekly-report-file
    (expand-file-name (concat obsidian-journal-dir "/" (year-name) "/" (weekly-name) " Report.md"))
    "Month's obsidian mode journal file")

(defun obsidian-mode-future-daily-file (days)
    "Future Day's obsidian mode journal file"
    (interactive)
    (expand-file-name (concat obsidian-journal-dir "/" (year-name) "/"  (daily-name (days-from-now days)) ".md")))

(defun obsidian-mode-future-weekly-file (weeks)
    "Future Week's obsidian mode journal file"
    (interactive)
    (expand-file-name (concat obsidian-journal-dir "/" (year-name) "/" (weekly-name (days-from-now (* weeks 7))) ".md")))

(defun obsidian-mode-future-monthly-file (months)
    "Future Month's obsidian mode journal file"
    (interactive)
    (expand-file-name (concat obsidian-journal-dir "/" (year-name) "/"(monthly-name (months-from-now months)) ".md")))



(after! org
  (require 'org-bullets)  ; Nicer bullets in org-mode
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

  (setq org-agenda-files (list
                         ;(concat org-directory "/work/xplm/projects")
                         ;(concat org-directory "/work/xplm/ecad")
                         ;(concat org-directory "/work/xplm/ecad/customers")
                         ;(concat org-directory "/work/xplm/clients")
                         (concat org-directory "/agenda")
                         ))

  (setq org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-log-done 'time
        org-use-property-inheritance t
        org-journal-dir (concat org-directory "/journal")
        org-journal-date-format "%B %d, %Y (%A)"
        org-journal-file-format "%Y-%m-%d.org"
        +org-capture-todo-file (concat org-directory "/agenda/todo.org")
        ;org-startup-folded t
        org-hide-emphasis-markers nil
        browse-url-browser-function 'browse-url-default-browser
        ;; ex. of org-link-abbrev-alist in action
        ;; [[arch-wiki:Name_of_Page][Description]]
        org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
          '(("google" . "http://www.google.com/search?q=")
            ("ddg" . "https://duckduckgo.com/?q=")
            ("wiki" . "https://en.wikipedia.org/wiki/"))
        org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
          '((sequence
             "TODO(t)"           ; A task that is ready to be tackled
             "NEXT(n)"           ; Task is next in line to be done
             "WAIT(w)"           ; Something is holding up this task
             "TEST(T)"           ; Might be done, but needs tested
             "SOMEDAY(s)"        ; Might do it eventually?
             "FUTURE(f)"         ; To be completed in the unspecified future
             "|"                 ; The pipe necessary to separate "active" states and "inactive" states
             "DONE(d)"           ; Task has been completed
             "DELEGATED(z)"      ; Task is someone else's responsibility
             "CANCELLED(c)" )))) ; Task has been cancelled

; Helpful example code
; https://github.com/kandread/doom-emacs-private/blob/master/%2Bgtd.el
(after! org
    (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "#dd7777" :weight bold)
                ("NEXT" :foreground "yellow" :weight bold)
                ("WAIT" :foreground "yellow" :weight bold)
                ("TEST" :foreground "cyan" :weight bold)
                ("SOMEDAY" :foreground "purple" :weight bold)
                ("FUTURE" :foreground "purple" :weight bold)
                ("DONE" :foreground "green" :weight bold)
                ("DELEGATED" :foreground "green" :weight bold)
                ("CANCELLED" :foreground "green" :weight bold)))))

;; Date Tree
;(setq-default org-reverse-datetree-level-formats
;              '("%Y"                    ; year
;                (lambda (time) (format-time-string "%Y-%m %B" (org-reverse-datetree-monday time))) ; month
;                "%Y W%W"                ; week
;                "%Y-%m-%d %A"           ; date
;                ))
(setq-default org-reverse-datetree-level-formats'("%Y W%W")) ; Only week; Year and month are file specific

; Define method to archive to a specific file based on the year and month
(defun custom-archive-command (&rest arg)
  ;(interactive "P")
  (interactive)
  (let ((time-stamp (org-reverse-datetree--get-entry-time
                     ;:ask-always t
                     :prefer`("CLOSED" "DEADLINE" "SCHEDULED"))))
  (org-reverse-datetree-refile-to-file
   (concat org-directory "/agenda-archive/" (monthly-name time-stamp) ".org")
   time-stamp)))

(setq org-archive-default-command 'custom-archive-command)

(map! :leader
      :map org-mode-map
      :desc "Archive"
      "m A" #'(lambda () (interactive) (org-archive-subtree-default)))

; Ensure Package is loaded after org
; Will also immediately load org on startup without additional args
(use-package! org-reverse-datetree)
  ;:hook (org-mode-hook))

(setq xplm-customers '("xplm" "insitu" "ddc" "telestream"))
(defun org-agenda-prop-search (property value)
  "Show TODOs that have match PROPERTY = VALUE"
  (org-tags-view t (format "%s=\"%s\"/TODO" property value)))
  ;(let ((org-use-property-inheritance
         ;(append org-use-property-inheritance '(property)))
        ;)
    ;(org-tags-view t (format "%s=\"%s\"/TODO" property value))
    ;)
  ;)

; See here for more https://orgmode.org/worg/org-tutorials/advanced-searching.html
(defun org-agenda-prop-search-interactive(key list)
  "Search for VALUE in property KEY; interactively set VALUE"
  (let ((value (completing-read (format "%s: " key) list)))
    (org-agenda-prop-search key value)))

;(map! :desc "Agenda View"
      ;"<f12> p" #'(lambda () (interactive) (org-tags-view t "-@XPLM"))
      ;"<f12> w w" #'(lambda () (interactive) (org-tags-view t "@XPLM"))
      ;"<f12> w p" #'(lambda () (interactive) (org-agenda-prop-search-interactive "customer" xplm-customers)))

; Use evil keys instead of having super-agenda overwrite
; https://github.com/alphapapa/org-super-agenda/issues/112
(setq org-super-agenda-header-map nil)
(org-super-agenda-mode)

; For some dumb reason, %s is ignored in org-agenda-prefix-format
; https://stackoverflow.com/questions/58820073/s-in-org-agenda-prefix-format-doesnt-display-dates-in-the-todo-view
; Using a custom expression instead
(setq org-agenda-custom-expression-scheduled '"%(let ((scheduled (org-get-scheduled-time (point)))) (if scheduled (format-time-string \"%Y-%m-%d\" scheduled) \"\")) ")
(setq org-agenda-custom-expression-deadline '"%(let ((deadline (org-get-deadline-time (point)))) (if deadline (format-time-string \"%Y-%m-%d\" deadline) \"\")) ")
(setq org-agenda-custom-commands
      `(("P" "Personal view Everything"
         ((agenda "" (
                      (org-agenda-span 'day)        ; Daily Agenda
                      (org-deadline-warning-days 7) ; 7 day advanced warning for deadlines
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date today
                                :todo "TODAY"
                                :or (
                                  :scheduled (before ,(format-time-string "%Y-%m-%d %a" (days-from-now 1)))
                                  :deadline (before ,(format-time-string "%Y-%m-%d %a" (days-from-now 1)))
                                )
                                :order 1)))))
          (alltodo "" (
                       (org-agenda-overriding-header "Everything TODO-List")
                       (org-agenda-prefix-format (concat "📌 [%c] " org-agenda-custom-expression-scheduled " " org-agenda-custom-expression-deadline " "))
                       ;(org-agenda-prefix-format "📌 [%c] ")
                       (org-super-agenda-groups
                        ; BACKTICK, not single quote
                        ; if single quote is used, cannot use back quote to eval list expressions
                        `(
                          (:discard (:tag ("Chore" "Daily")))
                          (:name "Trivial"
                                :priority<= "C"
                                :tag ("TRIVIAL" "UNIMPORTANT")
                                :todo ("SOMEDAY" )
                                :order 1000)
                          (:name "Next to do"
                                :todo "NEXT"
                                :face (:background "black" :underline t))
                          (:name "Overdue"
                                :face (:background "black" :underline t)
                                :deadline past)
                          (:name "Due Today"
                                :face (:background "black" :underline t)
                                :deadline today)
                          (:name "Important"
                                :tag "Important"
                                :and(
                                    :not(:category "Social")
                                    :priority "A"
                                    )
                                :face (:background "black" :underline t))
                          (:name "Low-Hanging Fruit"
                                :effort< "0:30")
                          (:name "Due Soon"
                                ; Use comma (backquote) to force eval
                                :deadline (before ,(format-time-string "%Y-%m-%d %a" (days-from-now 21))))
                          (:name "Scheduled soon"
                                ; Use comma (backquote) to force eval
                                :scheduled (before ,(format-time-string "%Y-%m-%d %a" (days-from-now 21))))
                          (:name "Tasks"
                                  :category "Tasks")
                          (:name "Social"
                                  :category "Social")
                          (:auto-category t)
                          ))))))

      ("p" "Personal view"
          ((alltodo "" (
                       (org-agenda-overriding-header "TODO-List")
                       (org-agenda-prefix-format (concat "📌 [%c] " org-agenda-custom-expression-scheduled " " org-agenda-custom-expression-deadline " "))
                       ;(org-agenda-prefix-format "📌 [%c] ")
                       (org-super-agenda-groups
                        ; BACKTICK, not single quote
                        ; if single quote is used, cannot use back quote to eval list expressions
                        `(
                          (:discard (:tag ("Chore" "Daily")))
                          (:name "Trivial"
                                :priority<= "C"
                                :tag ("TRIVIAL" "UNIMPORTANT")
                                :todo ("SOMEDAY" )
                                :order 1000)
                          (:name "Next to do"
                                :todo "NEXT"
                                :face (:background "black" :underline t))
                          (:name "Important"
                                :tag "Important"
                                :and(
                                    :not(:category "Social")
                                    :priority "A"
                                    )
                                :face (:background "black" :underline t))
                          (:name "Low-Hanging Fruit"
                                :effort< "0:30")
                          (:name "Tasks"
                                  :category "Tasks")
                          ;(:name "Social"
                                  ;:category "Social")
                          (:discard (:anything))
                          ))))))))

(map! :desc "Agenda View"
      "<f12>" #'(lambda () (interactive) (org-agenda "p")))

(map! :leader
      :desc "List bookmarks"
      "b L" 'list-bookmarks
      :leader
      :desc "Save current bookmarks to bookmark file"
      "b w" 'bookmark-save)

(defun xah-open-in-external-app (&optional @fname)
  "Open the current file or dired marked files in external app.
When called in emacs lisp, if @fname is given, open that.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-11-04 2021-02-16"
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (shell-command (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'" (shell-quote-argument (expand-file-name $fpath )) "'")))
         $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" $fpath))) $file-list))))))


; Dired mode mappings
(map! :leader
      :map dired-mode-map
      :desc "Open in external app"
      "m o" #'(lambda () (interactive) (xah-open-in-external-app)))

(map! :leader
      :desc "Dired"
      "a d d" #'dired
      :leader
      :desc "Dired jump to current"
      "a d j" #'dired-jump
      (:after dired
        (:map dired-mode-map
         :leader
         :desc "Peep-dired image previews"
         "a d p" #'peep-dired
         :leader
         :desc "Dired view file"
         "a d v" #'dired-view-file)))
(evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file
                                             (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(setq
      doom-font (font-spec :family "Source Code Variable" :size 14)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 14)
      doom-big-font (font-spec :family "Source Code Variable" :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DARK THEME FAVORITES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-gruvbox)
;; (setq doom-theme 'doom-horizon)
;; (setq doom-theme 'doom-molokai)
;; (setq doom-theme 'doom-oceanic-next)
;; (setq doom-theme 'doom-tomorrow-night)
(setq doom-theme 'doom-badger)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MID-LIGHT THEME FAVORITES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq doom-theme 'doom-nova)
;;(setq doom-theme 'doom-nord)
;; (setq doom-theme 'doom-spacegrey)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LIGHT THEME FAVORITES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(setq doom-theme 'doom-nord-light)

(map! :leader
      :desc "Load new theme"
      "h t" #'counsel-load-theme)

(map! :leader
      :desc "root org"
      "j d o o" #'(lambda () (interactive) (dired org-directory))
      :desc "org agenda"
      "j d a" #'(lambda () (interactive) (dired (concat org-directory "/agenda")))
      :leader
      :desc "root work"
      "j d w w" #'(lambda () (interactive) (dired (concat org-directory "/work/xplm")))
      ;:leader
      ;:desc "work ecad"
      ;"j d w e" #'(lambda () (interactive) (dired (concat org-directory "/work/xplm/ecad")))
      :leader
      :desc "work time"
      "j f w t" #'(lambda () (interactive) (find-file (concat org-directory "/work/xplm/time-tracking.org")))
      :leader
      :desc "root work"
      "j f w w" #'(lambda () (interactive) (find-file (concat org-directory "/work/xplm/xplm.org")))
      :leader
      :desc "Edit todo.org"
      "j f a" #'(lambda () (interactive) (find-file (concat org-directory "/agenda/todo.org")))
      :leader
      :desc "Edit doom config.org"
      "j f c" #'(lambda () (interactive) (find-file "~/.doom.d/config.org"))
      :leader
      :desc "Edit eshell aliases"
      "j f e" #'(lambda () (interactive) (find-file "~/.doom.d/aliases"))
      :leader
      :desc "Edit doom init.el"
      "j f i" #'(lambda () (interactive) (find-file "~/.doom.d/init.el"))

; Journal
      :leader
      :desc "Daily Journal"
      "j f d" #'(lambda () (interactive) (find-file obsidian-mode-daily-file))
      :leader
      :desc "Weekly Journal"
      "j f w" #'(lambda () (interactive) (find-file obsidian-mode-weekly-file))
      :leader
      :desc "Weekly Report"
      "j f r" #'(lambda () (interactive) (find-file obsidian-mode-weekly-report-file))
      :leader
      :desc "Monthly Journal"
      "j f m" #'(lambda () (interactive) (find-file obsidian-mode-monthly-file))

      :leader
      :desc "Tomorrow Journal"
      "j f n d" #'(lambda () (interactive) (find-file (obsidian-mode-future-daily-file 1)))
      :leader
      :desc "Next Week Journal"
      "j f n w" #'(lambda () (interactive) (find-file (obsidian-mode-future-weekly-file 1)))
      :leader
      :desc "Next Month Journal"
      "j f n m" #'(lambda () (interactive) (find-file (obsidian-mode-future-monthly-file 1)))

      :leader
      :desc "Yesterday Journal"
      "j f p d" #'(lambda () (interactive) (find-file (obsidian-mode-future-daily-file -1)))
      :leader
      :desc "Prev Week Journal"
      "j f p w" #'(lambda () (interactive) (find-file (obsidian-mode-future-weekly-file -1)))
      :leader
      :desc "Prev Month Journal"
      "j f p m" #'(lambda () (interactive) (find-file (obsidian-mode-future-monthly-file -1))))

(after! yasnippet
  (setq yas--default-user-snippets-dir "~/.doom.d/snippets"))

;; add extra snippet directories
(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/Dropbox/emacs-snippets")))

(map! :map yas-keymap
      :desc "Next field"
      "M-j" 'yas-next-field-or-maybe-expand)
(map! :map yas-keymap
      :desc "Prev field"
      "M-k" 'yas-prev-field)

(setq company-backends
      '((company-files          ; files & directory
         company-keywords       ; keywords
         company-capf           ; CompletAtPointFunction defined by major mode
         company-yasnippet      ; Snippets
         company-dabbrev-code   ; Symbols in the current buffer that aren't comments or strings
         )
        (company-abbrev company-dabbrev) ; Backend for the company-abbrev function
        ))

;; Enable Auto-complete globally
(add-hook 'after-init-hook 'global-company-mode)

(setq-default company-idle-delay 0)
(setq-default company-minimum-prefix-length 2) ; Show suggestions after entering characters
(setq-default company-selection-wrap-around t)
; Use tab key to cycle through suggestions.
; ('tng' means 'tab and go')
(company-tng-configure-default)

; Add underscore to gdscrit mode
(add-hook 'gdscript-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
; Add dash to emacs-lisp mode
(add-hook 'emacs-lisp-mode-hook #'(lambda () (modify-syntax-entry ?- "w")))

;;; org-outlook.el - Support for links to Outlook items in Org
;; (require 'org)
(org-add-link-type "outlook" 'org-outlook-open)
(defun org-outlook-open (id)
   "Open the Outlook item identified by ID.  ID should be an Outlook GUID."
   (w32-shell-execute "open" "C:/Outlook" (concat "/select " "outlook:" id))) ;; To use this method, outlook must exist or have a shortcut at the specified location
   ;; To use this method, need to do a registry tweak
   ;(w32-shell-execute "open" (concat "outlook:" id)))

;(provide 'org-outlook)

;;; org-outlook.el ends here

(add-to-list 'auto-mode-alist '("\\.mermaid\\'" . mermaid-mode))
(if (eq system-type 'windows-nt)
    (setq mermaid-mmdc-location "~/.doom.d/dependencies/mermaid-cli/node_modules/.bin/mmdc.cmd")
    (setq mermaid-mmdc-location "~/.doom.d/dependencies/mermaid-cli/node_modules/.bin/mmdc"))
;(setq mermaid-output-format ".png")
;(setq mermaid-tmp-dir "~/.doom.d/mermaid-tmp/")
;(setq mermaid-flags "")

(map! :desc "autoformat"
      "<f10>" #'(lambda () (interactive) (format-all-buffer)))

(obsidian-specify-path "~/Dropbox/DropsyncFiles/ObsidianVault/V1")
;; If you want a different directory of `obsidian-capture':
(setq obsidian-inbox-directory "Inbox")

;; Replace standard command with Obsidian.el's in obsidian vault:
;(bind-key (kbd "C-c C-o") 'obsidian-follow-link-at-point 'obsidian-mode-map)
; (map! :leader
    ; :desc "Follow Link"
    ; :map obsidian-mode-map
    ; "o o" #'obsidian-follow-link-at-point)

;; Use either `obsidian-insert-wikilink' or `obsidian-insert-link':
; (map! :leader
      ; :desc "Insert Link"
      ; :map obsidian-mode-map
      ; "o l" #'obsidian-insert-link)

; (map! :leader
      ; :map obsidian-mode-map
      ; "o c" #'obsidian-capture)

(map! :leader
      :map obsidian-mode-map
      "o o" #'obsidian-hydra/body)
(map! :leader
      :map obsidian-mode-map
      "j o" #'obsidian-jump)

;; Activate detectino of Obsidian vault
(global-obsidian-mode t)
