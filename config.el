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

(map! :leader
      :desc "Highlight"
      "a h" #'(lambda () (interactive) (highlight-symbol)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org-mode")
(setq org-journal-dir (concat org-directory "/journal"))


; Journal
(defconst org-mode-daily-file
    (let ((daily-name (format-time-string "%Y-%m-%d [%a]")))
    (expand-file-name (concat org-journal-dir "/" daily-name ".org")))
    "Today's org mode journal file")

(defconst org-mode-weekly-file
    (let ((weekly-name (format-time-string "%Y-%m W%W")))
    (expand-file-name (concat org-journal-dir "/" weekly-name ".org")))
    "Week's org mode journal file")

(defconst org-mode-monthly-file
    (let ((monthly-name (format-time-string "%Y-%m")))
    (expand-file-name (concat org-journal-dir "/" monthly-name ".org")))
    "Month's org mode journal file")

(map! :desc "Daily Journal"
    "M-n d" #'(lambda () (interactive) (find-file org-mode-daily-file))
    :desc "Weekly Journal"
    "M-n w" #'(lambda () (interactive) (find-file org-mode-weekly-file))
    :desc "Monthly Journal"
    "M-n m" #'(lambda () (interactive) (find-file org-mode-monthly-file)))

(after! org
  (require 'org-bullets)  ; Nicer bullets in org-mode
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

  (setq org-agenda-files (list
                         (concat org-directory "/work/xplm/projects")
                         (concat org-directory "/work/xplm/ecad")
                         (concat org-directory "/work/xplm/ecad/customers")
                         (concat org-directory "/agenda")
                         (concat org-directory "/work/xplm/clients")
                         ))

  (setq org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-log-done 'time
        org-use-property-inheritance t
        org-journal-dir (concat org-directory "/journal")
        org-journal-date-format "%B %d, %Y (%A)"
        org-journal-file-format "%Y-%m-%d.org"
        org-hide-emphasis-markers t
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
             "WAIT(w)"           ; Something is holding up this task
             "FUTURE(f)"         ; To be completed in the unspecified future
             "|"                 ; The pipe necessary to separate "active" states and "inactive" states
             "DONE(d)"           ; Task has been completed
             "CANCELLED(c)" )))) ; Task has been cancelled

(setq xplm-customers '("insitu" "ddc" "intuitive_surgical" "molex" "telestream"))
(defun org-agenda-prop-search (property value)
  "Show TODOs that have match PROPERTY = VALUE"
  (org-tags-view t (format "%s=\"%s\"/TODO" property value)))
  ;(let ((org-use-property-inheritance
         ;(append org-use-property-inheritance '(property)))
        ;)
    ;(org-tags-view t (format "%s=\"%s\"/TODO" property value))
    ;)
  ;)

(defun org-agenda-prop-search-interactive(key list)
  "Search for VALUE in property KEY; interactively set VALUE"
  (let ((value (completing-read (format "%s: " key) list)))
    (org-agenda-prop-search key value)))

(map! :desc "Agenda View"
      "<f12>" #'(lambda () (interactive) (org-agenda-prop-search-interactive "customer" xplm-customers)))

(map! :leader
      :desc "List bookmarks"
      "b L" 'list-bookmarks
      :leader
      :desc "Save current bookmarks to bookmark file"
      "b w" 'bookmark-save)

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

(setq doom-font (font-spec :family "Source Code Variable" :size 14)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 14)
      doom-big-font (font-spec :family "Source Code Variable" :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq doom-theme 'doom-one)
(map! :leader
      :desc "Load new theme"
      "h t" #'counsel-load-theme)

(map! :leader
      :desc "root org"
      "j d o" #'(lambda () (interactive) (dired org-directory))
      :leader
      :desc "root work"
      "j d w w" #'(lambda () (interactive) (dired (concat org-directory "/work/xplm")))
      :leader
      :desc "work ecad"
      "j d w e" #'(lambda () (interactive) (dired (concat org-directory "/work/xplm/ecad")))
      :leader
      :desc "work time"
      "j f w t" #'(lambda () (interactive) (find-file (concat org-directory "/work/xplm/time-tracking.org")))
      :leader
      :desc "Edit doom config.org"
      "j f c" #'(lambda () (interactive) (find-file "~/.doom.d/config.org"))
      :leader
      :desc "Edit eshell aliases"
      "j f e" #'(lambda () (interactive) (find-file "~/.doom.d/aliases"))
      :leader
      :desc "Edit doom init.el"
      "j f i" #'(lambda () (interactive) (find-file "~/.doom.d/init.el"))
      :leader
      :desc "Edit doom packages.el"
      "j f p" #'(lambda () (interactive) (find-file "~/.doom.d/packages.el")))

(after! yasnippet
  (setq yas--default-user-snippets-dir "~/.doom.d/snippets"))
