;; Expected files:
;; * gtd.org with level 1: Tasks, Projects
;;
;;   Projects with level 2: elisp, function-args, tiny.el, el-TeX,
;;   Worf, Scientific Articles
;;
;; * ent.org with level 1: Articles, Videos
;; * wiki/stack.org with level 1: Questions

;;* base directory
(defun orfu-expand (name)
  (expand-file-name name "~/Dropbox/org"))

(defcustom orfu-github-project-name
  "https://github\\.com/abo-abo/\\([^/]+\\)"
  "Regex for Github repository projects."
  :type 'string)

;;* capture
;;** basic
(require 'org-capture)
;; http://orgmode.org/manual/Capture-templates.html#Capture-templates
(setq org-capture-templates
      '(("t" "TODO" entry (file+headline (orfu-expand "gtd.org") "Tasks")
         "* TODO %^{Brief Description}\nAdded: %U\n%?\n")
        ("b" "Buffer" entry (file+headline (orfu-expand "gtd.org") "Tasks")
         "* TODO %a")))
;;** project
(defvar orfu-project-list
  '(("ELISP" "e" "elisp")
    ("FARGS" "f" "function-args")
    ("WORF" "w" "worf")
    ("LISPY" "y" "lispy"))
  "List of projects in gtd.org in '(tag key description) format.")

(mapc
 (lambda (project)
   (add-to-list
    'org-capture-templates
    (destructuring-bind (tag key name) project
      `(,key ,name entry (file+olp (orfu-expand "gtd.org") "Projects" ,name)
             ,(format
               "* TODO %%^{Brief Description}  :%s:\nAdded: %%U  %%i\n  %%?\n"
               tag)
             :clock-in t :clock-resume t))))
 orfu-project-list)
;;** PDF
(push
 '("p" "Pdf article" entry (file+olp (orfu-expand "gtd.org") "Projects" "Scientific Articles")
   "* TODO Read %(orfu-process-current-pdf)%(org-set-tags-to\"OFFICE\")\nAdded: %U %i\n  %?\n")
 org-capture-templates)

(require 'org-attach)
(defun orfu-process-current-pdf ()
  (let* ((buffer (org-capture-get :buffer))
         (buffer-mode (with-current-buffer buffer major-mode))
         (filename (org-capture-get :original-file)))
    (when (file-directory-p filename)
      (with-current-buffer (org-capture-get :original-buffer)
        (setq filename (dired-get-filename))))
    (when (or (string= (file-name-extension filename) "pdf")
              (string= (file-name-extension filename) "djvu"))
      (let ((org-attach-directory (orfu-expand "data/"))
            (name (file-name-sans-extension
                   (file-name-nondirectory filename))))
        (org-attach-attach filename nil 'cp)
        (if (string-match "\\[\\(.*\\)\\] \\(.*\\)(\\(.*\\))" name)
            (format "\"%s\" by %s"
                    (match-string 2 name)
                    (match-string 1 name))
          name)))))
;;** protocol
(require 'org-protocol)
(setq org-protocol-default-template-key "l")
(push '("l" "Link" entry (function orfu-handle-link)
        "* TODO %(orfu-wash-link)\nAdded: %U\n%(orfu-link-hooks)\n%?")
      org-capture-templates)

(defun orfu-wash-link ()
  "Return a pretty-printed top of `org-stored-links'.
Try to remove superfluous information, like website title."
  (let ((link (caar org-stored-links))
        (title (cadar org-stored-links)))
    (org-make-link-string
     link
     (replace-regexp-in-string " - Stack Overflow" "" title))))

(defvar orfu-link-hook nil)

(defun orfu-link-hooks ()
  (prog1
      (mapconcat #'funcall
                 orfu-link-hook
                 "\n")
    (setq orfu-link-hook nil)))

(defun orfu-raise-frame ()
  (if (eq system-type 'gnu/linux)
      (call-process
       "wmctrl" nil nil nil "-i" "-R"
       (frame-parameter (selected-frame) 'outer-window-id))
    (raise-frame)))

(defun orfu-handle-link ()
  (orfu-raise-frame)
  (let ((link (caar org-stored-links))
        (title (cadr (car org-stored-links)))
        file)
    (cond ((string-match "^https://www.youtube.com/" link)
           (orfu-handle-link-youtube link))
          ((string-match "^https://scholar.google.com/scholar.bib" link)
           (url-retrieve
            link
            (lambda (status)
              (let ((err (plist-get status :error)))
                (if err (error
                         "\"%s\" %s" link
                         (downcase (nth 2 (assq (nth 2 err) url-http-codes)))))
                (message (buffer-substring-no-properties
                          (point-min)
                          (point-max)))))
            nil nil t))
          ((string-match (regexp-quote "http://stackoverflow.com/") link)
           (find-file (orfu-expand "wiki/stack.org"))
           (goto-char (point-min))
           (re-search-forward "^\\*+ +Questions" nil t))
          ((string-match orfu-github-project-name link)
           (let ((project-name (match-string 1 link))
                 (parts (split-string title "·")))
             (setf (cl-cadar org-stored-links)
                   (concat (car parts)
                           (substring (cadr parts) 7)))
             (find-file (orfu-expand "wiki/github.org"))
             (goto-char (point-min))
             (re-search-forward (concat "^\\*+ +" project-name) nil t)))
          (t
           (find-file (orfu-expand "ent.org"))
           (goto-char (point-min))
           (re-search-forward "^\\*+ +Articles" nil t)))))

(require 'async)
(defun orfu-handle-link-youtube (link)
  (lexical-let*
      ((file-name (org-trim
                   (shell-command-to-string
                    (concat
                     "youtube-dl \""
                     link
                     "\""
                     " -o \"%(title)s.%(ext)s\" --get-filename"))))
       (dir "~/Downloads/Videos")
       (full-name
        (expand-file-name file-name dir)))
    (add-hook 'orfu-link-hook
              (lambda ()
                (concat
                 (org-make-link-string dir dir)
                 "\n"
                 (org-make-link-string full-name file-name))))
    (async-shell-command
     (format "youtube-dl \"%s\" -o \"%s\"" link full-name))
    (find-file (orfu-expand "ent.org"))
    (goto-char (point-min))
    (re-search-forward "^\\*+ +Videos" nil t)))

;;** agenda
(defun orfu-tags-projects ()
  (mapcar (lambda(x) `(tags-todo ,(car x)))
          orfu-project-list))

(setq org-agenda-custom-commands
      `(("n" "Agenda and all TODO's" ((agenda "") (alltodo "")))
        ("h" "Office and Outside and Home"
             ((agenda)
              (tags-todo "OFFICE")
              (tags-todo "OUTSIDE")
              ,@(orfu-tags-projects)
              (tags-todo "HOME")))
        ("r" "Articles"
             ((tags-todo "article")
              (tags-todo "MASTERINGEMACS")))
        ("o" "Office and Outside"
             ((agenda)
              (tags-todo "OFFICE")
              (tags-todo "OUTSIDE")
              ,@(orfu-tags-projects)))
        ("d" "Daily Action List"
             ((agenda ""
                      ((org-agenda-ndays 1)
                       (org-agenda-sorting-strategy
                        (quote ((agenda time-up priority-down tag-up))))
                       (org-deadline-warning-days 0)))))
        ("P" "Project List"
             ((tags "PROJECT")))))

;;;###autoload
(defun orfu-agenda-quick ()
  (interactive)
  (org-agenda nil "h"))

;;;###autoload
(defun orfu-agenda-office ()
  (interactive)
  (org-agenda nil "o"))

;;;###autoload
(defun orfu-agenda-day ()
  (interactive)
  (org-agenda nil "d"))

;;;###autoload
(defun orfu-agenda-articles ()
  (interactive)
  (org-agenda nil "r"))

(provide 'org-fu)

;;; org-fu.el ends here
