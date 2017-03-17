;;; org-fu.el --- Org-mode tricks. -*- lexical-binding: t -*-
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
      `(("t" "TODO" entry (file+headline ,(orfu-expand "gtd.org") "Tasks")
             "* TODO %?\nAdded: %T\n")
        ("f" "FILE+TODO" entry (file+headline ,(orfu-expand "gtd.org") "Tasks")
             "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")))
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
        "* TODO %(orfu-wash-link)\nAdded: %T\n%(orfu-link-hooks)\n%?")
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
           (orfu-handle-link-youtube link title))
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
(defun orfu-handle-link-youtube (link title)
  (let* ((file-name (concat title ".mp4"))
         (dir "~/Downloads/Videos")
         (full-name
          (expand-file-name file-name dir)))
    (add-hook 'orfu-link-hook
              `(lambda ()
                 ,(concat
                   (org-make-link-string dir dir)
                   "\n"
                   (org-make-link-string full-name file-name))))
    (let* ((max-id 0)
           id
           (max-id
            (progn
              (dolist (b (buffer-list))
                (when (string-match "\\`\\*youtube-dl \\([0-9]+\\)\\*\\'" (buffer-name b))
                  (setq id (string-to-number (match-string 1 (buffer-name b))))
                  (setq max-id (max id max-id))))
              max-id))
           (output-buffer (get-buffer-create
                           (format "*youtube-dl %d*" (1+ max-id)))))
      (async-shell-command
       (format "youtube-dl -f mp4 \"%s\" -o %s" link (shell-quote-argument full-name))
       output-buffer))
    (find-file (orfu-expand "ent.org"))
    (goto-char (point-min))
    (re-search-forward "^\\*+ +Videos" nil t)))

;;** agenda
(defun orfu-tags-projects ()
  nil)

(setq org-agenda-custom-commands
      `(("n" "Agenda and all TODO's" ((agenda "")
                                      (alltodo "")))
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
                        '((agenda time-up priority-down tag-up)))
                       (org-deadline-warning-days 0)))))
        ("P" "Project List"
             ((tags "PROJECT")))
        ("w" "Weekly"
             ((agenda ""
                      ((org-agenda-span 'week)
                       (org-agenda-skip-function 'orfu-skip-daily-tasks)
                       (org-deadline-warning-days 0)))))))

(defun orfu-skip-daily-tasks ()
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
        (headline (or (and (org-at-heading-p) (point))
                      (save-excursion (org-back-to-heading))))
        (rep-str (org-get-repeat)))
    (if (and rep-str (string-match "\\+[0-9]+d" rep-str))
        next-headline
      nil)))

;;;###autoload
(defun orfu-agenda-quick ()
  (interactive)
  (org-agenda nil "h"))

;;;###autoload
(defun orfu-agenda-office ()
  (interactive)
  (org-agenda nil "o"))

(defvar orfu-agenda-files-home nil)

(defun orfu-vacation-p ()
  "Requires a file \"wiki/vacation.org\".
With contents, for example:
    * 2017
    ** day off <2017-03-17 Fri>"
  (let ((vacation-file (orfu-expand "wiki/vacation.org")))
    (when (file-exists-p vacation-file)
      (let ((today (format-time-string "%Y-%m-%d")))
            (with-current-buffer (find-file-noselect
                                  vacation-file)
              (save-excursion
                (goto-char (point-min))
                (re-search-forward today nil t)))))))

;;;###autoload
(defun orfu-agenda-day ()
  (interactive)
  (let* ((ct (decode-time (current-time)))
         (hour (nth 2 ct))
         (dow (nth 6 ct)))
    (if (and
         (member dow '(1 2 3 4 5))
         (>= hour 9)
         (< hour 17)
         (not (orfu-vacation-p)))
        (setq org-agenda-files
              (cl-set-difference
               org-agenda-files orfu-agenda-files-home
               :test 'equal))
      (setq org-agenda-files
            (cl-union
             org-agenda-files orfu-agenda-files-home
             :test 'equal))))
  (org-agenda nil "d"))

;;;###autoload
(defun orfu-agenda-articles ()
  (interactive)
  (org-agenda nil "r"))

(provide 'org-fu)

;;; org-fu.el ends here
