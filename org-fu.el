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
(require 'orca)
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
             "* TODO %?\nAdded: %T\n")))
;;** PDF
(push
 `("P" "Pdf article" entry (file+olp ,(orfu-expand "gtd.org") "Projects" "Articles")
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
        "* TODO %(orfu-wash-link)\nAdded: %T\n%(orfu-link-hooks)\n")
      org-capture-templates)

(defun orfu-wash-link ()
  "Return a pretty-printed top of `org-stored-links'.
Try to remove superfluous information, like website title."
  (let ((link (caar org-stored-links))
        (title (cl-cadar org-stored-links)))
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

(setq orca-handler-list
      (delete-dups
       (append
        '((orfu-handle-link-youtube)
          (orfu-handle-link-github))
        orca-handler-list)))

(defun orfu-handle-link-scholar ()
  (let ((link (caar org-stored-links)))
    (when (string-match "^https://scholar.google.com/scholar.bib" link)
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
       nil nil t))))

(defun orfu-handle-link-github ()
  (let ((link (caar org-stored-links))
        (title (cl-cadar org-stored-links)))
    (when (string-match orfu-github-project-name link)
      (let ((project-name (match-string 1 link))
            (parts (split-string title "·")))
        (setf (cl-cadar org-stored-links)
              (concat (car parts)
                      (substring (cadr parts) 7)))
        (find-file (orfu-expand "wiki/github.org"))
        (goto-char (point-min))
        (re-search-forward (concat "^\\*+ +" project-name) nil t)))))

(defun orfu-handle-link ()
  (orca-handle-link))

(defun orfu-shell (cmd output-buffer)
  "Run CMD in OUTPUT-BUFFER."
  (save-window-excursion
    (with-current-buffer (shell output-buffer)
      (insert cmd)
      (comint-send-input))))

(defun orfu--youtube-output-buffer ()
  (let ((max-id 0))
    (dolist (b (buffer-list))
      (when (string-match "\\`\\*youtube-dl \\([0-9]+\\)\\*\\'" (buffer-name b))
        (with-current-buffer b
          (if (looking-back comint-prompt-regexp)
              (kill-buffer)
            (setq max-id
                  (max (string-to-number (match-string 1 (buffer-name b)))
                       max-id))))))
    (format "*youtube-dl %d*" (1+ max-id))))

(defun orfu--youtube-link ()
  (let ((link (caar org-stored-links)))
    (when (string-match "https://www.youtube.com/" link)
      (when (string-match "\\`\\(.*\\)&list=.*" link)
        (setq link (match-string 1 link)))
      (when (string-match "\\`\\(.*\\)&index=.*" link)
        (setq link (match-string 1 link)))
      link)))

(defmacro orfu-wait (expr &optional n-seconds)
  (setq n-seconds (or n-seconds 10))
  `(let ((tic (current-time))
         timeout)
     (while (not (or ,expr timeout))
       (when (> (time-to-seconds (time-subtract (current-time) tic)) ,n-seconds)
         (setq timeout t))
       (sit-for 0.1))
     (when timeout
       (error "Waited %d seconds for %S" ,n-seconds ',expr))))

(defun orfu--youtube-json (cmd)
  (dolist (file (directory-files default-directory t
                                 "\\.info\\.json\\'"))
    (delete-file file))
  (orfu-shell cmd (orfu--youtube-output-buffer))
  (let (description-json-file)
    (orfu-wait
     (setq description-json-file
           (car (directory-files default-directory t
                                 "\\.info\\.json\\'"))))
    (prog1 (json-read-file description-json-file)
      (delete-file description-json-file))))

(defvar orfu-youtube-file-format "youtube-%(uploader)s-%(title)s.%(ext)s")

(defun orfu--handle-link-youtube-1 (link)
  (setq link (replace-regexp-in-string "time_continue=[0-9]+&" "" link))
  (let* ((default-directory "~/Downloads/Videos")
         (cmd (format
               "setsid -w youtube-dl --write-info-json -f mp4 -o %S %s"
               orfu-youtube-file-format link))
         (json (orfu--youtube-json cmd))
         (fname (cdr (assoc '_filename json)))
         (title (cdr (assoc 'title json)))
         (fname-part (concat fname ".part"))
         (channel (cdr (assoc 'uploader json))))
    (add-hook 'orfu-link-hook
              `(lambda ()
                 ,(concat (org-make-link-string (expand-file-name fname) title)
                          (format "\nDuration: %d." (/ (cdr (assoc 'duration json)) 60)))))
    (goto-char (point-min))
    (unless (re-search-forward (concat "^\\*+ +" channel) nil t)
      (re-search-forward "^\\*+ +Misc$")
      (insert "\n** " channel))
    (org-capture-put
     :immediate-finish t
     :jump-to-captured t)
    (condition-case nil
        (orfu--start-vlc fname fname-part)
      (error
       (progn
         (orfu-shell
          (replace-regexp-in-string "-f mp4 " "" cmd)
          (orfu--youtube-output-buffer))
         (orfu--start-vlc fname fname-part))))
    t))

(defun orfu--start-vlc (fname fname-part)
  (progn
    (orfu-wait
     (or (file-exists-p fname) (file-exists-p fname-part)))
    (when (file-exists-p fname-part)
      (setq fname fname-part))
    (orfu-wait
     (> (read (counsel--command "du" "-schb" fname)) (* 2 1024 1024)))
    (when (string= "" (shell-command-to-string "pidof vlc"))
      (orly-start "vlc" fname))))

(defun orfu-handle-link-youtube ()
  (let ((link (orfu--youtube-link)))
    (when link
      (find-file (orfu-expand "wiki/youtube.org"))
      (goto-char (point-min))
      (if (search-forward link nil t)
          (user-error "Link already captured")
        (orfu--handle-link-youtube-1 link)))))

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
            (re-search-forward (concat "<" today "[ a-zA-Z]*>") nil t)))))))

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
