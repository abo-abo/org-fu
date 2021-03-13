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
(require 'json)
(require 'org-protocol)
(require 'org-capture)

(defvar orfu-org-basedir "~/Dropbox/org")

(defun orfu-expand (name)
  (expand-file-name name orfu-org-basedir))

(defcustom orfu-github-project-name
  "https://github\\.com/abo-abo/\\([^/]+\\)"
  "Regex for Github repository projects."
  :type 'string)

;;* capture
;;** basic
;; http://orgmode.org/manual/Capture-templates.html#Capture-templates
(setq org-agenda-todo-ignore-with-date t)
(setq org-capture-templates
      `(("t" "TODO" entry (file+headline ,(orfu-expand "gtd.org") "Tasks")
             "* TODO %?\nAdded: %U\n")
        ("L" "Link" entry #'orfu-handle-link
             "* TODO %(orfu-wash-link)\nAdded: %T\n%(orfu-link-hooks)\n")
        ("p" "Link" entry #'orfu-handle-link
             "* TODO %(orfu-wash-link)\nAdded: %T\n%i%(orfu-link-hooks)\n")))

(defun orfu-wash-link ()
  "Return a pretty-printed top of `org-stored-links'.
Try to remove superfluous information, like website title."
  (let ((link (caar org-stored-links))
        (title (cl-cadar org-stored-links)))
    (cond ((string-match-p "https://stackoverflow.com" link)
           (setq title (replace-regexp-in-string " - Stack Overflow" "" title)))
          ((string-match "https://twitter.com/\\([^/]+\\)/" link)
           (let ((user (concat "@" (match-string-no-properties 1 link))))
             (when (string-match ".* on Twitter: \"\\(.*\\)\" / Twitter" title)
               (setq title (concat user ": " (match-string 1 title))))
             (setq title (replace-regexp-in-string " *https://t.co/[^ ]+" "" title)))))
    (org-make-link-string link title)))

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
        '((orca-handler-project)
          (orca-handler-current-buffer "\\* Tasks")
          (orfu-handle-link-youtube)
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
  (setq orfu-link-hook nil)
  (let ((link (orfu--youtube-link)))
    (when link
      (add-hook 'orfu-link-hook (lambda () (orfu--handle-link-youtube-2 link))))
    (orca-handle-link)))

(defun orfu-shell (cmd output-buffer)
  "Run CMD in OUTPUT-BUFFER."
  (save-window-excursion
    (with-current-buffer (shell output-buffer)
      (comint-clear-buffer)
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
    (when (and link (string-match "https://www.youtube.com/" link))
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
    (ignore-errors
      (orfu-wait
       (setq description-json-file
             (car (directory-files default-directory t
                                   "\\.info\\.json\\'")))
       5))
    (when description-json-file
      (prog1 (json-read-file description-json-file)
        (delete-file description-json-file)))))

(defvar orfu-youtube-file-format "youtube-%(uploader)s-%(title)s.%(ext)s")

(defun orfu-difference (set1 set2)
  (cl-set-difference set1 set2 :test #'equal))

(defun orfu--try-start-vlc (fname cmd)
  (let ((fname-part (concat fname ".part"))
        fname-alt)
    (cond ((file-exists-p
            (setq fname-alt (replace-regexp-in-string "mp4$" "mkv" fname)))
           (orfu--start-vlc fname-alt fname-alt))
          ((file-exists-p
            (setq fname-alt (replace-regexp-in-string "mp4$" "webm" fname)))
           (orfu--start-vlc fname-alt fname-alt))
          (t
           (condition-case nil
               (orfu--start-vlc fname fname-part)
             (error
              (progn
                (orfu-shell
                 (replace-regexp-in-string "-f mp4 " "" cmd)
                 (orfu--youtube-output-buffer))
                (orfu--start-vlc fname fname))))))))

(defun orfu--handle-link-youtube-1 (link &optional no-org)
  (setq link (replace-regexp-in-string "time_continue=[0-9]+&" "" link))
  (let* ((default-directory "~/Downloads/Videos")
         (cmd (format
               "setsid -w youtube-dl -f mp4 --write-info-json -o %S %s"
               orfu-youtube-file-format link))
         (json (orfu--youtube-json cmd)))
    (if (not json)
        (progn
          (goto-char (point-min))
          (re-search-forward "^\\*+ +Videos$")
          (org-capture-put
           :immediate-finish t
           :jump-to-captured t)
          t)
      (let* ((fname (cdr (assoc '_filename json)))
             (title (cdr (assoc 'title json)))
             (channel (cdr (assoc 'uploader json))))
        (unless no-org
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
           :jump-to-captured t))
        (orfu--try-start-vlc fname cmd)
        t))))

(defun orfu--handle-link-youtube-2 (link)
  "Start youtube-dl on LINK, extracting the title from the process output."
  (setq link (replace-regexp-in-string "time_continue=[0-9]+&" "" link))
  (let ((dir "~/Downloads/Videos"))
    (make-directory dir t)
    (let* ((default-directory dir)
           (cmd (format
                 "setsid -w youtube-dl -f mp4 -o %S %s"
                 orfu-youtube-file-format link))
           (buf (orfu--youtube-output-buffer)))
      (orfu-shell cmd buf)
      (with-current-buffer buf
        (catch 'res
          (while t
            (cond ((string-match "\\[download\\] \\(?:Destination: \\)?\\(.+\\.mp4\\)\\(?: has already been downloaded\\)?" (buffer-string))
                   (throw 'res
                     (let* ((fname (match-string-no-properties 1 (buffer-string)))
                            (title (progn
                                     (string-match "youtube-\\(.*\\)-\\(.*\\)\\.mp4$" fname)
                                     (match-string 2 fname))))
                       (prog1 (org-make-link-string (expand-file-name fname dir) title)
                         (orfu--try-start-vlc fname cmd)))))
                  ((string-match "ERROR: .* File name too long" (buffer-string))
                   (throw 'res
                     (let ((orfu-youtube-file-format "youtube-%(uploader)s-%(id)s.%(ext)s"))
                       (orfu--handle-link-youtube-2 link))))
                  (t
                   (accept-process-output (get-buffer-process (current-buffer)) 0.5)))))))))

(defun orfu-play-link-at-point ()
  "Download Youtube video at point and play it with VLC."
  (interactive)
  (orfu--handle-link-youtube-2
   (org-element-property :raw-link (org-element-context))))

(defcustom orfu-start-vlc-if-already-running t
  "When non-nil, start a new VLC."
  :type 'boolean)

(defun orfu--start-vlc (fname fname-part)
  (orfu-wait
   (or (file-exists-p fname) (file-exists-p fname-part)))
  (orfu-wait
   (or (file-exists-p fname)
       (and (file-exists-p fname-part)
            (> (read (counsel--command
                      "du" "-schb"
                      fname-part))
               (* 3 1024 1024)))))
  (when (or orfu-start-vlc-if-already-running
            (string= "" (shell-command-to-string "pidof vlc")))
    (orly-start
     "vlc"
     (if (file-exists-p fname-part)
         fname-part
       fname))))

(defun orfu-handle-link-youtube ()
  (let ((link (orfu--youtube-link)))
    (when link
      (find-file (orfu-expand "wiki/youtube.org"))
      (goto-char (point-min))
      (when (search-forward link nil t)
        (message "Link already captured"))
      (setq orfu-link-hook nil)
      (orfu--handle-link-youtube-1 link))))

;;** agenda
(defun orfu-tags-projects ()
  nil)

(setq org-agenda-custom-commands
      `(("n" "Agenda and all TODO's"
             ((agenda "")
              (todo "PROG")
              (todo "TODO"
                    ((org-agenda-files '("~/Dropbox/org/projects/2016-06-habit/habit.org"))
                     (org-agenda-todo-ignore-with-date nil)))
              (todo "NEXT")
              (todo "TODO")
              (todo "LIST")))
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

(defvar orfu-agenda-files-function nil
  "Function to produce a list of agenda files.")

;;;###autoload
(defun orfu-agenda-day ()
  (interactive)
  (when orfu-agenda-files-function
    (setq org-agenda-files (funcall orfu-agenda-files-function)))
  (org-agenda nil "d")
  (setq default-directory orfu-org-basedir))

;;;###autoload
(defun orfu-agenda-articles ()
  (interactive)
  (org-agenda nil "r"))

(provide 'org-fu)

;;; org-fu.el ends here
