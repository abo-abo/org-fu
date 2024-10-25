;;; org-fu.el --- Org-mode tricks. -*- lexical-binding: t -*-
;; Expected files:
;; - gtd.org with level 1: Tasks

(require 'orca)
(require 'org-capture)

(defun orfu-expand (name)
  (expand-file-name name org-directory))

(defcustom orfu-github-project-name
  "https://github\\.com/abo-abo/\\([^/]+\\)"
  "Regex for Github repository projects."
  :type 'string)

;;* capture
;;** basic
;; http://orgmode.org/manual/Capture-templates.html#Capture-templates
(setq org-agenda-todo-ignore-with-date t)
(unless (assoc "t" org-capture-templates)
  (add-to-list 'org-capture-templates
               `("t" "TODO" entry (file+headline ,(orfu-expand "gtd.org") "Tasks")
                 "* TODO %?\nAdded: %U\n")))

(setq orca-handler-list
      (delete-dups
       (append
        '((orca-handler-project)
          (orfu-handle-link-youtube)
          (orca-handler-current-buffer "\\* Tasks")
          (orfu-handle-link-github))
        orca-handler-list)))

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

(defun orfu--youtube-link ()
  (let ((link (caar org-stored-links)))
    (when (and link (string-match-p "https://www.youtube.com/watch" link))
      (when (string-match "\\`\\(.*\\)&list=.*" link)
        (setq link (match-string 1 link)))
      (when (string-match "\\`\\(.*\\)&index=.*" link)
        (setq link (match-string 1 link)))
      link)))

(defun orfu-handle-link-youtube ()
  (let ((link (orfu--youtube-link)))
    (when link
      (setq orca-link-hook nil)
      ;; (orfu--handle-link-youtube-1 link)
      )))

;;** agenda
(setq org-agenda-custom-commands
      `(("n" "Agenda and all TODO's"
         ((agenda "")
          (todo "PROG")
          (todo "NEXT")
          (todo "TODO")
          ;; (todo "LIST")
          ))))

(defvar orfu-agenda-files-function nil
  "Function to produce a list of agenda files.")

;;;###autoload
(defun orfu-agenda-day ()
  (interactive)
  (when orfu-agenda-files-function
    (setq org-agenda-files (funcall orfu-agenda-files-function)))
  (org-agenda nil "n"))

(provide 'org-fu)

;;; org-fu.el ends here
