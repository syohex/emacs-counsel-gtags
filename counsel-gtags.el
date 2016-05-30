;;; counsel-gtags.el --- counsel for GNU global -*- lexical-binding: t; -*-

;; Copyright (C) 2016 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-counsel-gtags
;; Version: 0.01
;; Package-Requires: ((emacs "24") (counsel "0.8.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Counsel for GNU global

;;; Code:

(require 'counsel)

(declare-function cygwin-convert-file-name-from-windows "cygw32.c")
(declare-function cygwin-convert-file-name-to-windows "cygw32.c")

(defgroup counsel-gtags nil
  "`counsel' for GNU Global"
  :group 'counsel)

(defcustom counsel-gtags-ignore-case nil
  "Ignore case in search command."
  :type 'boolean)

(defcustom counsel-gtags-path-style 'root
  "Candidates path style.
  - `root' shows path from root where tag files are
  - `relative' shows path from current directory
  - `absolute' shows absolute path."
  :type '(choice (const :tag "Root of the current project" root)
                 (const :tag "Relative from the current directory" relative)
                 (const :tag "Absolute Path" absolute)))

(defconst counsel-gtags--prompts
  '((definition . "Find Definition: ")
    (reference  . "Find Reference: ")
    (pattern    . "Find Pattern: ")
    (symbol     . "Find Symbol: ")))

(defconst counsel-gtags--complete-options
  '((reference . "-r")
    (symbol    . "-s")
    (pattern   . "-g")))

(defvar counsel-gtags--context nil)

(defun counsel-gtags--select-gtags-label ()
  (let ((labels '("default" "native" "ctags" "pygments")))
    (ivy-read "GTAGSLABEL(Default: default): " labels)))

(defun counsel-gtags--generate-tags ()
  (if (not (yes-or-no-p "File GTAGS not found. Run 'gtags'? "))
      (error "Abort generating tag files.")
    (let* ((root (read-directory-name "Root Directory: "))
           (label (counsel-gtags--select-gtags-label))
           (default-directory root))
      (message "gtags is generating tags....")
      (unless (zerop (process-file "gtags" nil nil nil "-q"
                                   (concat "--gtagslabel=" label)))
        (error "Faild: 'gtags -q'"))
      root)))

(defun counsel-gtags--root ()
  (or (getenv "GTAGSROOT")
      (locate-dominating-file default-directory "GTAGS")
      (counsel-gtags--generate-tags)))

(defsubst counsel-gtags--windows-p ()
  (memq system-type '(windows-nt ms-dos)))

(defun counsel-gtags--set-absolute-option-p ()
  (or (eq counsel-gtags-path-style 'absolute)
      (and (counsel-gtags--windows-p)
           (getenv "GTAGSLIBPATH"))))

(defun counsel-gtags--command-options (type)
  (let ((options '("--result=grep")))
    (let ((opt (assoc-default type counsel-gtags--complete-options)))
      (when opt
        (push opt options)))
    (when (counsel-gtags--set-absolute-option-p)
      (push "-a" options))
    (when counsel-gtags-ignore-case
      (push "-i" options))
    (when current-prefix-arg ;; XXX
      (push "-l" options))
    (when (getenv "GTAGSLIBPATH")
      (push "-T" options))
    options))

(defun counsel-gtags--complete-candidates (string type)
  (let ((cmd-options (counsel-gtags--command-options type)))
    (push "-c" cmd-options)
    (push string cmd-options)
    (counsel--async-command
     (mapconcat #'shell-quote-argument (cons "global" (reverse cmd-options)) " "))
    nil))

(defun counsel-gtags--file-and-line (candidate)
  (if (and (counsel-gtags--windows-p)
           (string-match-p "\\`[a-zA-Z]:" candidate)) ;; Windows Driver letter
      (when (string-match "\\`\\([^:]+:[^:]+:\\):\\([^:]+\\)" candidate)
        (list (match-string-no-properties 1)
              (string-to-number (match-string-no-properties 2))))
   (let ((fields (split-string candidate ":")))
     (list (cl-first fields) (string-to-number (cl-second fields))))))

(defun counsel-gtags--find-file (candidate)
  (with-ivy-window
   (swiper--cleanup)
   (cl-destructuring-bind (file line) (counsel-gtags--file-and-line candidate)
     (push (list :file file :line line) counsel-gtags--context)
     (find-file file)
     (goto-char (point-min))
     (forward-line (1- line))
     (back-to-indentation))))

(defun counsel-gtags--read-tag (type)
  (let ((default-val (thing-at-point 'symbol))
        (prompt (assoc-default type counsel-gtags--prompts))
        (comp-fn (lambda (string)
                   (counsel-gtags--complete-candidates string type))))
    (ivy-read prompt comp-fn
              :initial-input default-val
              :dynamic-collection t
              :unwind (lambda ()
                        (counsel-delete-process)
                        (swiper--cleanup))
              :caller 'counsel-gtags--read-tag)))

(defun counsel-gtags--tag-directory ()
  (with-temp-buffer
    (or (getenv "GTAGSROOT")
        (progn
          (unless (zerop (process-file "global" nil t nil "-p"))
            (error "GTAGS not found"))
          (goto-char (point-min))
          (let ((dir (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
            (file-name-as-directory (if (eq system-type 'cygwin)
                                        (cygwin-convert-file-name-from-windows dir)
                                      dir)))))))

(defsubst counsel-gtags--construct-command (options &optional input)
  (mapconcat #'shell-quote-argument (append '("global") options (list input)) " "))

(defun counsel-gtags--execute (type tagname encoding)
  (let* ((options (counsel-gtags--command-options type))
         (cmd (counsel-gtags--construct-command (reverse options) tagname))
         (default-directory default-directory)
         (coding-system-for-read encoding)
         (coding-system-for-write encoding))
    (counsel--async-command cmd nil)
    nil))

(defun counsel-gtags--select-file (type tagname)
  (let* ((root (counsel-gtags--default-directory))
         (encoding buffer-file-coding-system)
         (comp-fn (lambda (_string)
                    (let ((default-directory root))
                      (counsel-gtags--execute type tagname encoding)))))
    (ivy-read "Pattern: " comp-fn
              :dynamic-collection t
              :unwind (lambda ()
                        (counsel-delete-process)
                        (swiper--cleanup))
              :action #'counsel-gtags--find-file
              :caller 'counsel-gtags--read-tag)))

;;;###autoload
(defun counsel-gtags-find-definition (tagname)
  (interactive
   (list (counsel-gtags--read-tag 'definition)))
  (counsel-gtags--select-file 'definition tagname))

;;;###autoload
(defun counsel-gtags-find-reference (tagname)
  (interactive
   (list (counsel-gtags--read-tag 'reference)))
  (counsel-gtags--select-file 'reference tagname))

;;;###autoload
(defun counsel-gtags-find-symbol (tagname)
  (interactive
   (list (counsel-gtags--read-tag 'symbol)))
  (counsel-gtags--select-file 'symbol tagname))

(defconst counsel-gtags--include-regexp
  "\\`\\s-*#\\(?:include\\|import\\)\\s-*[\"<]\\(?:[./]*\\)?\\(.*?\\)[\">]")

(defun counsel-gtags--default-file-name ()
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (when (string-match counsel-gtags--include-regexp line)
      (match-string-no-properties 1 line))))

(defun counsel-gtags--read-file-name ()
  (let ((default-file (counsel-gtags--default-file-name))
        (candidates
         (with-temp-buffer
           (let* ((options (cl-case counsel-gtags-path-style
                             (absolute "-Poa")
                             (root "-Poc")
                             (relative ""))))
             (unless (zerop (process-file "global" nil t nil options))
               (error "Failed: collect file names."))
             (goto-char (point-min))
             (let (files)
               (while (not (eobp))
                 (push (buffer-substring-no-properties (point) (line-end-position)) files)
                 (forward-line 1))
               (reverse files))))))
    (ivy-read "Find File: " candidates
              :initial-input default-file)))

(defun counsel-gtags--default-directory ()
  (cl-case counsel-gtags-path-style
    ((relative absolute) default-directory)
    (root (counsel-gtags--root))))

;;;###autoload
(defun counsel-gtags-find-file (filename)
  (interactive
   (list (counsel-gtags--read-file-name)))
  (let ((default-directory (counsel-gtags--default-directory)))
    (find-file filename)))

;;;###autoload
(defun counsel-gtags-pop ()
  (interactive)
  (let ((context (pop counsel-gtags--context)))
    (find-file (plist-get context :file))
    (goto-char (point-min))
    (forward-line (1- (plist-get context :line)))))

;;;###autoload
(defun counsel-gtags-create-tags (rootdir label)
  "Create tag files tags in `rootdir'. This command is asynchronous."
  (interactive
   (list (read-directory-name "Directory: " nil nil t)
         (counsel-gtags--select-gtags-label)))
  (let* ((default-directory rootdir)
         (proc-buf (get-buffer-create " *counsel-gtags-tag-create*"))
         (proc (start-file-process
                "counsel-gtags-tag-create" proc-buf
                "gtags" "-q" (concat "--gtagslabel=" label))))
    (set-process-sentinel
     proc
     (lambda (p _event)
       (when (eq (process-status p) 'exit)
         (kill-buffer proc-buf)
         (message "%s: creating tag files(label: %s) in %s"
                  (if (zerop (process-exit-status p)) "Success" "Failed")
                  label rootdir))))))

(provide 'counsel-gtags)

;;; counsel-gtags.el ends here
