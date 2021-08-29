;;; ghb --- Implement a Emacs frame header bar

;; Copyright (C) 2021 Léandre GIRET

;; Author:  Léandre GIRET <lgiret@icloud.com>
;; Created: 28 Aug 2021
;; URL: https://github.com/Uswald/ghb
;; Version: 0.1.0
;; Package-Version: $Id$
;; Package-Requires: ((emacs "24.4") (dash "2.11.0") (f "0.17.2"))

;; This file is NOT part of Emacs

;; This program is free software: you can redistribute it and/or modify
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
;;; Code:

(require 'all-the-icons)
(require 'battery)
(require 'projectile)
(require 'desktop)

(defgroup header-bar nil
  "Header bar in a emacs frame."
  :group 'environment
  )

(defface header-background
  '((t :background "#1E90FF"))
  "Font"
  :group 'header-bar
  )

(defcustom header-time-format "- %A %d %B %Y - %X -"
  "Define the display format of time in the header bar."
  :group 'header-bar
  :type 'string
  )

(defcustom header-battery-display-type 'header-battery-string-wo-icons
  "Define how to display the battery status.

Valid Values: icons, text, both."
  :group 'header-bar
  :type '(choice (function-item :tag "Only icons"
                                :doc "Use only icons"
                                header-battery-string-w-icons)
                 (function-item :tag "Only text"
                                :doc "Use only text"
                                header-battery-string-wo-icons)
                 (function-item :tag "Text & icons"
                                :doc "Use text and icons"
                                header-battery-string-text-icons)
                 )
  )

(defcustom header-battery-format "%b%p%% (%L)"
  "Define the display battery format in the header bar."
  :group 'header-bar
  :type 'string
  )

(defconst header-buffer-name "Header")

(defvar header-window-origin nil)

(defvar header-parameters
  '(window-parameters . ((no-other-window . t)
                         (no-delete-other-windows . t)
                         (mode-line-format . none))))

(defvar header-timer nil)


(defun header-const-buffer-name ()
  "Construct header buffer name."
  (concat "  *" header-buffer-name "*  "))

(defun header-get-buffer ()
  "Return header buffer object."
  (get-buffer-create (header-const-buffer-name)))

(defun header-get-window ()
  "Return header window object."
  (let ((header-window (get-buffer-window (header-const-buffer-name))))
    (unless header-window
      (progn
        (setq header-window (display-buffer-in-side-window (header-get-buffer)
                                                           `((side . top)
                                                             (slot . 0)
                                                             (window-height . 1)
                                                             (preserve-size . (nil . t))
                                                             ,header-parameters)))
        (set-window-parameter header-window 'mode-line-format 'none)
        )
      )
    header-window))

(defun header-call-func-in-header (funcname &rest args)
  "Call FUNCNAME in header window with ARGS and come back to previous window."
  (let ((header-window (header-get-window))
        (header-window-origin (get-buffer-window)))
    (select-window header-window)
    (let ((result (if args
                      (apply funcname args)
                    (funcall funcname))))
      (select-window header-window-origin)
      (. result))))

(defun header-write-text (project)
  "Errase buffer and write header text."
  (erase-buffer)
  (header-print project)
  )

(defun header-text-width ()
  "Return header window text width."
  (header-call-func-in-header 'window-body-width))

(defun header-exists-p ()
  "Return header buffer if exists."
  (get-buffer (header-const-buffer-name)))

(defun header-update ()
  "Update header buffer text."
  (let ((project (projectile-project-name)))
    (header-call-func-in-header 'header-write-text project))
  )

(defun header-battery-string-w-icons ()
  "Return the battery string with only icons."
  (let* ((battery-stat (funcall battery-status-function))
         (percent (string-to-number (cdr (assq ?p battery-stat))))
         (status (cdr (assq ?L battery-stat))))
    (concat (if (string= status "AC")
                (propertize (all-the-icons-material "power")
                            'face `(:family ,(all-the-icons-material-family)
                                            :height .8))
              "")
            (propertize (if (> percent 90)
                            (all-the-icons-faicon "battery-full")
                          (if (> percent 62)
                              (all-the-icons-faicon "battery-three-quarters")
                            (if (> percent 37)
                                (all-the-icons-faicon "battery-half")
                              (if (> percent 10)
                                  (all-the-icons-faicon "battery-quarter")
                                (all-the-icons-faicon "battery-empty")))))
                        'face `(:family ,(all-the-icons-faicon-family)
                                        :height 1)))))

(defun header-battery-string-wo-icons ()
  "Return the battery string with only text."
  (battery-format header-battery-format (funcall battery-status-function))
  )

(defun header-battery-string-text-icons ()
  "Return the battery string with text and icons."
  (let ((text (header-battery-string-wo-icons))
        (icons (header-battery-string-w-icons)))
    (concat icons " - " text)
    )
  )

(defun header-time-string ()
  "Return the time string to display in header."
  (format-time-string header-time-format)
  )

(defun header-print (project)
  (let* ((battery (funcall header-battery-display-type))
         (time (header-time-string))
         (project-width (string-width project))
         (line-width (header-text-width))
         (battery-width (string-width battery))
         (time-width (string-width time))
         (adjust (if (or (eq header-battery-display-type 'header-battery-string-w-icons)
                         (eq header-battery-display-type 'header-battery-string-text-icons)
                         )
                     2
                   0))
         (start-width (- (/ (- line-width time-width) 2) project-width))
         (end-width (- line-width (+ start-width battery-width time-width project-width adjust)))
         )
    (insert (concat
             project
             (make-string start-width ? )
             time
             (make-string end-width ? )
             battery))))

(defun header-open (&rest _x)
  (interactive)
  (message "Open")
  (setq header-window-origin (get-buffer-window))
  (let ((header-exists (header-exists-p))
        (header-buffer (header-get-buffer))
        (header-window (header-get-window)))
    (unless header-exists
      (set-window-buffer header-window header-buffer)
      (set-buffer header-buffer)
      (select-window header-window)
      (header-bar-mode)
      (select-window header-window-origin)
      )
    )
  )

(defun header-close (&rest _x)
  (interactive)
  (message "Close")
  (let ((header-exists (header-exists-p)))
    (when header-exists
      (let ((header-buffer (header-get-buffer)))
        (kill-buffer header-buffer)
        (cancel-timer header-timer)
        )
      )
    )
  )

(define-derived-mode header-bar-mode nil "Header-Bar"
  "Major mode for Headerbar."
  (buffer-face-set 'header-background)
  (setq cursor-type nil)
  (setq header-timer (run-at-time "0 sec" .5 'header-update))
  :group 'header
  )

(add-to-list 'desktop-modes-not-to-save 'header-bar-mode)
(advice-add 'desktop-save :before 'header-close)
(advice-add 'desktop-save :after 'header-open)

(provide 'header)
;;; header.el ends here
