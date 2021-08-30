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

(defgroup ghb-bar nil
  "Header bar in a emacs frame."
  :group 'environment
  )

(defface ghb-background
  '((t :background "#1E90FF"))
  "Font"
  :group 'ghb-bar
  )

(defcustom ghb-time-format "- %A %d %B %Y - %X -"
  "Define the display format of time in the header bar."
  :group 'ghb-bar
  :type 'string
  )

(defcustom ghb-battery-display-function 'ghb-battery-string-wo-icons
  "Define how to display the battery status.

Valid Values: icons, text, both."
  :group 'ghb-bar
  :type '(choice (function-item :tag "Only icons"
                                :doc "Use only icons"
                                ghb-battery-string-w-icons)
                 (function-item :tag "Only text"
                                :doc "Use only text"
                                ghb-battery-string-wo-icons)
                 (function-item :tag "Text & icons"
                                :doc "Use text and icons"
                                ghb-battery-string-text-icons)
                 )
  )

(defcustom ghb-battery-format "%b%p%% (%L)"
  "Define the display battery format in the header bar."
  :group 'ghb-bar
  :type 'string
  )

(defconst ghb-buffer-name "Ghb")

(defvar ghb-window-origin nil)

(defvar ghb-parameters
  '(window-parameters . ((no-other-window . t)
                         (no-delete-other-windows . t)
                         (mode-line-format . none))))

(defvar ghb-timer nil)


(defun ghb-const-buffer-name ()
  "Construct header buffer name."
  (concat "  *" ghb-buffer-name "*  "))

(defun ghb-get-buffer ()
  "Return header buffer object."
  (get-buffer-create (ghb-const-buffer-name)))

(defun ghb-get-window ()
  "Return header window object."
  (let ((ghb-window (get-buffer-window (ghb-const-buffer-name))))
    (unless ghb-window
      (progn
        (setq ghb-window (display-buffer-in-side-window (ghb-get-buffer)
                                                           `((side . top)
                                                             (slot . 0)
                                                             (window-height . 1)
                                                             (preserve-size . (nil . t))
                                                             ,ghb-parameters)))
        (set-window-parameter ghb-window 'mode-line-format 'none)
        )
      )
    ghb-window))

(defun ghb-call-func-in-ghb (funcname &rest args)
  "Call FUNCNAME in header window with ARGS and come back to previous window."
  (let ((ghb-window (ghb-get-window))
        (ghb-window-origin (get-buffer-window)))
    (select-window ghb-window)
    (let ((result (if args
                      (apply funcname args)
                    (funcall funcname))))
      (select-window ghb-window-origin)
      (. result))))

(defun ghb-write-text (project)
  "Errase buffer and write header text."
  (erase-buffer)
  (ghb-print project)
  )

(defun ghb-text-width ()
  "Return header window text width."
  (ghb-call-func-in-ghb 'window-body-width))

(defun ghb-exists-p ()
  "Return header buffer if exists."
  (get-buffer (ghb-const-buffer-name)))

(defun ghb-update ()
  "Update header buffer text."
  (let ((project (projectile-project-name)))
    (ghb-call-func-in-ghb 'ghb-write-text project))
  )

(defun ghb-battery-string-w-icons ()
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

(defun ghb-battery-string-wo-icons ()
  "Return the battery string with only text."
  (battery-format ghb-battery-format (funcall battery-status-function))
  )

(defun ghb-battery-string-text-icons ()
  "Return the battery string with text and icons."
  (let ((text (ghb-battery-string-wo-icons))
        (icons (ghb-battery-string-w-icons)))
    (concat icons " - " text)
    )
  )

(defun ghb-time-string ()
  "Return the time string to display in header."
  (format-time-string ghb-time-format)
  )

(defun ghb-print (project)
  (let* ((battery (if (not battery-status-function)
                      ""
                    (funcall ghb-battery-display-function)
                    )
                  )
         (time (ghb-time-string))
         (project-width (string-width project))
         (line-width (ghb-text-width))
         (battery-width (string-width battery))
         (time-width (string-width time))
         (adjust (if (or (eq ghb-battery-display-function 'ghb-battery-string-w-icons)
                         (eq ghb-battery-display-function 'ghb-battery-string-text-icons)
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

(defun ghb-open (&rest _x)
  (interactive)
  (message "Open")
  (setq ghb-window-origin (get-buffer-window))
  (let ((ghb-exists (ghb-exists-p))
        (ghb-buffer (ghb-get-buffer))
        (ghb-window (ghb-get-window)))
    (unless ghb-exists
      (set-window-buffer ghb-window ghb-buffer)
      (set-buffer ghb-buffer)
      (select-window ghb-window)
      (ghb-bar-mode)
      (select-window ghb-window-origin)
      )
    )
  )

(defun ghb-close (&rest _x)
  (interactive)
  (message "Close")
  (let ((ghb-exists (ghb-exists-p)))
    (when ghb-exists
      (let ((ghb-buffer (ghb-get-buffer)))
        (kill-buffer ghb-buffer)
        (cancel-timer ghb-timer)
        )
      )
    )
  )

(define-derived-mode ghb-bar-mode nil "Ghb-Bar"
  "Major mode for Headerbar."
  (buffer-face-set 'ghb-background)
  (setq cursor-type nil)
  (setq ghb-timer (run-at-time "0 sec" .5 'ghb-update))
  :group 'ghb
  )

(add-to-list 'desktop-modes-not-to-save 'ghb-bar-mode)
(advice-add 'desktop-save :before 'ghb-close)
(advice-add 'desktop-save :after 'ghb-open)

(provide 'ghb)
;;; ghb.el ends here
