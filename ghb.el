;;; ghb.el --- Implement a Emacs frame header bar

;; Copyright (C) 2021 Léandre GIRET

;; Author:  Léandre GIRET <lgiret@icloud.com>
;; Created: 28 Aug 2021
;; URL: https://github.com/Uswald/ghb
;; Version: 1.1.1
;; Package-Requires: ((emacs "27.0")
;;                    (all-the-icons "5.0.0")
;;                    (projectile "2.6.0")
;;                    (frame-local "0.0.1"))

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

;; This package propose to insert a frame header bar with :
;;  - project name;
;;  - date and time;
;;  - battery information.

;;; Code:

(require 'all-the-icons)
(require 'battery)
(require 'projectile)
(require 'desktop)
(require 'vc)
(require 'frame-local)

;;----------------------------------------------------------------------------;;
;; Functionalities                                                            ;;
;;----------------------------------------------------------------------------;;

(defmacro ghb-get (var &optional frame)
  "Get VAR in FRAME if non-nil or in the current frame."
  (let ((var-name (intern (format "%s" var))))
    `(frame-local-get ',var-name (or ,frame (selected-frame)))))

(defmacro ghb-set (var val &optional frame)
  "Set VAR to VAL in FRAME if non-nil or in the current frame.
Return VAL."
  (let ((var-name (intern (format "%s" var))))
    `(frame-local-set ',var-name ,val (or ,frame (selected-frame)))))

;;----------------------------------------------------------------------------;;
;; Customization                                                              ;;
;;----------------------------------------------------------------------------;;

(defgroup ghb nil
  "Header bar in a emacs frame."
  :group 'environment
  )

(defface ghb
  '((t :background "#1E90FF" :bold t))
  "Font"
  :group 'ghb
  )

(defcustom ghb-time-format "- %A %d %B %Y - %X -"
  "Define the display format of time in the header bar."
  :group 'ghb
  :type 'string
  )

(defcustom ghb-battery-display-function 'ghb-battery-string-wo-icons
  "Define how to display the battery status.

Valid Values: icons, text, both."
  :group 'ghb
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
  :group 'ghb
  :type 'string
  )

(defcustom ghb-time-idle-delay 0.5
  "Seconds of idle time before the first update of the header bar.
Values smaller than 0.1 sec are treated as 0.1 sec."
  :type 'number
  :group 'ghb
  :set (lambda (symbol value)
         (let ((used (max value 0.1)))
           (set-default symbol used)
           (--each (frame-list)
             (when (ghb-get ghb-idle-timer it)
               (ghb--start-idle-timer it))))))

(defcustom ghb-time-update-interval 0.5
  "Time between two update of the header bar in seconds.
Values smaller than 0.1 sec are treated as 0.1 sec."
  :type 'number
  :group 'ghb
  :set (lambda (symbol value)
         (let ((used (max value 0.1)))
           (set-default symbol value)
           (--each (frame-list)
             (when (ghb-get ghb-timer it)
               (ghb--start-timer it))))))

(defvar ghb-parameters
  '(window-parameters . ((no-other-window . t)
                         (no-delete-other-windows . t)
                         (mode-line-format . none))))

(defun ghb-const-buffer-name (&optional frame)
  "Construct header buffer name from `ghb-buffer-name' and the FRAME name.
If FRAME is nil, we use the current frame.
The return value should be unique for each frame.
On terminals instance, we use the frame parameter `name'
On Graphics ones, the name isn't unique for each frame, so we use
`window-id' that isn't available on terminals instance."
  (let ((previous (ghb-get ghb-buffer-name frame)))
    (if previous
        previous
      (let ((name (concat "  *GHB-"
                          (or (frame-parameter nil 'window-id)
                              (frame-parameter nil 'name))
                          "*  ")))
        (ghb-set ghb-buffer-name name frame)))))

(defun ghb-get-buffer (&optional frame)
  "Return header buffer object for FRAME."
  (get-buffer-create (ghb-const-buffer-name frame)))

(defun ghb-set-window (&optional frame)
  "Display ghb  buffer in a side window in FRAME."
  (display-buffer (ghb-get-buffer frame)
                  `(display-buffer-in-side-window . ((side . top)
                                                     (slot . 0)
                                                     (window-height . 1)
                                                     (preserve-size . (nil . t))
                                                     ,ghb-parameters))
                  frame))

(defun ghb-get-window (&optional frame)
  "Return the created/existing window displaying the ghb.
If FRAME is non-nil, the window is created in frame."
  (let ((ghb-window (get-buffer-window (ghb-const-buffer-name frame) frame)))
    (unless ghb-window
      (setq ghb-window (ghb-set-window frame))
      (set-window-dedicated-p ghb-window t)
      (set-window-parameter ghb-window 'no-delete-other-windows t)
      (set-window-parameter ghb-window 'mode-line-format 'none))
    ghb-window))

(defun ghb--start-idle-timer (&optional frame)
  "Start the `ghb-idle-timer'."
  (let ((idle-timer (ghb-get ghb-idle-timer frame)))
    (when idle-timer (cancel-timer idle-timer))
    (ghb-set ghb-idle-timer
             (run-with-idle-timer ghb-time-idle-delay
                                  :repeat #'ghb-start frame)
             frame)))

(defun ghb--start-timer (&optional frame)
  "Start the `ghb-timer'."
  (let ((timer (ghb-get ghb-timer frame)))
    (when timer (cancel-timer timer))
    (ghb-set ghb-timer
             (run-with-timer ghb-time-update-interval
                             ghb-time-update-interval
                             #'ghb-update frame)
             frame)))

(defun ghb-start (&optional frame)
  "Timer function called from the timer `ghb-idle-timer'.
This starts the timer `ghb-timer', which updates the header bar
if appropriate.  It also arranges to cancel that timer when the next
command starts, by installing a pre-command hook."
  (when (null (ghb-get ghb-timer frame))
    ;; Set up the timer first, so that if this signals an error,
    ;; ghb is not added to pre-command-hook.
    (ghb--start-timer frame)
    (add-hook 'pre-command-hook `(lambda () (ghb-end ,frame)))))

(defun ghb-end (&optional frame)
  "Stop header bar updating.
This is installed as a pre-command hook by `ghb-start'.
When run, it cancels the timer `ghb-timer' and removes
itself as a pre-command hook."
  (remove-hook 'pre-command-hook `(lambda () (ghb-end ,frame)))
  (let ((timer (ghb-get ghb-timer frame)))
    (when timer
      (cancel-timer timer)
      (ghb-set ghb-timer nil frame))))


(defun ghb-call-func-in-ghb (funcname &optional frame &rest args)
  "Call FUNCNAME in header window with ARGS and come back to previous window."
  (with-current-buffer (ghb-get-buffer frame)
    (let ((result (if args
                      (apply funcname args)
                    (funcall funcname))))
      (. result))))

(defun ghb-write-text (project &optional frame)
  "Errase buffer and write header text with PROJECT as project name."
  (erase-buffer)
  (ghb-print project frame)
  )

(defun ghb-text-width (&optional frame)
  "Return header window text width."
  (window-width (ghb-get-window frame)))

(defun ghb-exists-p ()
  "Return header buffer if exists."
  (get-buffer (ghb-const-buffer-name)))

(defun ghb-update (&optional frame)
  "Update header buffer text."
  (let ((project (ghb-project-string)))
    (ghb-call-func-in-ghb 'ghb-write-text frame  project frame)))

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

(defun ghb-project-string ()
  "Format the project name in the header bar."
  (let* ((name (projectile-project-name))
         (file (buffer-file-name))
         (backend (vc-backend file))
         (rev (if backend (vc-call-backend backend 'mode-line-string file) ""))
        )
    (concat name (if (not (string= "" rev))
                     (concat " > " rev)
                   ""
                   )
            )
    )
  )

(defun ghb-print (project &optional frame)
  "Write the header in the header buffer using PROJECT as project name."
  (let* ((battery (if (not battery-status-function)
                      ""
                    (funcall ghb-battery-display-function)
                    )
                  )
         (time (ghb-time-string))
         (project-width (string-width project))
         (line-width (ghb-text-width frame))
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
  "Open the header buffer."
  (interactive)
  (let ((origin (get-buffer-window))
        (ghb-exists (ghb-exists-p))
        (ghb-buffer (ghb-get-buffer))
        (ghb-window (ghb-get-window)))
    (unless ghb-exists
      (set-window-buffer ghb-window ghb-buffer)
      (set-buffer ghb-buffer)
      (select-window ghb-window)
      (ghb-bar-mode)
      (select-window origin)
      )
    )
  )

(defun ghb-close (&rest _x)
  "Close the header buffer."
  (interactive)
  (let ((ghb-exists (ghb-exists-p)))
    (when ghb-exists
      (let ((ghb-buffer (ghb-get-buffer)))
        (kill-buffer ghb-buffer)
        )
      )
    )
  (ghb-end)
  (let ((idle-timer (ghb-get ghb-idle-timer)))
    (when idle-timer
      (cancel-timer idle-timer)
      (ghb-set ghb-idle-timer nil))))

(define-derived-mode ghb-bar-mode nil "Ghb-Bar"
  "Major mode for Headerbar."
  (buffer-face-set 'ghb)
  (setq cursor-type nil)
  (ghb--start-idle-timer (selected-frame))
  :group 'ghb
  )

(add-to-list 'desktop-modes-not-to-save 'ghb-bar-mode)
(advice-add 'desktop-save :before 'ghb-close)
(advice-add 'desktop-save :after 'ghb-open)

(setq ghb-time-idle-delay 5.0)

(provide 'ghb)
;;; ghb.el ends here
