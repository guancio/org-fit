(defvar org-fit-data-file "/home/guancio/Sources/org-fit/data/res.org")
(defvar org-fit-graph-file "/home/guancio/Sources/org-fit/data/graph.png")
(defvar org-fit-cli-exec "/home/guancio/Sources/org-fit/cli/graph.py")


;; Create the keymap for this mode.
(defvar org-fit-mode-map (make-sparse-keymap)
  "Keymap for `org-fit-mode'.")

(define-derived-mode org-fit-mode nil "Org-fit"
  "Major mode for Org-fit).
     \\{org-fit-mode-map}
     No hook."
  )

(defvar org-fit-last-value "vol")
(defvar org-fit-last-groupby "month")
(defvar org-fit-last-months "all")


(defun org-fit-update-graph (proc output)
  (when (buffer-live-p (process-buffer proc))
         (with-current-buffer (process-buffer proc)
           (let ((moving (= (point) (process-mark proc))))
             (save-excursion
               ;; Insert the text, advancing the process marker.
               (goto-char (process-mark proc))
               (insert output)
               (set-marker (process-mark proc) (point)))
             (if moving (goto-char (process-mark proc))))))
  (with-current-buffer (get-buffer-create "*org-fit-graph*")
    (erase-buffer)
    (clear-image-cache t)
    (insert-image (create-image org-fit-graph-file))
    ))

(defvar org-fit-cli-process nil)
(defun org-fit-start-cli ()
  (interactive)
  (setq org-fit-cli-process (start-process "org-fit-cli" "*org-fit-log*" org-fit-cli-exec))
  (switch-to-buffer "*org-fit-log*")
  (process-send-string org-fit-cli-process (format "load_org %s\n" org-fit-data-file))
  (set-process-filter org-fit-cli-process 'org-fit-update-graph)
  (org-fit-show-graph)
  (switch-to-buffer-other-window "*org-fit-graph*")
  )


(defun org-fit-show-graph()
  (interactive)
  (process-send-string org-fit-cli-process
                       (format "graph %s %s %s %s\n"
                               org-fit-last-value org-fit-last-groupby org-fit-last-months
                               org-fit-graph-file))
  )

(defun org-fit-set-value (value)
  (setq org-fit-last-value value)
  (org-fit-show-graph))

(defun org-fit-set-groupby (groupby)
  (setq org-fit-last-groupby groupby)
  (org-fit-show-graph))

(defun org-fit-set-months (months)
  (setq org-fit-last-months months)
  (org-fit-show-graph))

(defun org-fit-set-category ()
  (interactive)
  (helm :sources (helm-build-sync-source "Muscle"
                   :candidates '("All"  "Shoulders"  "Triceps"  "Biceps"  "Chest"  "Back"  "Legs"  "Abs"  "Cardio")
                   :action (lambda (candidate) (message candidate))
                   )))

(general-define-key
 :keymaps 'org-fit-mode-map
 "z"  (lambda () (interactive) (org-fit-set-value "vol"))
 "x"  (lambda () (interactive) (org-fit-set-value "count"))
 "c"  (lambda () (interactive) (org-fit-set-value "sets"))
 "v"  (lambda () (interactive) (org-fit-set-value "reps"))

 "a"  (lambda () (interactive) (org-fit-set-groupby "day"))
 "s"  (lambda () (interactive) (org-fit-set-groupby "week"))
 "d"  (lambda () (interactive) (org-fit-set-groupby "month"))

 "1"  (lambda () (interactive) (org-fit-set-months "1"))
 "2"  (lambda () (interactive) (org-fit-set-months "2"))
 "3"  (lambda () (interactive) (org-fit-set-months "3"))
 "6"  (lambda () (interactive) (org-fit-set-months "6"))
 "0"  (lambda () (interactive) (org-fit-set-months "all"))

 "q"  'org-fit-set-category
 )
