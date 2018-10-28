(defvar org-fit-data-file)
(defvar org-fit-graph-file)
(defvar org-fit-cli-exec)
(defvar org-fit-csv-file)


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
(defvar org-fit-last-muscle "all")
(defvar org-fit-last-type :lines)


(defvar org-fit-cli-callback nil)

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
  (if (and (not (eq org-fit-cli-callback nil))
           (string-match-p (regexp-quote ">") output))
      (let ((cb org-fit-cli-callback))
        (setq org-fit-cli-callback nil)
        (funcall cb)
    )))


(defvar org-fit-cli-process nil)
(defun org-fit-start-cli ()
  (interactive)
  (setq org-fit-cli-process (start-process "org-fit-cli" "*org-fit-log*" org-fit-cli-exec))
  (switch-to-buffer "*org-fit-log*")
  (process-send-string org-fit-cli-process (format "load_org %s\n" org-fit-data-file))
  (set-process-filter org-fit-cli-process 'org-fit-update-graph)
  (org-fit-show-graph)
  (switch-to-buffer-other-window "*org-fit-graph*")
  (org-fit-mode)
  )


(defun org-fit-update-graph-callback ()
  (with-current-buffer (get-buffer-create "*org-fit-graph*")
    (erase-buffer)
    (clear-image-cache t)
    (insert-image (create-image org-fit-graph-file))
    ))

(defun org-fit-show-graph()
  (interactive)
  (setq org-fit-cli-callback 'org-fit-update-graph-callback)
  (if (eq :lines org-fit-last-type)
      (process-send-string org-fit-cli-process
                           (format "graph %s %s %s %s %s\n"
                                   org-fit-last-value org-fit-last-groupby
                                   org-fit-last-months org-fit-last-muscle
                                   org-fit-graph-file))
    (process-send-string org-fit-cli-process
                         (format "pie %s month %s\n"
                                 org-fit-last-value  org-fit-graph-file))
    ))

(defun org-fit-set-value (value)
  (setq org-fit-last-value value)
  (org-fit-show-graph))

(defun org-fit-set-groupby (groupby)
  (setq org-fit-last-groupby groupby)
  (org-fit-show-graph))

(defun org-fit-set-months (months)
  (setq org-fit-last-months months)
  (org-fit-show-graph))

(defun org-fit-set-category-callback ()
  (let* ((str_values 
         (with-current-buffer "*org-fit-log*"
           ;; This is slow
           (string-trim (car (last (split-string (buffer-string) ">") 2)))
           ))
         (lst_values (cons "all" (split-string str_values ","))))
    (helm :sources (helm-build-sync-source "Muscle"
                     :candidates lst_values
                     :action (lambda (candidate)
                               (setq org-fit-last-muscle candidate)
                               (org-fit-show-graph))
                     ))
    )
  )


(defun org-fit-set-category ()
  (interactive)
  (setq org-fit-cli-callback 'org-fit-set-category-callback)
  (process-send-string org-fit-cli-process "list_muscles\n"))

(defun org-fit-set-type (type)
  (setq org-fit-last-type type)
  (org-fit-show-graph))

(defun org-fit-import-csv ()
  (interactive)
  (setq org-fit-cli-callback nil)
  (process-send-string org-fit-cli-process
                       (format "import %s %s\n"
                               org-fit-csv-file
                               org-fit-data-file)))



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

 "p"  (lambda () (interactive) (org-fit-set-type :pie))
 "l"  (lambda () (interactive) (org-fit-set-type :lines))

 "q"  'org-fit-set-category
 )

(provide 'gym)




(defvar org-fit-data-file "/home/guancio/Sources/org-fit/data/res.org")
(defvar org-fit-export-path "/home/guancio/Sources/org-fit/data/")
(defvar org-fit-cli-exec "/home/guancio/Sources/org-fit/cli")

(defun org-fit-start ()
  (interactive)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (org-display-inline-images)
  (funcall 
   (intern (concat "org-babel-execute:" "python"))
   (format "fit_path = \"%s\"
org_file = \"%s\"
data_path = \"%s\"
import sys
import os
sys.path.append(fit_path)
import orgparser
import graph
import csvimport
from cmd import Cmd
trains = orgparser.parse_gym_file(org_file)
" org-fit-cli-exec org-fit-data-file org-fit-export-path)
   (org-combine-plists
    '((:colname-names) (:rowname-names) (:result-params "replace") (:result-type . value) (:results . "replace") (:exports . "none") (:session . "plots") (:tangle . "no") (:hlines . "no") (:noweb . "no") (:cache . "no"))
  )))

(defun org-fit-add-graph-history ()
  (interactive)
  (insert (format "
#+header: :var value=\"reps\" period=\"month\" months=\"all\" muscle=\"all\"
#+begin_src python :session plots :results file :exports results
graph.draw_line_graph(trains, value, period, months, muscle, data_path)
#+end_src
"))
  (org-babel-execute-src-block)
  )

(defun org-fit-add-graph-breakout ()
  (interactive)
  (insert (format "
#+header: :var value=\"reps\" period=\"month\" 
#+begin_src python :session plots :results file :exports results
graph.draw_pie_graph(trains, value, period, data_path)
#+end_src
"))
  (org-babel-execute-src-block)
  )


(defun org-fit-graph-change-attr (attr val)
  ;; (save-excursion
  (goto-char (org-babel-where-is-src-block-head))
  (if (looking-at-p "#\\+begin_src")
      (progn
        (re-search-backward "#\\+header: :var" nil t)
        (re-search-forward ":var +" nil t)
        (let* ((var-str (buffer-substring-no-properties (point) (line-end-position)))
               (var-ass (split-string var-str " "))
               (var-vals (mapcar (lambda (x) (split-string x "=")) var-ass))
               (new-var-vals (mapcar (lambda (x)
                                      (if (equal (car x) attr)
                                          (list attr val)
                                        x)) var-vals))
               (new-var-ass (mapcar (lambda (x) (string-join x "=")) new-var-vals))
               (new-var-str (string-join new-var-ass " "))
               )
          (delete-region (point) (line-end-position))
          (insert new-var-str)
          (org-babel-execute-src-block)
      ))
  ))

(general-define-key
 :keymaps 'org-mode-map
 "C-q" '(nil :which-key "fit-value")
 "C-q 1"  '((lambda () (interactive) (org-fit-graph-change-attr "value" "\"vol\"")) :which-key "Volume")
 "C-q 2"  '((lambda () (interactive) (org-fit-graph-change-attr "value" "\"reps\"")) :which-key "Reps")
 "C-q 3"  '((lambda () (interactive) (org-fit-graph-change-attr "value" "\"count\"")) :which-key "Count")
 "C-q 4"  '((lambda () (interactive) (org-fit-graph-change-attr "value" "\"sets\"")) :which-key "Sets")
 "C-w" '(nil :which-key "fit-period")
 "C-w 1"  '((lambda () (interactive) (org-fit-graph-change-attr "period" "\"month\"")) :which-key "Month")
 "C-w 2"  '((lambda () (interactive) (org-fit-graph-change-attr "period" "\"week\"")) :which-key "Week")
 "C-w 3"  '((lambda () (interactive) (org-fit-graph-change-attr "period" "\"day\"")) :which-key "Day")
 "C-e" '(nil :which-key "fit-months")
 "C-e 1"  '((lambda () (interactive) (org-fit-graph-change-attr "months" "1")) :which-key "1")
 "C-e 2"  '((lambda () (interactive) (org-fit-graph-change-attr "months" "3")) :which-key "3")
 "C-e 3"  '((lambda () (interactive) (org-fit-graph-change-attr "months" "6")) :which-key "6")
 "C-e 0"  '((lambda () (interactive) (org-fit-graph-change-attr "months" "\"all\"")) :which-key "all")
 )
 





(defun org-fit-test ()
  (interactive)
  ;; (save-excursion
  (goto-char (org-babel-where-is-src-block-head))
  (if (looking-at-p "#\\+begin_src")
      (progn
        (re-search-backward "#\\+header: :var" nil t)
        (re-search-forward ":var +" nil t)
        (let* ((var-str (buffer-substring-no-properties (point) (line-end-position)))
               (var-ass (split-string var-str " "))
               (var-vals (mapcar (lambda (x) (split-string x "=")) var-ass))
               (new-var-vals (mapcar (lambda (x)
                                      (if (equal (car x) "value")
                                          '("value" "\"vol\"")
                                        x)) var-vals))
               (new-var-ass (mapcar (lambda (x) (string-join x "=")) new-var-vals))
               (new-var-str (string-join new-var-ass " "))
               )
          (delete-region (point) (line-end-position))
          (insert new-var-str)
          (org-babel-execute-src-block)
      ))
  ))

(defun org-fit-test1 ()
  (interactive)
  (message (org-element-put-property (:header (org-element-at-point) "100"))
           ))

