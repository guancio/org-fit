(defvar org-fit-data-file)
(defvar org-fit-export-path)
(defvar org-fit-cli-exec)


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
 "C-e 2"  '((lambda () (interactive) (org-fit-graph-change-attr "months" "2")) :which-key "2")
 "C-e 3"  '((lambda () (interactive) (org-fit-graph-change-attr "months" "3")) :which-key "3")
 "C-e 6"  '((lambda () (interactive) (org-fit-graph-change-attr "months" "6")) :which-key "6")
 "C-e 0"  '((lambda () (interactive) (org-fit-graph-change-attr "months" "\"all\"")) :which-key "all")
 )
 

(provide 'gym)
