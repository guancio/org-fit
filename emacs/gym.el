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
#+header: :var value=\"reps\" period=\"month\" months=\"all\" muscle=\"all\" exercise=\"all\"
#+begin_src python :session plots :results file :exports results
graph.draw_line_graph(trains, value, period, months, muscle, exercise, data_path)
#+end_src
"))
  (org-babel-execute-src-block)
  )

(defun org-fit-add-table-summary ()
  (interactive)
  (insert (format "
#+header: :var period=\"month\" months=\"all\" muscle=\"all\" exercise=\"all\"
#+begin_src python :session plots :results value :exports results
graph.get_summary(trains, period, months, muscle, exercise)
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

(defun org-fit-add-table-breakout ()
  (interactive)
  (insert (format "
#+header: :var period=\"month\" 
#+begin_src python :session plots :results value table :exports results
graph.get_breakout(trains, period)
#+end_src
"))
  (org-babel-execute-src-block)
  )

;; Problems with spaces
(defun org-fit-graph-change-attr-no-exec (attr val)
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
      ))
  ))



(defun org-fit-update-graph ()
  (goto-char (org-babel-where-is-src-block-head))
  (if (looking-at-p "#\\+begin_src")
      (org-babel-execute-src-block)
  ))

(defun org-fit-graph-change-attr (attr val)
  ;; (save-excursion
  (goto-char (org-babel-where-is-src-block-head))
  (if (looking-at-p "#\\+begin_src")
      (progn
        (org-fit-graph-change-attr-no-exec attr val)
        (org-babel-execute-src-block)
      ))
  )

(defun org-fit-get-all-muscles ()
  (cons "all"
        (split-string (funcall 
                       (intern (concat "org-babel-execute:" "python"))
                       "\",\".join(graph.get_all_muscles(trains).index.values)"
                       (org-combine-plists
                        '((:colname-names) (:rowname-names) (:result-params "replace") (:result-type . value) (:results . "replace") (:exports . "none") (:session . "plots") (:tangle . "no") (:hlines . "no") (:noweb . "no") (:cache . "no"))
                        )) ","))
  )

(defun org-fit-select-muscle ()
  (interactive)
  (org-fit-graph-change-attr-no-exec "exercise" "\"all\"")
  (org-fit-graph-change-attr
   "muscle"
   (format "\"%s\"" (completing-read "Muscle:" (org-fit-get-all-muscles)))))

(defun org-fit-get-all-exercises ()
  (cons "all"
        (split-string (funcall 
                       (intern (concat "org-babel-execute:" "python"))
                       "\",\".join(graph.get_all_exercises(trains).index.values)"
                       (org-combine-plists
                        '((:colname-names) (:rowname-names) (:result-params "replace") (:result-type . value) (:results . "replace") (:exports . "none") (:session . "plots") (:tangle . "no") (:hlines . "no") (:noweb . "no") (:cache . "no"))
                        )) ","))
  )

(defun org-fit-select-exercise ()
  (interactive)
  (org-fit-graph-change-attr-no-exec "muscle" "\"all\"")
  (org-fit-graph-change-attr
   "exercise"
   (format "\"%s\"" (completing-read "Exercise:" (org-fit-get-all-exercises)))))



;; https://stackoverflow.com/questions/6666862/org-mode-go-back-from-sparse-tree-to-previous-visibility/44158824#44158824
(defun org-fit-extend-file ()
  (interactive)
  (let ((buf (find-file-noselect org-fit-data-file)))
    (if (not (buffer-modified-p buf))
        (progn
          (funcall 
           (intern (concat "org-babel-execute:" "python"))
           "import extender
extender.extend_gym_file(org_file)"
           (org-combine-plists
            '((:colname-names) (:rowname-names) (:result-params "replace") (:result-type . value) (:results . "replace") (:exports . "none") (:session . "plots") (:tangle . "no") (:hlines . "no") (:noweb . "no") (:cache . "no"))
            ))
          (with-current-buffer buf
            (revert-buffer :ignore-auto :noconfirm))
  ))))




(require 'hydra)

(defun org-fit-graph-get-attr (attr)
  (goto-char (org-babel-where-is-src-block-head))
  (if (looking-at-p "#\\+begin_src")
      (progn
        (re-search-backward "#\\+header: :var" nil t)
        (re-search-forward ":var +" nil t)
        (let* ((var-str (buffer-substring-no-properties (point) (line-end-position)))
               (var-ass (split-string var-str " "))
               (var-vals (mapcar (lambda (x) (split-string x "=")) var-ass)))
          (cadr (car (seq-filter (lambda (x) (equal (car x) attr)) var-vals)))
      ))
  ))

(defun fit-unquote-parameter (value)
  (cond
   ((> (string-to-number value) 0)
    (string-to-number value))
   (value
    (let ((v1 (split-string value "\"")))
      (if (eq 1 (length v1))
          value
        (cadr v1))))
   (t nil)
   ))

(defun fit-quote-parameter (value)
  (cond
   ((numberp value)
    (format "%d" value))
   (t (format "\"%s\"" value))))


(defun fit-hydra-update ()
  (interactive)
  (org-fit-graph-change-attr-no-exec
   "period"
   (fit-quote-parameter hydra-fit/period))
  (org-fit-graph-change-attr-no-exec
   "value"
   (fit-quote-parameter hydra-fit/value))
  (org-fit-graph-change-attr-no-exec
   "months"
   (fit-quote-parameter hydra-fit/months))
  (org-fit-graph-change-attr-no-exec
   "muscle"
   (fit-quote-parameter hydra-fit/muscle))
  (org-fit-graph-change-attr-no-exec
   "exercise"
   (fit-quote-parameter hydra-fit/exercise))
   (org-fit-update-graph)
)

(defhydradio hydra-fit ()
  (value "Metric to display" ["volume" "reps" "max-weight" "sets" "max-reps" "epley"])
  (period "Period to group by" ["day" "week" "month" "all"])
  (months "Months" ["all" 1 2 3 4 5 6 7 8 9])
  (muscle "Filter by muscle" ["all"])
  (exercise "Filter by exercise" ["all"])
)

(defhydra hydra-fit ()
  "
_v_alue: % -15`hydra-fit/value _p_eriod: % -15`hydra-fit/period months (0..9/y): % -15`hydra-fit/months _m_uscle: % -15`hydra-fit/muscle _e_xercise: % -15`hydra-fit/exercise
"
  ("v" (hydra-fit/value) nil)
  ("p" (hydra-fit/period) nil)
  ("0" (lambda ()(interactive) (setq hydra-fit/months "all")) nil)
  ("1" (lambda ()(interactive) (setq hydra-fit/months 1)) nil)
  ("2" (lambda ()(interactive) (setq hydra-fit/months 2)) nil)
  ("3" (lambda ()(interactive) (setq hydra-fit/months 3)) nil)
  ("4" (lambda ()(interactive) (setq hydra-fit/months 4)) nil)
  ("5" (lambda ()(interactive) (setq hydra-fit/months 5)) nil)
  ("6" (lambda ()(interactive) (setq hydra-fit/months 6)) nil)
  ("7" (lambda ()(interactive) (setq hydra-fit/months 7)) nil)
  ("8" (lambda ()(interactive) (setq hydra-fit/months 8)) nil)
  ("9" (lambda ()(interactive) (setq hydra-fit/months 9)) nil)
  ("y" (lambda ()(interactive) (setq hydra-fit/months 12)) nil)
  ("m" (lambda ()(interactive)
         (setq hydra-fit/exercise "all")
         (setq hydra-fit/muscle (completing-read "Muscle:" (org-fit-get-all-muscles)))) nil)
  ("e" (lambda ()(interactive)
         (setq hydra-fit/muscle "all")
         (setq hydra-fit/exercise (completing-read "Exercise:" (org-fit-get-all-exercises))) nil))
  ("u" fit-hydra-update "update" :exit nil)
  ("x" fit-hydra-update "update" :exit t))


(defun fit-open-hydra ()
  (interactive)
  (setq hydra-fit/value (fit-unquote-parameter (org-fit-graph-get-attr "value")))
  (setq hydra-fit/period (fit-unquote-parameter (org-fit-graph-get-attr "period")))
  (setq hydra-fit/months (fit-unquote-parameter (org-fit-graph-get-attr "months")))
  (setq hydra-fit/muscle (fit-unquote-parameter (org-fit-graph-get-attr "muscle")))
  (setq hydra-fit/exercise (fit-unquote-parameter (org-fit-graph-get-attr "exercise")))
  (hydra-fit/body))

;; (general-define-key
;;  :keymaps 'org-mode-map
;;  "C-q" '(nil :which-key "fit-value")
;;  "C-q 1"  '((lambda () (interactive) (org-fit-graph-change-attr "value" "\"volume\"")) :which-key "Volume")
;;  "C-q 2"  '((lambda () (interactive) (org-fit-graph-change-attr "value" "\"reps\"")) :which-key "Reps")
;;  "C-q 3"  '((lambda () (interactive) (org-fit-graph-change-attr "value" "\"max-weight\"")) :which-key "Max weight")
;;  "C-q 4"  '((lambda () (interactive) (org-fit-graph-change-attr "value" "\"sets\"")) :which-key "Sets")
;;  "C-q 5"  '((lambda () (interactive) (org-fit-graph-change-attr "value" "\"max-reps\"")) :which-key "Max reps")
;;  "C-q 6"  '((lambda () (interactive) (org-fit-graph-change-attr "value" "\"epley\"")) :which-key "Epley")
;;  "C-w" '(nil :which-key "fit-period")
;;  "C-w 1"  '((lambda () (interactive) (org-fit-graph-change-attr "period" "\"month\"")) :which-key "Month")
;;  "C-w 2"  '((lambda () (interactive) (org-fit-graph-change-attr "period" "\"week\"")) :which-key "Week")
;;  "C-w 3"  '((lambda () (interactive) (org-fit-graph-change-attr "period" "\"day\"")) :which-key "Day")
;;  "C-e" '(nil :which-key "fit-months")
;;  "C-e 1"  '((lambda () (interactive) (org-fit-graph-change-attr "months" "1")) :which-key "1")
;;  "C-e 2"  '((lambda () (interactive) (org-fit-graph-change-attr "months" "2")) :which-key "2")
;;  "C-e 3"  '((lambda () (interactive) (org-fit-graph-change-attr "months" "3")) :which-key "3")
;;  "C-e 6"  '((lambda () (interactive) (org-fit-graph-change-attr "months" "6")) :which-key "6")
;;  "C-e 0"  '((lambda () (interactive) (org-fit-graph-change-attr "months" "\"all\"")) :which-key "all")
;;  "C-e 0"  '((lambda () (interactive) (org-fit-graph-change-attr "months" "\"all\"")) :which-key "all")
;;  "C-e m" 'org-fit-select-muscle
;;  "C-e e" 'org-fit-select-exercise
;;  )

(define-key org-mode-map (kbd "C-q") 'fit-open-hydra)

(provide 'gym)

