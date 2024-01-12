const char LispLibrary[] PROGMEM = R"lisplibrary(

;; comments confuse uLisp, see README.md
;; to integrate with uLisp see http://www.ulisp.com/show?27OV

(defun wrap-statetime (number-of-millis fn initial-state)
  (let ((next-time 0)
        (state     initial-state)) 
    (lambda (current-time)
      (if (>= current-time next-time) 
          (progn
            (setq next-time (+ number-of-millis current-time))
            (setq state (funcall fn state)) ))
      state)))

(defun list-to-array (l)
  (let ((a (make-array (length l))))
    (dotimes (i (length l))
      (setf (aref a i) (nth i l)))
    a))

(defun run-event-loop (time-fn-state-list)
  (let* ((len         (length time-fn-state-list))
         (fn-array    (list-to-array (mapcar (lambda (fn-t)
                                               (wrap-statetime (first fn-t) (second fn-t) (third fn-t)) )
                                             time-fn-state-list))) )
    (loop
       (dotimes (i len) 
         (funcall (aref fn-array i) (millis)) ))))

)lisplibrary";
