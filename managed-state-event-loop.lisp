
;;
;; Given a single-argument-function, a millisecond-count and initial-state value
;; it returns a function that is scheduled to run at every millisecond-count.
;;
;; The returned function expects the ongoing millisecond count.  If fired, it
;; returns the new state, otherwise the old state is stored for the next
;; iteration and also retruned.
;;
(defun wrap-statetime (number-of-millis fn initial-state)
  (let ((next-time 0)
        (state     initial-state)) 
    (lambda (current-time)
      (if (>= current-time next-time) 
          (progn
            (setq next-time (+ number-of-millis current-time))
            (setq state (funcall fn state)) ))
      state)))

;;
;; Converts a list to an array the hard way, a complete Common Lisp
;; implementation includes 'coerce'
;;
(defun list-to-array (l)
  (let ((a (make-array (length l))))
    (dotimes (i (length l))
      (setf (aref a i) (nth i l)))
    a))

;;
;; Expects a list of millisecond time, function and initial state like this:
;;
;;                                          millis   lambda-fn     initial state
;;   (defvar time-fn-state-list (list (list 500      my-do-this1   10000)
;;                                    (list 1000     my-do-this2   20000)
;;                                    (list 7000     my-do-this3   30000)))
;;
;; relies on 'wrap-statetime' to execute after the millis have expired and
;; passes in the new computed state
;;
(defun run-event-loop (time-fn-state-list)
  (let* ((len         (length time-fn-state-list))
         (fn-array    (list-to-array (mapcar (lambda (fn-t)
                                               (wrap-statetime (first fn-t) (second fn-t) (third fn-t)) )
                                             time-fn-state-list))) )
    (loop
       (dotimes (i len) 
         (funcall (aref fn-array i) (millis)) ))))

