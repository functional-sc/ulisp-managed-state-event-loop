
;;
;; Given a single-argument-function, a millisecond-count and initial-state value
;; it returns a function that is scheduled to run at every millisecond-count.
;;
;; The returned function expects the ongoing millisecond count.  If fired, it
;; returns the new state, otherwise the old state is returned.
;;
(defun wrap-fn-in-time (number-of-millis fn)
  (let ((next-time 0)) 
    (lambda (current-time state)
      (if (> current-time next-time) 
          (progn
            (setq next-time (+ number-of-millis current-time)) 
            (funcall fn state) )
          state))))

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
;; 
;; Optionally use a named-state to share the states across functions:
;;
;;                                          millis   lambda-fn     init  NAME
;;  (defvar time-fn-state-list (list (list 500      my-do-this1   10000  foo)
;;                                   (list 1000     my-do-this2   20000  foo)
;;                                   (list 7000     my-do-this3   30000  bar)))
;;

(defun run-event-loop (time-fn-state-list)
  (let* ((len                (length time-fn-state-list))
         (fn-array           (list-to-array (mapcar (lambda (fn-t) (wrap-fn-in-time (first fn-t) (second fn-t))) time-fn-state-list)))
         (state-offset-array (make-array len)) 
         (state-array        (make-array len)) 
         (name-offset-alist  nil) ) 
    
    (dotimes (i len)
      (setf (aref state-array i) (nth 2 (nth i time-fn-state-list)))
      (let ((this-name (nth 3 (nth i time-fn-state-list))))
        (setf name-offset-alist   (cons (cons this-name i) name-offset-alist))
        (setf (aref state-offset-array i) (if (car (assoc this-name (reverse name-offset-alist)))
                                             (cdr (assoc this-name (reverse name-offset-alist)))
                                             i)) ))

    (loop 
       (dotimes (i len) 
         (setf (aref state-array (aref state-offset-array i))
               (funcall (aref fn-array i) (millis) (aref state-array
                                                    (aref state-offset-array i) ))) ))))
