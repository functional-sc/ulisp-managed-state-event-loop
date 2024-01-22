const char LispLibrary[] PROGMEM = R"lisplibrary(

;; comments confuse uLisp, see README.md
;; to integrate with uLisp see http://www.ulisp.com/show?27OV

(defun wrap-fn-in-time (number-of-millis fn)
  (let ((next-time 0)) 
    (lambda (current-time state)
      (if (> current-time next-time) 
          (progn
            (setq next-time (+ number-of-millis current-time)) 
            (funcall fn state) )
          state))))

(defun list-to-array (l)
  (let ((a (make-array (length l))))
    (dotimes (i (length l))
      (setf (aref a i) (nth i l)))
    a))

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

)lisplibrary";
