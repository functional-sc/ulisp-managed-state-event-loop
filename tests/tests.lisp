;;
;; uLisp on an Adafruit Trinkey runs at about 2000 fns/sec and at 4% overhead
;; in raw speed over straight function call.
;;
;; SBCL on a 2020-era laptop runs at 9,500,000 fn/sec
;;
(defun test-event-loop (millis-to-run time-fn-state-list)
  (let* ((len         (length time-fn-state-list))
         (fn-array    (list-to-array (mapcar (lambda (fn-t)
                                               (wrap-statetime (first fn-t) (second fn-t) (third fn-t)) )
                                             time-fn-state-list)))
         (last-state nil)
         (start-time (millis))
         (stop-time  (+ start-time millis-to-run)))

    (format t "  START: ~a~%" start-time)

    (loop
      (dotimes (i len)
        (setf last-state (funcall (aref fn-array i) (millis))) )
       (if (>= (millis) stop-time) (return)) )

    (setf stop-time (millis))
    (format t "  DEBUG: ~a~%" last-state)
    (format t "   STOP: ~a~%" stop-time)
    (format t "   DIFF: ~a~%" (- stop-time start-time))
    (if (< 0 (- stop-time start-time))
        (format t "FNS/SEC: ~a~%" (/ last-state (/ (- stop-time start-time) 1000))))
    last-state))

;; expect about 2000 fns/sec running as fast as we can with no latency
(test-event-loop 10000 (list (list 0 (lambda (x) (1+ x)) 0)))

;; expect 11 iterations: 1 second runtime, every 1/10th of a second +fencepost
(test-event-loop 1000 (list (list 100 (lambda (x) (1+ x)) 0)))

