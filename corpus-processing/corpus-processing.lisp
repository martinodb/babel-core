;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              ;;
;; Corpus Processing                                            ;;
;;                                                              ;;
;; System developed for parallel processing of text corpora.    ;;
;;                                                              ;;
;; Paul - March 2017                                            ;;
;;                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load with:
;; (ql:quickload :corpus-processing)

(in-package :utils)

(export '(process-corpus))

(defun process-corpus (&key function
                            function-kwargs
                            inputfile
                            outputfile
                            (number-of-threads 4)
                            (number-of-lines-per-thread 2000)
                            write-empty-lines-p)
  "Applies function to every line in inputfile and writes the result in outputfile.
   A higher number-of-threads results in a higher speed (if these threads are available).
   A higher number-of-lines-per-thread results in a higher speed, but also in a higher
   memory use."
  ;; Printing some information
  (format t "~%~%****************** Started Corpus Processing ******************")
  (format t "~%~%Inputfile: ~a" inputfile)
  (format t "~%Outputfile: ~a" outputfile)
  (format t "~%Applying function: ~a" function)
  (format t "~%Passing function arguments: ~a" function-kwargs)
  (format t "~%~%Threads: ~a" number-of-threads)
  (format t "~%Lines per thread: ~a" number-of-lines-per-thread)
  (format t "~%Lines per batch: ~a" (* number-of-threads number-of-lines-per-thread))

  ;; count number of lines in file
  (format t "~%Total number of lines: ")
  (let ((start-time (get-universal-time))
        (number-of-lines (number-of-lines inputfile))
        (number-of-lines-per-batch (* number-of-lines-per-thread number-of-threads)))
    (format t "~a" number-of-lines)

    ;; clear outputfile
    (with-open-file (stream outputfile :direction :output :if-exists :supersede :if-does-not-exist :create)
      (declare (ignore stream)))

    ;; Divide inputfile in batches, that are processed sequentionally (avoids reading whole corpus in memory)
    (multiple-value-bind (number-of-complete-batches number-of-lines-in-last-batch)
        (floor number-of-lines number-of-lines-per-batch)
      (format t "~%Number of batches: ~a complete + ~a lines in extra batch.~%" number-of-complete-batches number-of-lines-in-last-batch)
      (with-open-file (stream inputfile)
        (dotimes (n number-of-complete-batches)
          (format t "~%Started complete batch ~a/~a..." (+ 1 n) number-of-complete-batches)
          (process-batch-multi-threaded function function-kwargs stream
                                        number-of-threads number-of-lines-per-batch outputfile
                                        write-empty-lines-p)
          (format t "~%(Finished)"))
        (when (> number-of-lines-in-last-batch 0)
          (format t "~%Started extra batch...")
          (process-batch-multi-threaded function function-kwargs stream
                                        number-of-threads number-of-lines-in-last-batch outputfile
                                        write-empty-lines-p)
          (format t "~%(Finished)"))))
    (let ((finish-time (get-universal-time)))
      (multiple-value-bind (h m s)
          (seconds-to-hours-minutes-seconds (- finish-time start-time))
      (format t "~%~%Processing took ~a hours, ~a minutes and ~a seconds." h m s))))
    (format t "~%~%***************** Finished Corpus Processing *****************~%~%"))


(defun process-batch-multi-threaded (function function-kwargs stream number-of-threads number-of-lines outputfile write-empty-lines-p)
  "takes a batch of lines, divides them over threads, applies function on each line and writes
   the output to outputfile"
  (let ((list-of-thread-batches nil))
        ;; Divide lines over threads
        (multiple-value-bind (lines-per-thread lines-last-thread)
            (floor number-of-lines number-of-threads)
          ;; Read from input
          (format t "~%      Launching ~a threads with ~a lines and 1 thread with ~a lines. "
                  (- number-of-threads 1) lines-per-thread (+ lines-per-thread lines-last-thread))
            (dotimes (n (- number-of-threads 1)) ;; For each thread, except last
              (push (cons (+ 1 n) (stream->list stream :number-of-lines lines-per-thread)) list-of-thread-batches))
            ;; For last thread
            (push (cons number-of-threads (stream->list stream :number-of-lines (+ lines-per-thread lines-last-thread)))
                  list-of-thread-batches))
       
        ;; process each thread-batch
        (let* ((mailbox (make-mailbox :name "batch-mailbox"))
               (thread-list (mapcar #'(lambda (thread-batch)
                                        (process-run-function "line-processing" '()
                                                              #'process-list-of-lines function function-kwargs thread-batch mailbox))
                                    list-of-thread-batches))
               (mailbox-messages nil))
          ;; Wait for messages
          (mp:process-wait "waiting for threads to finish..." 'all-threads-dead thread-list)
          ;; Read batches from mailbox
          (while (not (mp:mailbox-empty-p mailbox))
            (push (mp:mailbox-read mailbox) mailbox-messages))
          ;; Write batches to outputfile
          (dolist (list-of-lines (sort mailbox-messages #'< :key #'car))
            (write-to-file outputfile (cdr list-of-lines) write-empty-lines-p)))))

(defun all-threads-dead (thread-list)
  "Returns t if all processes in thread-list are of status dead."
  (let ((all-dead t)
        (statuses (mapcar #'mp:process-whostate thread-list)))
    (dolist (status statuses)
      (unless (equalp "dead" status)
        (setf all-dead nil)))
    all-dead))

(defun write-to-file (output-file list-of-lines write-empty-lines-p)
  "writes list-of-lines to output-file (appending)"
  (with-open-file (stream output-file :direction :output :if-exists :append)
    (if write-empty-lines-p
      (mapcar #'(lambda (line) (format stream "~a~%" line)) list-of-lines)
      (mapcar #'(lambda (line) (when line (format stream "~a~%" line))) list-of-lines))))
  
(defun process-list-of-lines (function function-kwargs list-of-lines mailbox)
  "applies function to list-of-lines and returns the processed list-of-lines"
  (mailbox-send mailbox
                (cons (car list-of-lines)
                      (mapcar #'(lambda (line)
                                  (if function-kwargs
                                    (apply function line function-kwargs)
                                    (funcall function line)))
                              (cdr list-of-lines)))))
  


