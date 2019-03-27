
(in-package :utils)

(export '(json-stream-process))

(defun all-threads-dead (thread-list)
  "Returns t if all processes in thread-list are of status dead."
  (let ((all-dead t)
        (statuses (mapcar #'mp:process-whostate thread-list)))
    (dolist (status statuses)
      (unless (equalp "dead" status)
        (setf all-dead nil)))
    all-dead))

(defun append-to-file (stream list-of-lines output-format first-batch-p)
  "Appends list-of-lines to the stream according to the output format.
   In the end, wait for all the output to arrive through the stream."
  (case output-format
    (:json (let ((string-data (format nil "~{~a~^, ~}" list-of-lines)))
             (unless first-batch-p
               (write-string "," stream))
             (write-string string-data stream)))
    (:lines (mapcar #'(lambda (line) (write-line line stream)) list-of-lines)))
  (finish-output stream))

(defun process-list-of-json-objects (function function-kwargs list-of-json-objects mailbox)
  "Applies function to list-of-json-objects and returns the processed list-of-objects"
  (mailbox-send mailbox
                (cons (car list-of-json-objects)
                      (mapcar #'(lambda (json-object)
                                  (if function-kwargs
                                    (apply function json-object function-kwargs)
                                    (funcall function json-object)))
                              (cdr list-of-json-objects)))))

(defun alistp (value)
  "Checks if value is an alist."
  (or (null value)
      (and (consp value)
           (mapl (lambda (tree)
                   (unless (and (consp (first tree))
                                (not (consp (first (first tree))))
                                (listp (rest tree)))
                     (return-from alistp nil)))
                 value)
           t)))

(defun json-stream-process (&key function
                                 function-kwargs
                                 input-file
                                 output-file
                                 (output-format :json)
                                 (process-batch-fn #'identity)
                                 (write-empty-lines-p nil)
                                 (number-of-threads 4)
                                 (number-of-objects-per-thread 2000))  
  ;; Check if the output-format is correct
  (unless (or (eql output-format :json)
              (eql output-format :lines))
    (error (format nil "Incorrect output format. Please specify :json or :lines")))

  ;; Printing some information
  (format t "~%~%****************** Started Corpus Processing ******************")
  (format t "~%~%Inputfile: ~a" input-file)
  (format t "~%Outputfile: ~a" output-file)
  (format t "~%Output format: ~a" output-format)
  (format t "~%Applying function: ~a" function)
  (when function-kwargs (format t "~%Passing function arguments: ~a" function-kwargs))
  (when process-batch-fn (format t "~%Applying function ~a to every batch result" process-batch-fn))
  (format t "~%~%Threads: ~a" number-of-threads)
  (format t "~%Objects per thread: ~a" number-of-objects-per-thread)
  (format t "~%Objects per batch: ~a" (* number-of-threads number-of-objects-per-thread))

  ;; Count number of objects in file
  (let ((start-time (get-universal-time))
        (number-of-objects (number-of-lines input-file))
        (number-of-objects-per-batch (* number-of-objects-per-thread number-of-threads)))
    (format t "~%Total number of objects: ~a" number-of-objects)

    ;; Clear the outputfile
    ;; If the output-format is json, add the open square bracket
    (ensure-directories-exist output-file)
    (with-open-file (out-stream output-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (when (eql output-format :json)
        (write-string "[" out-stream)))

    ;; Divide inputfile in batches, that are processed sequentially (avoids reading whole corpus in memory)
    (multiple-value-bind (number-of-complete-batches number-of-objects-in-last-batch)
        (floor number-of-objects number-of-objects-per-batch)
      (format t "~%Number of batches: ~a complete + ~a objects in extra batch.~%"
              number-of-complete-batches number-of-objects-in-last-batch)
      (with-open-file (in-stream input-file :direction :input)
        (with-open-file (out-stream output-file :direction :output :if-exists :append)
          (dotimes (n number-of-complete-batches)
            (format t "~%Started complete batch ~a/~a..." (+ 1 n) number-of-complete-batches)
            (process-batch-multi-threaded function function-kwargs in-stream out-stream output-format process-batch-fn
                                          number-of-threads number-of-objects-per-batch write-empty-lines-p (= n 0)))
          (when (> number-of-objects-in-last-batch 0)
            (format t "~%Started extra batch...")
            (process-batch-multi-threaded function function-kwargs in-stream out-stream output-format process-batch-fn
                                          number-of-threads number-of-objects-in-last-batch write-empty-lines-p nil)))))

    ;; When the output-format is :json,
    ;; add the closing square bracket to the output file
    (when (eql output-format :json)
      (with-open-file (out-stream output-file :direction :output :if-exists :append)
        (write-string "]" out-stream)))
    (format t "~%(Finished)")

    ;; Finish
    (let ((finish-time (get-universal-time)))
      (multiple-value-bind (h m s)
          (seconds-to-hours-minutes-seconds (- finish-time start-time))
        (format t "~%~%Processing took ~a hours, ~a minutes and ~a seconds." h m s)))
    (format t "~%~%***************** Finished Corpus Processing *****************")))

(defun process-batch-multi-threaded (function
                                     function-kwargs
                                     in-stream
                                     out-stream
                                     output-format
                                     process-batch-fn
                                     number-of-threads
                                     number-of-objects
                                     write-empty-lines-p
                                     first-batch-p)
  (let ((list-of-thread-batches nil))
    ;; Divide objects over threads
    (multiple-value-bind (entries-per-thread entries-last-thread)
        (floor number-of-objects number-of-threads)
      (format t "~%      Launching ~a threads with ~a entries and 1 thread with ~a entries "
              (- number-of-threads 1) entries-per-thread (+ entries-per-thread entries-last-thread))
      (dotimes (n (- number-of-threads 1)) ;; For each thread, except last
        (push (cons (+ 1 n)
                    (mapcar #'cl-json:decode-json-from-string
                            (stream->list in-stream :number-of-lines entries-per-thread)))
              list-of-thread-batches))
      ;; For last thread
      (push (cons number-of-threads
                  (mapcar #'cl-json:decode-json-from-string
                          (stream->list in-stream :number-of-lines (+ entries-per-thread entries-last-thread))))
            list-of-thread-batches))
       
    ;; process each thread-batch
    (let* ((mailbox (make-mailbox :name "batch-mailbox"))
           (thread-list (mapcar #'(lambda (thread-batch)
                                    (process-run-function "json-processing" '()
                                                          #'process-list-of-json-objects
                                                          function function-kwargs thread-batch mailbox))
                                list-of-thread-batches))
           (mailbox-messages nil))
      ;; Wait for messages
      (mp:process-wait "waiting for threads to finish..." 'all-threads-dead thread-list)
      ;; Read batches from mailbox
      (while (not (mp:mailbox-empty-p mailbox))
        (push (mp:mailbox-read mailbox) mailbox-messages))
      ;; Process batches and write to output
      (let* ((list-of-results
              (loop for message in (sort mailbox-messages #'< :key #'car)
                    for message-content = (rest message)
                    if (alistp (first message-content))
                    append message-content
                    else
                    append (first message-content)))
             (encoded-results
              (mapcar #'cl-json:encode-json-to-string
                      (if write-empty-lines-p
                        list-of-results
                        (remove nil list-of-results)))))
        (funcall process-batch-fn list-of-results)
        (append-to-file out-stream
                        encoded-results
                        output-format
                        first-batch-p)))))
