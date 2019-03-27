;; Copyright 2019 AI Lab, Vrije Universiteit Brussel - Sony CSL Paris

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================
(in-package :utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions implementing ssh and scp interfacing (via shell) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(scp-get ssh-clear-dir))

(defun scp-get (host username password remote-file local-file)
  "Downloads a file from a remote machine (e.g. robot) on your local machine."
  ;; Check for sshpass
  (unless (sshpass-installed-p)
    (error "sshpass is not installed or not in the path. You can install it e.g. with homebrew."))
  ;; Ensure the local-file directory exists
  (ensure-directories-exist local-file)
  ;; Do the scp request, avoiding the password prompt with sshpass
  (let ((arg (format nil "sshpass -p ~a /usr/bin/scp ~a@~a:~a ~a"
                     password username host remote-file local-file)))
    ;; If exec-and-return returns something, then something went wrong (it should just have done its job)
    (let ((error
           #+lispworks (exec-and-return  arg)
           #-lispworks (exec-and-return "/bin/sh" "-c" arg)
           ))
      (when error (error (format nil "SCP command failed: ~{~a ~}" error))))))

(defun ssh-clear-dir (host username password remote-dir)
  "Clears a given dir on a remote machine (e.g. robot). Will execute 'rm remote-dir/*'"
  ;; Check for sshpass
  (unless (sshpass-installed-p)
    (error "sshpass is not installed or not in the path. You can install it e.g. with homebrew."))
  ;; Do the scp request, avoiding the password prompt with sshpass
  (let* ((arg (format nil "sshpass -p ~a ssh ~a@~a 'rm ~a/*'" password username host remote-dir))
         (error
            #+lispworks (exec-and-return arg)
            #-lispworks (exec-and-return "/bin/sh" "-c" arg)
            ))
      (when error
        (error (format nil "RM command failed: ~{~a ~}" error)))
      t))

(defun sshpass-installed-p ()
  "Returns true if sshpass is installed and in the path, nil otherwise."
  (exec-and-return "which" "sshpass"))
