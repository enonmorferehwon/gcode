#! /usr/bin/env -S sbcl --script ;; If needed, change the shebang 

(defun estrai-pesi-da-file (input-file-path output-file-path)
"Reads the input files and searches the string 'is:' then writes in the output files the numbers after 'is:' to create a vector of weights of the graphs belonging to a given group of either spanning trees or branched cycles. Different groups are individuated by the string: 'Processing the Graph' in the same input files"
  (with-open-file (in input-file-path :direction :input)
    (with-open-file (out output-file-path :direction :output :if-does-not-exist :create :if-exists :supersede)
      (loop with gruppo = nil
            for line = (read-line in nil nil) ;; Reads each line of the input files
            while line
            do (cond
            ;; The string 'Processing the Graph' in the input file separates the different graph groups
            ((search "Processing the Graph" line)
            (when gruppo
            (format out "(~{~a~^ ~})~%" (nreverse gruppo))) ;; Writes the group in Lisp format
                (setf gruppo nil)) ;; Resets the group to start a new one
                 ;; Extracts numbers after 'is:'
                 ((search "is:" line)
                  (let ((start-pos (+ (search "is:" line) 3))) ;; Parses the position after 'is:'
                   (let ((numero (subseq line start-pos)))   ;; Extracts the number
                    (push (string-trim '(#\Space) numero) gruppo))))) ;; Adds the group number
            finally
              (when gruppo
                (format out "(~{~a~^ ~})~%" (nreverse gruppo))))))) ;; Writes the last group

(defun estrai-pesi-da-file-multipli (input-files output-files)
  "Reads a list of input files and produces the corresponding output files"
  (loop for input-file in input-files
        for output-file in output-files
        do (estrai-pesi-da-file input-file output-file)))

;; Driver-like function; input and output file have to be specified 
(estrai-pesi-da-file-multipli 
  '("END_0" "END_1" "END_2" "END_3" "END_4" "END_5" "END_6" "END_7"
    "END_8" "END_9" "END_10" "END_11" "END_12" "END_C1" "END_C2"
    "END_C3" "END_C4" "END_C5" "END_C6" "END_C7") ;; input
  '("EXIT_0" "EXIT_1" "EXIT_2"  "EXIT_3"  "EXIT_4"  "EXIT_5"  "EXIT_6"
    "EXIT_7"  "EXIT_8"  "EXIT_9"  "EXIT_10"  "EXIT_11"  "EXIT_12"  "EXIT_C1"
    "EXIT_C2"  "EXIT_C3"  "EXIT_C4"  "EXIT_C5" "EXIT_C6" "EXIT_C7")) ;; output
