#! /usr/bin/env -S sbcl --script ;; If needed, change the shebang 

(defun leggi-liste-da-file (file-path)
  "Reads the EXIT input files and writes a list of lists ;)"
  (with-open-file (in file-path :direction :input)
    (loop for line = (read-line in nil nil)
          while line
          collect (read-from-string line)))) ;; Read a Lisp list

(defun processa-liste (liste temp R)
  "Processes each list of numbers by multiplying each element by -1.0d3 and dividing by the gas constant R and the temperature temp"
  (mapcar (lambda (lista)
            (mapcar (lambda (numero)
                      (exp (/ (* numero -1.0d3) R temp))) ;; Computes the exponential kernel
                    lista))
          liste))

(defun somma-lista (lista)
  "Computes the sum of all elements in a list"
  (reduce #'+ lista))

(defun scrivi-risultati (liste file-path)
  "Writes the processed lists and the sum of the weights of each list to a file after converting the energies to frequencies"
  (with-open-file (out file-path
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (dolist (lista liste)
      (let ((somma (somma-lista lista)))     ;; Computes the sum of the energies in the list
        (format out "(~{~a~^ ~})~%" lista)   ;; Prints the frequency list after converting the energies ...
        (format out "Summa: ~a~%" somma))))) ;; ... and their sum

(defun processa-multipli-file (input-files output-files temp R)
  "Processes a set of input files and produces the corresponding output files"
  (loop for input-file in input-files
        for output-file in output-files
        do (let* ((liste (leggi-liste-da-file input-file)) ;; Reads the input file
                  (liste-processate (processa-liste liste temp R))) ;; Processes the lists
             (format t "Temperature utilized for ~a: ~a K~%" input-file temp) ;; Prints the temperature
             (scrivi-risultati liste-processate output-file)))) ;; Writes the results

;; Driver-like function; input and output files have to be specified 
;; Run with the temperature specified (number) on the command line, e.g., ./F3_m.lisp number, where number is an integer or float
(let* ((args sb-ext:*posix-argv*)            
       (temp-str (second args))              
       (temp (if temp-str
                 (read-from-string temp-str) ;; Reads temperature (integer or float)
                 298.0))                     ;; Default temperature value if not specified on the command line
       (R 8.314)                             ;; Ideal gas constant
       (input-files '("EXIT_0" "EXIT_1" "EXIT_2" "EXIT_3" "EXIT_4" "EXIT_5" "EXIT_6" "EXIT_7"
                      "EXIT_8" "EXIT_9" "EXIT_10" "EXIT_11" "EXIT_12" "EXIT_C1" "EXIT_C2"
		      "EXIT_C3" "EXIT_C4" "EXIT_C5" "EXIT_C6" "EXIT_C7"))
       (output-files '("RESULT_0" "RESULT_1" "RESULT_2" "RESULT_3" "RESULT_4" "RESULT_5" "RESULT_6"
                       "RESULT_7" "RESULT_8" "RESULT_9" "RESULT_10" "RESULT_11" "RESULT_12"
                       "RESULT_C1" "RESULT_C2" "RESULT_C3" "RESULT_C4" "RESULT_C5"
		       "RESULT_C6" "RESULT_C7")))
  (format t "Temperature utilized: ~a K~%" temp)
  (processa-multipli-file input-files output-files temp R))
