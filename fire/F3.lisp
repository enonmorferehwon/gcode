#! sbcl --script

(defun leggi-liste-da-file (file-path)
  "Legge il file EXIT e restituisce una lista di liste con i numeri."
  (with-open-file (in file-path :direction :input)
    (loop for line = (read-line in nil nil)
          while line
          collect (read-from-string line)))) ;; Legge la lista come espressione Lisp

(defun processa-liste (liste temp R)
  "Elabora ogni lista di numeri moltiplicandoli per -1.0d3 e dividendoli per la costante dei gas R e per la temperatura temp."
  (mapcar (lambda (lista)
            (mapcar (lambda (numero)
                      (exp (/ (* numero -1.0d3) R temp))) ;; Operazioni richieste
                    lista))
          liste))

(defun somma-lista (lista)
  "Calcola la somma di tutti gli elementi in una lista."
  (reduce #'+ lista))

(defun scrivi-risultati (liste file-path)
  "Scrive le liste processate e la somma di ciascuna su un file."
  (with-open-file (out file-path :direction :output :if-does-not-exist :create :if-exists :supersede)
    (dolist (lista liste)
      (let ((somma (somma-lista lista))) ;; Calcola la somma della lista
        (format out "(~{~a~^ ~}) ; Somma: ~a~%" lista somma))))) ;; Stampa lista e somma

(defun processa-multipli-file (input-files output-files temp R)
  "Elabora una lista di file di input e produce i file di output corrispondenti."
  (loop for input-file in input-files
        for output-file in output-files
        do (let* ((liste (leggi-liste-da-file input-file)) ;; Legge il file di input
                  (liste-processate (processa-liste liste temp R))) ;; Processa le liste
             (format t "Temperatura utilizzata per ~a: ~a K~%" input-file temp) ;; Stampa la temperatura
             (scrivi-risultati liste-processate output-file)))) ;; Scrive i risultati

;; Inizio dell'esecuzione
(let* ((R 8.314) ;; Costante dei gas
       (temp 298) ;; Temperatura definita come parametro
       (input-files '("EXIT_0" "EXIT_1" "EXIT_2" "EXIT_3" "EXIT_4" "EXIT_5" "EXIT_6" "EXIT_7"
		      "EXIT_8" "EXIT_9" "EXIT_10" "EXIT_11" "EXIT_12" "EXIT_C2" "EXIT_C3")) ;; input
       (output-files '("RESULT_0" "RESULT_1" "RESULT_2" "RESULT_3" "RESULT_4" "RESULT_5" "RESULT_6"
		        "RESULT_7" "RESULT_8" "RESULT_9" "RESULT_10" "RESULT_11" "RESULT_12"
			"RESULT_C2" "RESULT_C3"))) ;; output
  (processa-multipli-file input-files output-files temp R))
