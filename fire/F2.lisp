#! sbcl --script

(defun estrai-pesi-da-file (input-file-path output-file-path)
  "Legge un file e scrive i numeri dopo 'è:' separandoli in righe per ogni gruppo che segue 'Processando il Grafo' nel file di output."
  (with-open-file (in input-file-path :direction :input)
    (with-open-file (out output-file-path :direction :output :if-does-not-exist :create :if-exists :supersede)
      (loop with gruppo = nil
            for line = (read-line in nil nil) ;; Legge ogni linea
            while line
            do (cond
                 ;; Se troviamo 'Processando il Grafo', iniziamo un nuovo gruppo
                 ((search "Processando il Grafo" line)
                  (when gruppo
                    (format out "(~{~a~^ ~})~%" (nreverse gruppo))) ;; Scrive il gruppo racchiuso tra parentesi tonde in modalità LISP
                  (setf gruppo nil)) ;; Resetta il gruppo per iniziare un nuovo gruppo
                 ;; Estrae i numeri dopo 'è:'
                 ((search "è:" line)
                  (let ((start-pos (+ (search "è:" line) 3))) ;; Trova la posizione dopo 'è:'
                    (let ((numero (subseq line start-pos)))   ;; Estrae il numero
                      (push (string-trim '(#\Space) numero) gruppo))))) ;; Aggiungi il numero al gruppo
            finally
              (when gruppo
                (format out "(~{~a~^ ~})~%" (nreverse gruppo))))))) ;; Scrive l'ultimo gruppo

(defun estrai-pesi-da-file-multipli (input-files output-files)
  "Legge una lista di file di input e produce i file di output corrispondenti."
  (loop for input-file in input-files
        for output-file in output-files
        do (estrai-pesi-da-file input-file output-file)))

;; Specifica i file di input e output
(estrai-pesi-da-file-multipli '("END_0" "END_1" "END_2" "END_3" "END_4" "END_5" "END_6" "END_7"
				"END_8" "END_9" "END_10" "END_11" "END_12" "END_C2" "END_C3") ;; input
                              '("EXIT_0" "EXIT_1" "EXIT_2"  "EXIT_3"  "EXIT_4"  "EXIT_5"  "EXIT_6"
				 "EXIT_7"  "EXIT_8"  "EXIT_9"  "EXIT_10"  "EXIT_11"  "EXIT_12"  "EXIT_C2"
				  "EXIT_C3")) ;; output
