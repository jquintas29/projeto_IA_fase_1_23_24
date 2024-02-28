;;; Ficheiro projeto.lisp do Projeto 1
;;; Autor: João Quintiliano, 201900287
;;; Curso: Licenciatura em Engenharia Informática
;;; UC: Inteligência Artificial
;; Carrega outros ficheiros de código, escreve e lê ficheiros, 
;; e trata da interação com o utilizador

(load "C:/Users/joaoq/OneDrive/Faculdade/Inteligencia_artificial/Projeto_IA/projeto_IA_fase_1_23_24/puzzle.lisp")
(load "C:/Users/joaoq/OneDrive/Faculdade/Inteligencia_artificial/Projeto_IA/projeto_IA_fase_1_23_24/procura.lisp")

(defun iniciar ()
"Permite iniciar o programa, fazendo a leitura do teclado do estado inicial e do algoritmo a 
utilizar para procurar a solução (neste caso a procura na profundidade ou na largura)"
    (let* ((tipo-tabuleiro (ler-tabuleiro))
           (tabuleiro (car tipo-tabuleiro))
           (cavalo (ler-cavalo tabuleiro))
           (algoritmo (ler-algoritmo))
           (profundidade (ler-profundidade)))
        (let* ((tabuleiro-novo (substituir (nth 0 cavalo) (nth 1 cavalo) tabuleiro 'T))
               (no (cria-no tabuleiro-novo 0 (celula (nth 0 cavalo) (nth 1 cavalo) tabuleiro) nil (car (cdr tipo-tabuleiro)))))
            (escrever-no no)
            (cond
		        ((equal algoritmo 'bfs) (escrever-ficheiro (tempo-execucao-bfs algoritmo no 'no-solucaop 'sucessores (operadores)) (nth 2 tipo-tabuleiro) algoritmo profundidade))
		        ((equal algoritmo 'dfs) (escrever-ficheiro (tempo-execucao-dfs algoritmo no 'no-solucaop 'sucessores (operadores) profundidade) (nth 2 tipo-tabuleiro) algoritmo profundidade))
	        )
        )
    )
)

(defun escrever-ficheiro (lista tabuleiro algoritmo profundidade)
"Escreve no ficheiro resultado.dat o resultado da procura."
    (let ((arquivo (open "C:/Users/joaoq/OneDrive/Faculdade/Inteligencia_artificial/Projeto_IA/projeto_IA_fase_1_23_24/resultado.dat" :direction :output :if-exists :append :if-does-not-exist :create)))
        (cond (arquivo            
                (progn
                    (escrever-dados-procura lista)
                    (format arquivo "***********************************************************************************************~%")
                    (format arquivo " | Tabuleiro: ~a | Algoritmo: ~a | Profundidade máxima: ~a~%" tabuleiro algoritmo profundidade)
                    (escrever-dados-procura lista arquivo)
                    (format t "Resultado escrito no ficheiro com sucesso.")
                    ;; Fechar o arquivo
                    (close arquivo)
                ))
              (t (format t "Erro ao abrir o arquivo."))
        )
    )
)

(defun receber-tabuleiros ()
    (let ((arquivo (open "C:/Users/joaoq/OneDrive/Faculdade/Inteligencia_artificial/Projeto_IA/projeto_IA_fase_1_23_24/problemas.dat" :direction :input)))
        (cond 
            (arquivo   
             (loop for tabuleiros = (read arquivo nil nil)
                while tabuleiros
                collect tabuleiros
            ))
            (t (format t "Erro ao abrir o arquivo."))
        )
    )
)

(defun ler-tabuleiro ()
    "Permite fazer a leitura do algoritmo a utilizar."
    (let ((tabuleiros (receber-tabuleiros)) (tabuleiro-random (tabuleiro-aleatorio)))
     (progn
        (format t "Que tabuleiro quer usar para procurar? ~%")
        (format t "1- Tabuleiro A ~%")
        (print-tabuleiro (nth 0 tabuleiros))
        (format t "~%2- Tabuleiro B ~%")
        (print-tabuleiro (nth 1 tabuleiros))
        (format t "~%3- Tabuleiro C ~%")
        (print-tabuleiro (nth 2 tabuleiros))
        (format t "~%4- Tabuleiro D ~%")
        (print-tabuleiro (nth 3 tabuleiros))
        (format t "~%5- Tabuleiro E ~%")
        (print-tabuleiro (nth 4 tabuleiros))
        (format t "~%6- Tabuleiro F - tabuleiro gerado aleatoriamente ~%")
        (print-tabuleiro tabuleiro-random)
        (format t "~%7- Tabuleiro G - tabuleiro dado ~%")
        ;(print-tabuleiro (nth 5 tabuleiros))
        (case (read)
            (1 (list (nth 0 tabuleiros) 70 'tabuleiro-a)) 
            (2 (list (nth 1 tabuleiros) 60 'tabuleiro-b))
            (3 (list (nth 2 tabuleiros) 270 'tabuleiro-c))
            (4 (list (nth 3 tabuleiros) 600 'tabuleiro-d))
            (5 (list (nth 4 tabuleiros) 300 'tabuleiro-e))
            (6 (list tabuleiro-random 2000 'tabuleiro-f))
            (7 (list (nth 5 tabuleiros) 'tabuleiro-g))
        )
    ))
)
(defun ler-cavalo (tabuleiro)
"Permite ler a posição inicial do cavalo no tabuleiro"
    (progn 
        (format t "~d ~%" (print-tabuleiro tabuleiro))
        (format t "Insira a linha em que quer inserir ~%")
        (let ((pos-linha (- (read) 1)))
            (format t "Insira a coluna em que quer inserir ~%")
            (let ((pos-coluna (- (read) 1)))
                (cond 
                    ((or (< pos-linha 0) (>= pos-linha 10)  (< pos-coluna 0) (>= pos-coluna 10)) 
                        (progn (format t "Posição inválida") (ler-cavalo tabuleiro)))
                    ((equal (celula pos-linha pos-coluna tabuleiro) 'nil) 
                        (progn (format t "Posição inválida") (ler-cavalo tabuleiro)))
                    (t (list pos-linha pos-coluna))
                )
            )
        )
    )
)

(defun ler-algoritmo ()
"Permite fazer a leitura do algoritmo a utilizar."
    (progn
        (format t "Que algoritmo quer usar para procurar? ~%")
        (format t "1- Procura na largura (bfs) ~%")
        (format t "2- Procura na profundidade (dfs) ~%")
        (format t "3- Procura por A* ~%")
        (case (read)
            (1 'bfs) 
            (2 'dfs)
            (3 'A)
        )
    )
)

(defun ler-profundidade()
"Permite fazer a leitura da profundidade limite para o algoritmo dfs."
    (progn
        (format t "Qual a profundidade limite? ~%")
        (read)
    )
)

