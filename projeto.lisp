;;; Ficheiro projeto.lisp
;; Carrega outros ficheiros de c�digo, escreve e l� ficheiros, 
;; e trata da intera��o com o utilizador

(load "C:/Users/joaoq/OneDrive/Faculdade/Inteligencia_artificial/Projeto_IA/projeto_IA_fase_1_23_24/puzzle.lisp")
(load "C:/Users/joaoq/OneDrive/Faculdade/Inteligencia_artificial/Projeto_IA/projeto_IA_fase_1_23_24/procura.lisp")

(defun iniciar ()
"Permite iniciar o programa, fazendo a leitura do teclado do estado inicial e do algoritmo a 
utilizar para procurar a solu��o (neste caso a procura na profundidade ou na largura)"
    (let* ((tipo-tabuleiro (ler-tabuleiro))
           (tabuleiro (funcall (car tipo-tabuleiro)))
           (cavalo (ler-cavalo tabuleiro))
           (algoritmo (ler-algoritmo))
           (profundidade (ler-profundidade)))
        (let* ((tabuleiro-novo (substituir (nth 0 cavalo) (nth 1 cavalo) tabuleiro 'T))
               (no (cria-no tabuleiro-novo 0 (celula (nth 0 cavalo) (nth 1 cavalo) tabuleiro) nil (car (cdr tipo-tabuleiro)))))
            (escrever-no no)
            (cond
		        ((equal algoritmo 'bfs) (escrever-dados-procura (tempo-execucao-bfs algoritmo no 'no-solucaop 'sucessores (operadores))))
		        ((equal algoritmo 'dfs) (escrever-dados-procura (tempo-execucao-dfs algoritmo no 'no-solucaop 'sucessores (operadores) profundidade)))
	        )
        )
    )
)

(defun ler-tabuleiro ()
    "Permite fazer a leitura do algoritmo a utilizar."
    (progn
        (format t "Que tabuleiro quer usar para procurar? ~%")
        (format t "1- Tabuleiro A ~%")
        (print-tabuleiro (tabuleiro-a))
        (format t "~%2- Tabuleiro B ~%")
        (print-tabuleiro (tabuleiro-b))
        (format t "~%3- Tabuleiro C ~%")
        (print-tabuleiro (tabuleiro-c))
        (format t "~%4- Tabuleiro D ~%")
        (print-tabuleiro (tabuleiro-d))
        (format t "~%5- Tabuleiro E ~%")
        (print-tabuleiro (tabuleiro-e))
        (format t "~%6- Tabuleiro F - tabuleiro gerado aleatoriamente ~%")
        (case (read)
            (1 '(tabuleiro-a 70))
            (1 '(tabuleiro-a 70)) 
            (2 '(tabuleiro-b 60))
            (3 '(tabuleiro-c 270))
            (4 '(tabuleiro-d 600))
            (5 '(tabuleiro-e 300))
            (6 '(tabuleiro-aleatorio 2000))
        )
    )
)

(defun ler-cavalo (tabuleiro)
"Permite ler a posi��o inicial do cavalo no tabuleiro"
    (progn 
        (format t "~d ~%" (print-tabuleiro tabuleiro))
        (format t "Insira a linha em que quer inserir ~%")
        (let ((pos-linha (read)))
            (format t "Insira a coluna em que quer inserir ~%")
            (let ((pos-coluna (read)))
                (cond 
                    ((or (< pos-linha 0) (>= pos-linha 10)  (< pos-coluna 0) (>= pos-coluna 10)) 
                        (progn (format t "Posi��o inv�lida") (ler-cavalo tabuleiro)))
                    ((equal (celula pos-linha pos-coluna tabuleiro) 'nil) 
                        (progn (format t "Posi��o inv�lida") (ler-cavalo tabuleiro)))
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
