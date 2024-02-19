;;;Ficheiro puzzle.lisp
;;Ficheiro com operadores e heur�sticas espec�ficos do dom�nio da aplica��o

(defun tabuleiro-jogado ()
"Tabuleiro de teste igual ao anterior mas tendo sido colocado o cavalo na posi��o: i=0 e j=0"
  '(
    (T 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 49 13 12 26 60) 
    (0 27 17 83 34 93 74 52 45 80) 
    (69 9 77 95 55 39 91 73 57 30) 
    (24 15 22 86 1 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71) 
    (99 51 6 18 53 28 7 63 10 88) 
    (59 42 46 85 90 75 87 43 20 31) 
    (3 61 58 44 65 82 19 4 35 62) 
    (33 70 84 40 66 38 92 67 98 97)
    )
)


(defun tabuleiro-a ()
"Tabuleiro para o problema A"
  '(
    (02 20 44 nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil 03 30 nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil 22 nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil)
    )
)

(defun tabuleiro-b ()
"Tabuleiro para o problema B"
  '(
    (02 nil 04 nil 06 nil 08 nil 10 nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil 03 nil 05 nil 07 nil 09 nil 11) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil)
    )
)

(defun tabuleiro-c ()
"Tabuleiro para o problema c"
  '(
    (01 12 03 23 nil 88 nil nil nil nil) 
    (21 45 43 nil nil nil nil nil nil nil) 
    (nil 56 nil 78 nil nil nil nil nil nil) 
    (89 nil 99 54 nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil)
    )
)

(defun tabuleiro-d ()
"Tabuleiro para o problema d"
  '(
    (98 97 96 95 94 93 92 91 90 89) 
    (01 02 03 04 05 55 06 07 08 09) 
    (nil 66 nil nil nil nil nil nil nil 11) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil 22 nil nil nil nil nil 33 nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil 88 nil nil nil 44 nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil 77 nil nil nil nil nil) 
    (nil nil nil nil nil nil 99 nil nil nil)
    )
)

(defun tabuleiro-e ()
"Tabuleiro para o problema e"
  '(
    (nil 05 nil nil nil 15 nil nil nil 25) 
    (nil nil nil 06 nil nil nil 16 nil nil) 
    (nil 04 nil nil nil 14 nil nil nil 24) 
    (nil nil nil 07 nil nil nil 17 nil nil) 
    (nil 03 nil nil nil 13 nil nil nil 23) 
    (nil nil nil 08 nil nil nil 18 nil nil) 
    (nil 02 nil nil nil 12 nil nil nil 22) 
    (nil nil nil 09 nil nil nil 19 nil nil) 
    (nil 01 nil nil nil 11 nil nil nil 21) 
    (nil nil nil 10 nil nil nil 20 nil nil)
    )
)

;;; M�todos seletores
(defun linha (indice tabuleiro)
"Fun��o que recebe um �ndice e o tabuleiro e retorna uma lista que representa essa linha do
tabuleiro."
    (cond ((and (integerp indice) (>= indice 0) (< indice (length tabuleiro))) 
            (nth indice tabuleiro))
          (t (error "Indice de linha inv�lido"))
    )
)

(defun celula (indiceLinha indiceColuna tabuleiro)
"Fun��o que recebe dois �ndices e o tabuleiro e retorna o valor presente nessa c�lula do
tabuleiro."
    (cond ((and (integerp indiceColuna) (>= indiceColuna 0) (< indiceColuna (length (linha indiceLinha tabuleiro)))) 
            (nth indiceColuna (linha indiceLinha tabuleiro)))
          (t (error "Indice de coluna inv�lido"))
    )
)

; n�o parece estar aqui a fazer nada
(defun alisa (lista)
    (cond ((null lista) nil)
          ((atom (car lista)) (cons (car lista) (alisa (cdr lista))))
          (t (append (alisa (car lista)) (alisa (cdr lista))))
    )
)

(defun lista-numeros (&optional (n 100))
"Fun��o que recebe um n�mero positivo n e cria uma lista com todos os n�meros
entre 0 (inclusiv�) e o n�mero passado como argumento (exclusiv�). Por default o n � 100"
    (cond ((= n 0) nil)
          (t (cons (- n 1) (lista-numeros (- n 1)))))
)

(defun remover-se(pred lista)
"Devolve a resconstru��o da lista passada como argumento sem os elementos que verificam a
condi��o/predicado passado pelo argumento pred 
exemplo: (remover-se #'(lambda (x) (= x 0)) lista) -> devolve a lista sem os �tomos 0"
    (cond ((null lista) NIL) 
          ((funcall pred (car lista)) (remover-se pred (cdr lista)))
          (T (cons (car lista) (remover-se pred (cdr lista))))
    )
)

(defun baralhar (lista)
"Recebe uma lista e ir� mudar aleatoriamente os n�meros de forma a criar uma lista baralhada."
    (cond ((null lista) nil)
        (t (let* ((index (random (length lista)))
                  (numberRandom (nth index lista))
                  (nova-lista (remover-se #'(lambda (x) (= x numberRandom)) lista)))
                (cons numberRandom (baralhar nova-lista))
            )
        )
    )
)

(defun tabuleiro-aleatorio (&optional (lista (baralhar (lista-numeros))) (n 10))
"Recebe uma lista (por default ser� chamada a fun��o baralhar com argumento a fun��o lista-numeros)
e vai devolver essa lista dividida em sublistas de n elementos recebido como par�metro pelo argumento n
(por default ter� o valor 10)"
    (cond ((null lista) nil)
          (t (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))
    )
)

(defun substituir-posicao (indice lista &optional (valor nil))
"Fun��o que recebe um �ndice, uma lista e um valor (por default o valor � NIL) e
substitui pelo valor pretendido nessa posi��o."
    (cond 
        ((and (integerp indice) (>= indice 0) (< indice (length lista))) 
            (let ((nova-lista lista))
                (setf (nth indice nova-lista) valor)
            )
        )
        (t (error "Indice para substituir inv�lido"))
    )
)

(defun substituir (indice-linha indice-coluna tabuleiro &optional (valor 'nil))
"Fun��o que recebe dois �ndices, o tabuleiro e um valor (por default o valor � NIL). A
fun��o retorna o tabuleiro com a c�lula substitu�da pelo valor pretendido."
    (let ((novo-tabuleiro tabuleiro) (numero (nth indice-coluna (nth indice-linha tabuleiro))))
        (substituir-posicao indice-coluna (linha indice-linha novo-tabuleiro) valor)
        (cond ((not (equal valor 'nil)) (substituir-simetrico numero tabuleiro)))    
    )
)

; (defun substituir (indice-linha indice-coluna tabuleiro &optional (valor 'nil))
; "Fun��o que recebe dois �ndices, o tabuleiro e um valor (por default o valor � NIL). A
; fun��o retorna o tabuleiro com a c�lula substitu�da pelo valor pretendido."
;     (let ((novo-tabuleiro tabuleiro) (numero (nth indice-coluna (nth indice-linha tabuleiro))))
;         (cond
;             ;((equal numero valor) (format t "ERRO: O cavalo j� est� nesta posi��o"))
;             ((not (equal valor 'nil)) (substituir-simetrico numero tabuleiro))
;         )    
;         (substituir-posicao indice-coluna (linha indice-linha novo-tabuleiro) valor)
;     )
; )


(defun substituir-simetrico (numero tabuleiro)
"Fun��o que recebe um n�mero, um tabuleiro. A fun��o vai retornar o tabuleiro 
com o sim�trico do n�mero da vari�vel numero substituido por NIL"
    (let* ((simetrico (obter-simetrico numero)) (posicao (procurar-posicao tabuleiro simetrico)))
        (cond 
            ((null posicao) nil)
            (t (substituir (nth 0 posicao) (nth 1 posicao) tabuleiro))
        )
    )
)

; (defun substituir-simetrico (numero tabuleiro &optional (valor-linha 0))
; "Fun��o que recebe um n�mero, um tabuleiro. A fun��o vai retornar o tabuleiro 
; com o sim�trico do n�mero da vari�vel numero substituido por NIL"
;     (cond 
;         ((>= valor-linha (length tabuleiro)) nil)
;         ((let ((lista-linha (linha valor-linha tabuleiro)) (simetrico (obter-simetrico numero)))
;             (cond 
;                 ((find simetrico lista-linha) (substituir valor-linha (position simetrico lista-linha) tabuleiro))
;                 (t (substituir-simetrico numero tabuleiro (+ valor-linha 1)))
;             )
;         ))
;     )
; )

(defun obter-simetrico (numero)
"Fun��o que recebe um n�mero e devolve o sim�trico desse n�mero (exemplo: 56->65)"
  (parse-integer (coerce (reverse (coerce (princ-to-string numero) 'list)) 'string))
)

(defun substituir-duplo (tabuleiro &optional (numero 0))
    (cond ((= numero 0) (setq numero (maior-duplo tabuleiro))))
    (let ((posicao (procurar-posicao tabuleiro numero)))
        (substituir (nth 0 posicao) (nth 1 posicao) tabuleiro)
    )   
)

(defun maior-duplo (tabuleiro)
"Fun��o que recebe um tabuleiro e retorna o duplo maior desse tabuleiro"
    (let* ((tabuleiro-liso (alisa tabuleiro)) (duplos (remove-if-not #'isduplo tabuleiro-liso)))
        (setq duplos (apply #'max duplos))
        (cond ((null duplos) nil) (t duplos))
    )
)

(defun isduplo (numero)
"Recebe um n�mero e retorna verdadeiro (T) se o n�mero for um duplo, retorna nil caso contr�rio"
    (cond 
        ((null numero) nil)
        ((= (mod numero 11) 0) T)
        (t nil)
    )
)

(defun procurar-posicao (tabuleiro valor &optional (valor-linha 0))
"Fun��o que recebe o tabuleiro e devolve a posi��o (i j) em que se encontra o
valor passado por argumento. Pode ser usado para procurar o cavalo.
Caso o valor n�o se encontre no tabuleiro retorna NIL."
    (cond 
        ((>= valor-linha (length tabuleiro)) nil)
        ((let ((lista-linha (linha valor-linha tabuleiro)))
            (cond 
                ((find valor lista-linha) (cons valor-linha (cons (position valor lista-linha) nil)))
                (t (procurar-posicao tabuleiro valor (+ valor-linha 1)))
            )
        ))
    )
)

(defun print-tabuleiro (tabuleiro)
    (cond 
        ((null tabuleiro) nil)
        (t (progn 
            (format t "~d~%" (car tabuleiro))
            (print-tabuleiro (cdr tabuleiro))
           )
        )
    )
)

;;; Operadores do problema (ver lab7)
;; operador-1
;usar fun��o substituir() para substituir pelo valor, e depois testar se � sim�trico 
;ou duplo e aplicar a regra
(defun operador-1 (tabuleiro)
"Fun��o que recebe um tabuleiro. Realiza um movimento do cavalo 2 casas para baixo e 1 para a esquerda.
Devolve o tabuleiro com a nova posi��o do cavalo."
    (let* ((posicao-cavalo (procurar-posicao tabuleiro 'T))
           (nova-pos-linha (+ (nth 0 posicao-cavalo) 2))
           (nova-pos-coluna (- (nth 1 posicao-cavalo) 1)))
        (cond 
            ((or (>= nova-pos-linha (length (car tabuleiro))) (>= nova-pos-coluna (length tabuleiro)) (< nova-pos-linha 0) (< nova-pos-coluna 0)) 
                (error "operador-1 n�o pode ser realizado, operador est� fora dos limites do tabuleiro."))
            ((equal (nth nova-pos-coluna (nth nova-pos-linha tabuleiro)) 'nil) (error "Esta casa j� foi visitada"))
        )
        (substituir nova-pos-linha nova-pos-coluna tabuleiro 'T)
        (substituir (nth 0 posicao-cavalo) (nth 1 posicao-cavalo) tabuleiro)
        (print-tabuleiro tabuleiro)
    )
)

;;operador-2
(defun operador-2 (tabuleiro)
"Fun��o que recebe um tabuleiro. Realiza um movimento do cavalo 2 casas para baixo e 1 para a esquerda.
Devolve o tabuleiro com a nova posi��o do cavalo."
    (let* ((posicao-cavalo (procurar-posicao tabuleiro 'T))
           (nova-pos-linha (+ (nth 0 posicao-cavalo) 2))
           (nova-pos-coluna (+ (nth 1 posicao-cavalo) 1)))
        (cond 
            ((or (>= nova-pos-linha (length (car tabuleiro))) (>= nova-pos-coluna (length tabuleiro)) (< nova-pos-linha 0) (< nova-pos-coluna 0)) 
                (error "operador-2 n�o pode ser realizado, operador est� fora dos limites do tabuleiro."))
            ((equal (nth nova-pos-coluna (nth nova-pos-linha tabuleiro)) 'nil) (error "Esta casa j� foi visitada"))
        )
        (substituir nova-pos-linha nova-pos-coluna tabuleiro 'T)
        (substituir (nth 0 posicao-cavalo) (nth 1 posicao-cavalo) tabuleiro)
        (print-tabuleiro tabuleiro)
    )
)

;;operador-3
(defun operador-3 (tabuleiro)
"Fun��o que recebe um tabuleiro. Realiza um movimento do cavalo 2 casas para a direita e 1 para baixo.
Devolve o tabuleiro com a nova posi��o do cavalo."
    (let* ((posicao-cavalo (procurar-posicao tabuleiro 'T))
           (nova-pos-linha (+ (nth 0 posicao-cavalo) 1))
           (nova-pos-coluna (+ (nth 1 posicao-cavalo) 2)))
        (cond 
            ((or (>= nova-pos-linha (length (car tabuleiro))) (>= nova-pos-coluna (length tabuleiro)) (< nova-pos-linha 0) (< nova-pos-coluna 0)) 
                (error "operador-3 n�o pode ser realizado, operador est� fora dos limites do tabuleiro."))
            ((equal (nth nova-pos-coluna (nth nova-pos-linha tabuleiro)) 'nil) (error "Esta casa j� foi visitada"))
        )
        (substituir nova-pos-linha nova-pos-coluna tabuleiro 'T)
        (substituir (nth 0 posicao-cavalo) (nth 1 posicao-cavalo) tabuleiro)
        (print-tabuleiro tabuleiro)
    )
)

;;operador-4
(defun operador-4 (tabuleiro)
"Fun��o que recebe um tabuleiro. Realiza um movimento do cavalo 2 casas para a direita e 1 para cima
Devolve o tabuleiro com a nova posi��o do cavalo."
    (let* ((posicao-cavalo (procurar-posicao tabuleiro 'T))
           (nova-pos-linha (- (nth 0 posicao-cavalo) 1))
           (nova-pos-coluna (+ (nth 1 posicao-cavalo) 2)))
        (cond 
            ((or (>= nova-pos-linha (length (car tabuleiro))) (>= nova-pos-coluna (length tabuleiro)) (< nova-pos-linha 0) (< nova-pos-coluna 0)) 
                (error "operador-4 n�o pode ser realizado, operador est� fora dos limites do tabuleiro."))
            ((equal (nth nova-pos-coluna (nth nova-pos-linha tabuleiro)) 'nil) (error "Esta casa j� foi visitada"))
        )
        (substituir nova-pos-linha nova-pos-coluna tabuleiro 'T)
        (substituir (nth 0 posicao-cavalo) (nth 1 posicao-cavalo) tabuleiro)
        (print-tabuleiro tabuleiro)
    )
)

;;operador-5
(defun operador-5 (tabuleiro)
"Fun��o que recebe um tabuleiro. Realiza um movimento do cavalo 2 casas cima e 1 para a direita
Devolve o tabuleiro com a nova posi��o do cavalo."
    (let* ((posicao-cavalo (procurar-posicao tabuleiro 'T))
           (nova-pos-linha (- (nth 0 posicao-cavalo) 2))
           (nova-pos-coluna (+ (nth 1 posicao-cavalo) 1)))
        (cond 
            ((or (>= nova-pos-linha (length (car tabuleiro))) (>= nova-pos-coluna (length tabuleiro)) (< nova-pos-linha 0) (< nova-pos-coluna 0)) 
                (error "operador-5 n�o pode ser realizado, operador est� fora dos limites do tabuleiro."))
            ((equal (nth nova-pos-coluna (nth nova-pos-linha tabuleiro)) 'nil) (error "Esta casa j� foi visitada"))
        )
        (substituir nova-pos-linha nova-pos-coluna tabuleiro 'T)
        (substituir (nth 0 posicao-cavalo) (nth 1 posicao-cavalo) tabuleiro)
        (print-tabuleiro tabuleiro)
    )
)

;;operador-6
(defun operador-6 (tabuleiro)
"Fun��o que recebe um tabuleiro. Realiza um movimento do cavalo 2 casas cima e 1 para a esquerda
Devolve o tabuleiro com a nova posi��o do cavalo."
    (let* ((posicao-cavalo (procurar-posicao tabuleiro 'T))
           (nova-pos-linha (- (nth 0 posicao-cavalo) 2))
           (nova-pos-coluna (- (nth 1 posicao-cavalo) 1)))
        (cond 
            ((or (>= nova-pos-linha (length (car tabuleiro))) (>= nova-pos-coluna (length tabuleiro)) (< nova-pos-linha 0) (< nova-pos-coluna 0)) 
                (error "operador-6 n�o pode ser realizado, operador est� fora dos limites do tabuleiro."))
            ((equal (nth nova-pos-coluna (nth nova-pos-linha tabuleiro)) 'nil) (error "Esta casa j� foi visitada"))
        )
        (substituir nova-pos-linha nova-pos-coluna tabuleiro 'T)
        (substituir (nth 0 posicao-cavalo) (nth 1 posicao-cavalo) tabuleiro)
        (print-tabuleiro tabuleiro)
    )
)

;;operador-7
(defun operador-7 (tabuleiro)
"Fun��o que recebe um tabuleiro. Realiza um movimento do cavalo 2 casas a esquerda e 1 para cima
Devolve o tabuleiro com a nova posi��o do cavalo."
    (let* ((posicao-cavalo (procurar-posicao tabuleiro 'T))
           (nova-pos-linha (- (nth 0 posicao-cavalo) 1))
           (nova-pos-coluna (- (nth 1 posicao-cavalo) 2)))
        (cond 
            ((or (>= nova-pos-linha (length (car tabuleiro))) (>= nova-pos-coluna (length tabuleiro)) (< nova-pos-linha 0) (< nova-pos-coluna 0)) 
                (error "operador-7 n�o pode ser realizado, operador est� fora dos limites do tabuleiro."))
            ((equal (nth nova-pos-coluna (nth nova-pos-linha tabuleiro)) 'nil) (error "Esta casa j� foi visitada"))
        )
        (substituir nova-pos-linha nova-pos-coluna tabuleiro 'T)
        (substituir (nth 0 posicao-cavalo) (nth 1 posicao-cavalo) tabuleiro)
        (print-tabuleiro tabuleiro)
    )
)

;;operador-8
(defun operador-8 (tabuleiro)
"Fun��o que recebe um tabuleiro. Realiza um movimento do cavalo 2 casas a esquerda e 1 para baixo
Devolve o tabuleiro com a nova posi��o do cavalo."
    (let* ((posicao-cavalo (procurar-posicao tabuleiro 'T))
           (nova-pos-linha (+ (nth 0 posicao-cavalo) 1))
           (nova-pos-coluna (- (nth 1 posicao-cavalo) 2)))
        (cond 
            ((or (>= nova-pos-linha (length (car tabuleiro))) (>= nova-pos-coluna (length tabuleiro)) (< nova-pos-linha 0) (< nova-pos-coluna 0)) 
                (error "operador-8 n�o pode ser realizado, operador est� fora dos limites do tabuleiro."))
            ((equal (nth nova-pos-coluna (nth nova-pos-linha tabuleiro)) 'nil) (error "Esta casa j� foi visitada"))
        )
        (substituir nova-pos-linha nova-pos-coluna tabuleiro 'T)
        (substituir (nth 0 posicao-cavalo) (nth 1 posicao-cavalo) tabuleiro)
        (print-tabuleiro tabuleiro)
    )
)


; Testar nos operadores se a posicao-cavalo � nil ou nao
; Estudar error handling e se calhar trocar nos m�todos que retornem erro para retornar nil em vez de erro(?)
; Pensar que na intera��o com o jogador poderei ter que dar catch de erros e mostrar ao jogador esses erros, 
;se calhar terei que deixar alguns m�todos a retornar error para dar catch no m�todo de intera��o com o jogador