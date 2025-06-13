:- module(ceritolog,
[
tablero/2, % tablero(+N,?Tablero)
% Devuelve un tablero de tamaño N vacío, o sea una matriz que representa un
% tablero vacío de juego como la descrita en la letra del laboratorio.

fin_del_juego/4, % fin_del_juego(+Tablero,?P1,?P2,?Ganador)
% Dado un tablero, el predicado es verdadero si el tablero representa un juego
% finalizado, y devuelve % la cantidad de puntos del jugador 1 en P1, la
% cantidad de puntos del jugador 2 en P2, y un string % que indica si alguno
% ganó, en el formato: “Gana el jugador 1”, “Gana el jugador 2”, o “Empate”.
% En caso de que no sea el fin del juego, el predicado falla.
 
jugada_humano/8, % jugada_humano(+Tablero,+Turno,+F,+C,+D,?Tablero2,?Turno2,?Celdas)
% Se le envía un tablero, de quién es el turno (1 o 2) y la línea elegida por el
% jugador humano con las variables F-C-D, y devuelve: el tablero modificado con
% la línea marcada (y celdas marcadas en caso de que sea necesario), de quién es
% el siguiente turno (Turno2), y una lista de celdas que se capturaron con esta
% acción en formato [Fila,Columna]. Por ejemplo: [[1,2],[1,3]]

jugada_maquina/9, % jugada_maquina(+Tablero,+Turno,+Nivel,?F,?C,?D,?Tablero2,?Turno2,?Celdas)
% Se le envía un tablero, de quién es el turno (1 o 2) y el Nivel de minimax,
% debe elegir una jugada a realizar por el jugador controlado por la computadora.
% El predicado devuelve: el tablero modificado luego de la jugada, de quién es
% el siguiente turno (Turno2), y una lista de celdas que se cerraron con esta
% acción en formato [Fila,Columna], de la misma forma que en el predicado anterior.

sugerencia_jugada/6 % sugerencia_jugada(+Tablero,+Turno,+Nivel,?F,?C,?D)
% Utiliza la estrategia de minimax para calcular una buena jugada para sugerirle
% a un jugador humano.
]).

% jugada_maquina(+Tablero, +Turno, +Nivel, ?F, ?C, ?D, ?Tablero2, ?Turno2, ?Celdas)
jugada_maquina(Tablero, Turno, Nivel, F, C, D, Tablero2, Turno2, Celdas) :-
    generar_jugadas_validas(Tablero, Jugadas),
    mejor_jugada_minimax(Jugadas, Tablero, Turno, Nivel, [F, C, D], Valor),
    writeln(mejor_jugada=[F, C, D]),
    writeln(valor_estimado=Valor),
    jugada_humano(Tablero, Turno, F, C, D, Tablero2, Turno2, Celdas),
    writeln(celdas_capturadas=Celdas),
    writeln(nuevo_turno=Turno2).

% mejor_jugada_minimax(+Jugadas, +Tablero, +Turno, +Nivel, -MejorJugada, -MejorValor)
mejor_jugada_minimax([Jugada], Tablero, Turno, Nivel, Jugada, Valor) :-
    Jugada = [F, C, D],
    jugada_humano(Tablero, Turno, F, C, D, Tab1, Turno1, _),
    writeln('--- jugada humano 2 ---'),
    writeln(jugada=Jugada),
    minimax(Tab1, Turno1, Nivel, Turno, Valor).

mejor_jugada_minimax([J1 | Resto], Tablero, Turno, Nivel, MejorJugada, MejorValor) :-
    J1 = [F, C, D],
    jugada_humano(Tablero, Turno, F, C, D, Tab1, Turno1, _),
    writeln('--- jugada humano ---'),
    writeln(jugada=J1),
    minimax(Tab1, Turno1, Nivel, Turno, Valor1),
    writeln("Sale minimax: "),
    writeln(Valor1),
    mejor_jugada_minimax(Resto, Tablero, Turno, Nivel, J2, Valor2),
    ( Valor1 >= Valor2 ->
        (MejorJugada = J1, MejorValor = Valor1)
    ;   (MejorJugada = J2, MejorValor = Valor2)
    ).


% minimax(+Tablero, +TurnoActual, +Nivel, +JugadorEvaluado, -Valor)
minimax(Tablero, _, 0, JugadorEvaluado, Valor) :-
    writeln("entro minimax 2 nivel:"),
    evaluar_tablero(Tablero, JugadorEvaluado, Valor),
    writeln("sale evaluar tablero"),
    writeln(Valor).

minimax(Tablero, TurnoActual, Nivel, JugadorEvaluado, Valor) :-
    Nivel > 0,
    writeln("entro minimax nivel:"),
    writeln(Nivel),
    generar_jugadas_validas(Tablero, Jugadas),
    writeln("Jugadas validas minimax: "),
    writeln(Jugadas),
    Nivel1 is Nivel - 1,
    findall(V, (
            member([F, C, D], Jugadas),
            jugada_humano(Tablero, TurnoActual, F, C, D, T1, T2, _),
            minimax(T1, T2, Nivel1, JugadorEvaluado, V)
        ),
        Valores),
    (
        TurnoActual =:= JugadorEvaluado -> max_list(Valores, Valor)
    								;	min_list(Valores, Valor)
    ).


% evaluar_tablero(+Tablero, +Jugador, -Valor)
evaluar_tablero(Tablero, Jugador, Valor) :-
    contar_celdas(Tablero, P1, P2),
    writeln("sale contar puntos: "),
    writeln(P1),
    writeln(P2),
    (Jugador =:= 1 -> Valor is P1 - P2 ; Valor is P2 - P1).

% evaluar_tablero(+Tablero, +Jugador, -Valor)
evaluar_tablero(Tablero, Jugador, Valor) :-
    contar_celdas(Tablero, Jugador, P1),
    OtroJugador is 3 - Jugador,
    contar_celdas(Tablero, OtroJugador, P2),
    Valor is P1 - P2.

% contar_celdas(+Tablero, +Jugador, -Cuenta)
contar_celdas(Tablero, Jugador, Cuenta) :-
    flatten(Tablero, Celdas),
    include(=(c(_, _, Jugador)), Celdas, Capturadas),
    length(Capturadas, Cuenta).

% generar_jugadas_validas(+Tablero, -Jugadas)
generar_jugadas_validas(Tablero, Jugadas) :-
    findall([F,C,h], (
        nth1(F, Tablero, Fila),
        nth1(C, Fila, c(H, _, J)),
        J =\= -1,
        H == f
    ), Horizontales),
    findall([F,C,v], (
        nth1(F, Tablero, Fila),
        nth1(C, Fila, c(_, V, J)),
        J =\= -1,
        V == f
    ), Verticales),
    append(Horizontales, Verticales, Jugadas).

% genera una fila con celdas capturables (J=0) y no capturables (J=-1) según la posición
fila_indexada(N, FilaIndex, Fila) :-
    findall(Celda, (
        between(1, N, ColIndex),
        (FilaIndex < N, ColIndex < N ->
            Celda = c(f, f, 0)
        ;
            Celda = c(f, f, -1)
        )
    ), Fila).

% genera el tablero completo con celdas capturables y no capturables
tablero(N, Tablero) :-
    findall(Fila, (
        between(1, N, FilaIndex),
        fila_indexada(N, FilaIndex, Fila)
    ), Tablero).

jugada_humano(Tablero, Turno, F, C, D, Tablero2, Turno2, CeldasCapturadas) :-
    marcar_linea(Tablero, F, C, D, TableroNuevo),
    detectar_capturas(TableroNuevo, F, C, D, Turno, Tablero2, CeldasCapturadas),
    actualizar_turno(Turno, CeldasCapturadas, Turno2).

marcar_linea(Tablero, F, C, h, TableroNuevo) :-
    get_celda(Tablero, F, C, c(f,V,J)),
    set_celda(Tablero, F, C, c(t,V,J), TableroNuevo),
    !.
marcar_linea(Tablero, F, C, v, TableroNuevo) :-
    get_celda(Tablero, F, C, c(H,f,J)),
    set_celda(Tablero, F, C, c(H,t,J), TableroNuevo),
    !.
marcar_linea(_,_,_,_,_) :- fail.

detectar_capturas(Tablero, F, C, D, Turno, TableroFinal, Capturadas) :-
    posibles_celdas_afectadas(F, C, D, Celdas),
    checkear_celdas(Tablero, Celdas, Turno, TableroFinal, Capturadas).


% Devuelve una lista con las celdas afectadas por una línea (puede ser una o dos).
posibles_celdas_afectadas(F, C, h, Pos) :-
    (F > 0 -> F1 is F - 1, Pos = [[F,C],[F1,C]] ; Pos = [[F,C]]).

posibles_celdas_afectadas(F, C, v, Pos) :-
    (C > 0 -> C1 is C - 1, Pos = [[F,C],[F,C1]] ; Pos = [[F,C]]).

checkear_celdas(Tablero, [], _, Tablero, []).

checkear_celdas(Tablero, [[F,C]|Resto], Turno, TabFinal, [[F,C]|CapturadasResto]) :-
    get_celda(Tablero, F, C, c(H,V,J)), J = 0,
    tiene_los_4_lados(Tablero, F, C),
    set_celda(Tablero, F, C, c(H, V, Turno), TabAux),
    checkear_celdas(TabAux, Resto, Turno, TabFinal, CapturadasResto).

checkear_celdas(Tablero, [[_,_]|Resto], Turno, TabFinal, Capturadas) :-
    checkear_celdas(Tablero, Resto, Turno, TabFinal, Capturadas).  % no capturada


tiene_los_4_lados(Tab, F, C) :-
    get_celda(Tab, F, C, c(H1,V1,_)), H1 == t, V1 == t,
    F1 is F+1, C1 is C+1,
    get_celda(Tab, F1, C, c(H2,_,_)), H2 == t,
    get_celda(Tab, F, C1, c(_,V2,_)), V2 == t.


actualizar_turno(Turno, [], Turno2) :-
    Turno2 is 3 - Turno.


actualizar_turno(Turno, [_|_], Turno).  % Si hubo capturas, no cambia


get_celda(Tablero, F, C, Celda) :-
    FI is F - 1,
    CI is C - 1,
    nth0(FI, Tablero, Fila),
    nth0(CI, Fila, Celda).


set_celda(Tablero, F, C, NuevaCelda, NuevoTablero) :-
    FI is F - 1,
    CI is C - 1,
    nth0(FI, Tablero, FilaVieja),
    replace_nth0(CI, FilaVieja, NuevaCelda, FilaNueva),
    replace_nth0(FI, Tablero, FilaNueva, NuevoTablero).


replace_nth0(I, Lista, Elem, Resultado) :-
    same_length(Lista, Resultado),
    append(Prefix, [_|Suffix], Lista),
    length(Prefix, I),
    append(Prefix, [Elem|Suffix], Resultado).

% contar_puntos(+Tablero, -P1, -P2)
contar_puntos(Tablero, P1, P2) :-
    flatten(Tablero, Celdas),
    include(es_capturable, Celdas, Capturables),
    contar_capturas(Capturables, P1, P2).


% es_capturable(+Celda)
es_capturable(c(_, _, J)) :- J \= -1.  % -1 para evitar confusión con bordes (si se usara)


% contar_capturas(+Celdas, -P1, -P2)
contar_capturas([], 0, 0).

contar_capturas([c(_, _, 1)|T], P1, P2) :-
    contar_capturas(T, P1T, P2),
    P1 is P1T + 1.

contar_capturas([c(_, _, 2)|T], P1, P2) :-
    contar_capturas(T, P1, P2T),
    P2 is P2T + 1.

contar_capturas([c(_, _, 0)|_], _, _) :-
    fail.  % Si hay alguna celda sin capturar, el predicado falla


% decidir_ganador(+P1, +P2, -Ganador)
decidir_ganador(P1, P2, "Gana el jugador 1") :- P1 > P2.
decidir_ganador(P1, P2, "Gana el jugador 2") :- P2 > P1.
decidir_ganador(P1, P2, "Empate") :- P1 =:= P2.


% fin_del_juego(+Tablero, ?P1, ?P2, ?Ganador)
fin_del_juego(Tablero, P1, P2, Ganador) :-
    contar_puntos(Tablero, P1, P2),
    decidir_ganador(P1, P2, Ganador).

sugerencia_jugada(_,_,_,_,_,_):-fail.
