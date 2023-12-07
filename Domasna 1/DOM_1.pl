%ZAD - 1 neparen palindrom
%neparen_palindrom(L) true ako L ima neparen broj na elementi i e palindrom
neparen_palindrom([_]).
neparen_palindrom([Glava|Ostatok]):-
    izvadi_posleden(Ostatok,Posleden,ListOut),
    Glava = Posleden,
    neparen_palindrom(ListOut),!.

%izvadi_posleden(L,Posleden,Rest) Poosleden e posledniot element vo listata L a Rest e ostatokot od L
izvadi_posleden([X],X,[]).
izvadi_posleden([Glava|Ostatok],Posleden,ListOut):-
    izvadi_posleden(Ostatok,Posleden,ListOutRec),
    ListOut=[Glava|ListOutRec].


%ZAD - 2
%nаj_podniza(L,N,X).  X e podlista so dolzina N koja sto se pojavuva najmnogu pati vo L
nаj_podniza(InputL,Dolzina,Maks):-
	findall(L1,(podniza(InputL,L1),dolzina(L1,Dolzina)),L),
    naj_pojavuvana(L,Maks),!.

%podniza(L1,L2). L2 e podnizna na L1 
podniza([X|L1],[X|L2]):-podnizaPocetok(L1,L2).
podniza([_|L1],L2):-podniza(L1,L2).
%podnizaPocetok(L1,L2). L2 e podniza koja sto se naoga na pocetok na L1, 
podnizaPocetok(_,[]).
podnizaPocetok([X|L1],[X|L2]):-podnizaPocetok(L1,L2).

%naj_pojavuvana(L,Maks) Maks e eleemntot koj sto najmnogu se pojavuva vo L
naj_pojavuvana([X|L],Maks):-izbroiPojavuvana(X,[X|L],N),naj_pojavuvana(L,[X|L],X,N,Maks).
%naj_pojavuvana(L,L,tempX,tempN,X).  X e element koj sto se pojavuva najmnogu pati vo L.
% tempX i tempN se pomosnini promenlvi koj go cuvaat tekovno najdeniot element so najmnogu pojavuvana 
naj_pojavuvana([],_,MaksX,_,MaksX).
naj_pojavuvana([X|O],L,_,MaksN,Out):-
    izbroiPojavuvana(X,L,N),N>MaksN,
    naj_pojavuvana(O,L,X,N,Out).
naj_pojavuvana([X|O],L,MaksX,MaksN,Out):-
    izbroiPojavuvana(X,L,N),N=<MaksN, 
    naj_pojavuvana(O,L,MaksX,MaksN,Out).

%izbroiPojavuvana(X,L,N).  N e broj na pojavuvana na X vo listata L.
izbroiPojavuvana(_,[],0).
izbroiPojavuvana(X,[X|L],N):-izbroiPojavuvana(X,L,M), N is M+1.
izbroiPojavuvana(X,[_|L],N):-izbroiPojavuvana(X,L,N).

%dolzina(L,N) N e brojot na elementi vo L
dolzina([],0).
dolzina([_|L],N):-dolzina(L,M), N is M+1.


%ZAD 3
proveri([A,B]):-B>A.
proveri([A,B,C|Ostatok]):-proveri_uslov3([A,B,C|Ostatok]).
    
proveri_uslov3([_]).
proveri_uslov3([A,B]):-B>A.
proveri_uslov3([A,B,C|Ostatok]):-B>A, C<B, proveri_uslov3([C|Ostatok]).


%ZAD 4
%permutacii(L1,L2). L2 e lista od site permutacii na L1
permutacii(L1,L2):-setof(P,permutacija(P,L1),L2).

%permutacija(L1,L2) True ako L1 e permutacija na L2
permutacija([],[]).
permutacija([X|L1],L2):-izvadiElem(L2,X,L3), permutacija(L1,L3).

%izvadiElem(L1,X,L2). od listata L1 isvadi go elementot X a ostatokot zapisi go vo L2
izvadiElem([X|L],X,L).
izvadiElem([X|L1],Y,[X|L2]):-izvadiElem(L1,Y,L2).


%ZAD 5
%convert(L,X). X e decimalniot zapis na brojot L koj e vo binaren zapis vo format na lista
%convert([X],X).
convert(L,Out) :- convert(L,0,Out).
convert([],Tmp,Tmp).
convert([X|Rest],Tmp,Out) :-
    NewTmp is (Tmp * 2) + X,
    convert(Rest,NewTmp,Out).

%convertR(X,L). L e binaren zapis vo format na lista na decimalniot broj X
convertR(X,L):-convertR(X,[],L).
convertR(0,L,L):-!.
convertR(X,L,Out):-
    Ostatok is X mod 2,
    Kolicnik is X // 2,
    convertR(Kolicnik,[Ostatok|L],Out).

%sobiranje(L1,L2,R):- R e zbir od L1 i L2 kade i L1 i L2 i R se vo binaren zapis
sobiranje(L1,L2,R):-
    convert(L1,X1),
    convert(L2,X2),
    R_dec is X1 + X2,
    convertR(R_dec,R).

%odzemanje(L1,L2,R):- R e razlika od L1 i L2 kade i L1 i L2 i R se vo binaren zapis
odzemanje(L1,L2,R):-
    convert(L1,X1),
    convert(L2,X2),
    R_dec is X1 - X2, 
    (R_dec>0, convertR(R_dec,R),!);
	R=[0].

%mnozenje(L1,L2,R):- R e proizvod od L1 i L2 kade i L1 i L2 i R se vo binaren zapis
mnozenje(L1,L2,R):-
    convert(L1,X1),
    convert(L2,X2),
    R_dec is X1 * X2,
    convertR(R_dec,R).

%delenje(L1,L2,R):- R e celobroen kolicnik od L1 i L2 kade i L1 i L2 i R se vo binaren zapis
delenje(L1,L2,R):-
    convert(L1,X1),
    convert(L2,X2),
    R_dec is X1 // X2,
    convertR(R_dec,R).


%ZAD 6
%presmetaj(M,R) vlez e matrica M a izlez e R dobieno kako M*M_transponirana
%R_ij=X_i1*X_j1 + X_i2*X_j2 + X_i3*X_j3 + ...     
%kade R_ij e pozicija i,j vo izleznata matrica R, X_ij e pozicija i,j vo vleznata matrica X
presmetaj(M,R):-presmetaj(M,M,1,R),!.

%presmetaj(M,M,I,R). M e vleznata matrica, R e izleznata I e iterator po redici. 
presmetaj(_,[],_,[]).
presmetaj(M,[X|L],I,[Y|L2]):-
    presmetajRed(M,X,I,1,Y),
    I_new is I+1,
    presmetaj(M,L,I_new,L2).

%presmetajRed(М,L,I,J,R). M-vlezna matrica, L e I-tiot red vo matricata, a R e I-tiot red od rezultatnata matrica
% J e iterator po koloni
presmetajRed(_,[],_,_,[]).
presmetajRed(M,[_|L1],I,J,[Y|L2]):-
	presmetajKelija(M,M,I,J,1,Y),
    J_new is J+1,
    presmetajRed(M,L1,I,J_new,L2).

%presmetajKelija(M,M,I,J,Iterator,R). M e vleznata matrica,R kelija na pozicija (I,J) vo rezultantnata matrica 
presmetajKelija(_,[],_,_,_,0).
presmetajKelija(M,[_|L],I,J,Iterator,R):-
    clen_pozicija_matrica(M,I,Iterator,X1),    
    clen_pozicija_matrica(M,J,Iterator,X2),
    P is X1 * X2,
    Iterator_new is Iterator + 1,
    presmetajKelija(M,L,I,J,Iterator_new,R2),
    R is R2 + P.

%clen_pozicija_matrica(M,I,J,X) X e elementot na pozicija I,J vo matricata X
clen_pozicija_matrica([X|_],1,J,Y):-clen_pozicija(X,J,Y).
clen_pozicija_matrica([_|L],I,J,X):-I_new is I-1,  clen_pozicija_matrica(L,I_new,J,X).

%clen_pozicija(L,N,X). X e elementot na N-ta pozicija vo L.
clen_pozicija([X|_],1,X).
clen_pozicija([_|L],I,X):-I_new is I-1, clen_pozicija(L,I_new,X).


%ZAD 7
transform(L1,L2):-izbrisiDuplikati(L1,L3),sortiraj_podlisti(L3,L2),!. 

%sortiraj_podlisti(L1,L2), L2 e podredena L1 kade L1 e sostavena od podlisti
sortiraj_podlisti([],[]):-!.
sortiraj_podlisti(L1,[Max|L2]):-
    izvadiNajgolem(L1,Max,Rest),
    sortiraj_podlisti(Rest,L2),!.

%izvadiNajgolem(L,Max,Rest) Max e najgolemiot element vo L, a Rest e L bez Max
izvadiNajgolem(L,Max,Rest):- max(L,Max),izvadiElem(L,Max,Rest).

%max(L,X). X e najgolemiot element vo L
max([X],X).
max([X|L1],Maks):-
    max(L1,MaksR), 
    pogolem(X,MaksR), Maks = X.
max([X|L1],Maks):-
    max(L1,MaksR),
    not(pogolem(X,MaksR)), Maks = MaksR.

%pogolem(L1,L2) True ako L1 ima poveke elementi od L2, 
%vo slucaj da imaat isti elementi se proveruva dali prviot elem od L1 e pogolem od prviot od L2
%ako i prvite elementi se ednakvi se proveruvaat vtorite i tn.
pogolem(L1,L2):-
	dolzina(L1,N1),
	dolzina(L2,N2),
	N1>N2.
pogolem(L1,L2):-
	dolzina(L1,N),
	dolzina(L2,N),
	pogolemiElementi(L1,L2).

%pogolemiElementi(L1,L2) True ako prviot elem od L1 e pogolem od prviot od L2 
%ako prvite elementi se ednakvi se proveruvaat vtorite i tn.
pogolemiElementi([X|_],[Y|_]):- X>Y.
pogolemiElementi([X|L1],[X|L2]):- pogolemiElementi(L1,L2).

%izbrisiDuplikati(L1,L2) L2 e lista od unikatnite elementi od L1.
izbrisiDuplikati([],[]).
izbrisiDuplikati([X|L1],L2):-clen(X,L1), izbrisiDuplikati(L1,L2).
izbrisiDuplikati([X|L1],[X|L2]):-not(clen(X,L1)), izbrisiDuplikati(L1,L2).

%clen(X,L) True ako X e element na L.
clen(X,[X|_]).
clen(X,[_|L]):-clen(X,L).


%Zad 8
%brisi_sekoe_vtoro(L,R) 
brisi_sekoe_vtoro(L,R):-najdi_unikati(L,Unikati),brisi_sekoe_vtoro(L,Unikati,R).

brisi_sekoe_vtoro(L,[],L).
brisi_sekoe_vtoro(L,[X|L2],R):-
    brisi_sekoe_vtoro_X(L,X,0,R2,_),
    brisi_sekoe_vtoro(R2,L2,R).

%brisi_sekoe_vtoro_X(L,X,Flag,R,FlagOut) R e dobieno taka sto od L e izbrisano sekoe vtoro pojavuvane na X.
%L moze da sodrzi i podlisti, Flag 0- preskokni sledno X, Flag 1 brisi sledno X. 
%FlagOut go ima istoto znacene so Flag samo sto e izlezen parametar, se koristi vo vgnezdenite rekurzii.
brisi_sekoe_vtoro_X([],_,Flag,[],Flag).
brisi_sekoe_vtoro_X([X|L],X,0,[X|R],FlagOut):-brisi_sekoe_vtoro_X(L,X,1,R,FlagOut).
brisi_sekoe_vtoro_X([X|L],X,1,R,FlagOut):-brisi_sekoe_vtoro_X(L,X,0,R,FlagOut).
brisi_sekoe_vtoro_X([G|L],X,Flag,[G|R],FlagOut):-not(lista(G)), G\==X, brisi_sekoe_vtoro_X(L,X,Flag,R,FlagOut).
brisi_sekoe_vtoro_X([G|L],X,Flag,[R2|R],FlagOut):-lista(G),
    brisi_sekoe_vtoro_X(G,X,Flag,R2,FlagOutR),
    brisi_sekoe_vtoro_X(L,X,FlagOutR,R,FlagOut).

%lista(X) True ako X e lista
lista([]).
lista([_|_]).

%najdi_unikati(L,L2) L2 e lista koja gi sodrzi samo unikatnite edinecni elementi od L kade L ima podlisti
najdi_unikati(L,R):-najdi_unikati(L,[],R).
najdi_unikati([],Acc,Acc).
najdi_unikati([X|L1],Acc,R):-not(lista(X)), not(clen(X,Acc)), najdi_unikati(L1,[X|Acc],R).
najdi_unikati([X|L1],Acc,R):-not(lista(X)), clen(X,Acc), najdi_unikati(L1,Acc,R).
najdi_unikati([X|L1],Acc,R):-lista(X),
    najdi_unikati(X,Acc,R2),
    najdi_unikati(L1,R2,R).