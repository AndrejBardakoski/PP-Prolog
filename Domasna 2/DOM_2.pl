%ZAD 1
%Data
lice(1,petko,petkovski,m,datum(1,3,1950),kratovo,skopje).
lice(2,marija,petkovska,z,datum(30,5,1954),kumanovo,skopje).
lice(3,ljubica,petkovska,z,datum(29,11,1965),skopje,skopje).
lice(4,vasil,vasilev,m,datum(8,4,1954),bitola,bitola).
lice(5,elena,vasileva,z,datum(19,6,1958),resen,bitola).
lice(6,krste,krstev,m,datum(9,8,1948),veles,veles).
lice(7,biljana,krsteva,z,datum(13,8,1949),veles,veles).
lice(8,igor,krstev,m,datum(26,10,1971),veles,skopje).
lice(9,kristina,krsteva,z,datum(30,5,1974),kumanovo,skopje).
lice(10,julija,petrova,z,datum(30,5,1978),skopje,skopje).
lice(11,bosko,petkovski,m,datum(13,11,1981),skopje,skopje).
lice(12,gjorgji,vasilev,m,datum(15,7,1978),bitola,bitola).
lice(13,katerina,petkovska,z,datum(11,12,1979),bitola,skopje).
lice(14,petar,vasilev,m,datum(21,2,1982),skopje,skopje).
lice(15,andrej,krstev,m,datum(3,8,1998),skopje,skopje).
lice(16,martina,petkovska,z,datum(5,12,2005),skopje,skopje).
familija(1,2,[9,10]).
familija(1,3,[11]).
familija(4,5,[12,13,14]).
familija(6,7,[8]).
familija(8,9,[15]).
familija(11,13,[16]).

%lice(Sifra,Ime,Prezime,Pol,Data_raganje,Mesto_raganje,Mesto_ziveenje),
%familija(Sifra_tatko,Sifra_majka,Lista_sifri_deca) 

%Task 1
%rodeni_razlicen_grad(Kolku). 
rodeni_razlicen_grad(Kolku):-
	findall(X,rodenRazlicenGrad(X),L),
    dolzina(L,Kolku).

%rodenRazlicenGrad(L) True ako liceto so sifra L e rodeno vo razlicen grad od gradot na roditelite
rodenRazlicenGrad(L):-
    lice(L,_,_,_,_,GLice,_),
    familija(T,M,D), clen(L,D),
    rodenGrad(T,GTatko), rodenGrad(M,GMajka),
	GLice\==GTatko, GLice\==GMajka.

%rodenGrad(L,G) G e gradot vo koj e rodeno liceto so sifra L
rodenGrad(L,G):-lice(L,_,_,_,_,G,_).

%clen(X,L) True ako X e element na L.
clen(X,[X|_]).
clen(X,[_|L]):-clen(X,L).

%dolzina(L,N) N e dolzina na listata L
dolzina([],0).
dolzina([_|L],N):-dolzina(L,M), N is M+1.

%Task 2
%predci(Sifra,L)
predci(Sifra,L):-findall(P,predok_uslov(Sifra,P),L).

%predok_uslov(L,P) P e predok na L, L i P se od ist pol i imaat blizok rodenden.
predok_uslov(L,P):-predok(L,P),
    lice(L,_,_,Pol,LData,_,_),
    lice(P,_,_,Pol,PData,_,_),
    blizokRodenden(LData,PData).

%predok(L,P) P e predok na L
predok(L,P):-roditel(L,P).
predok(L,P):-roditel(L,R),predok(R,P).

%roditel(L,R) True ako R e roditel na L
roditel(L,R):-familija(R,_,D),clen(L,D).
roditel(L,R):-familija(_,R,D),clen(L,D).

%blizokRodenden(Data1,Data2) Data1 i Data2 se razlikuvaat za edna nedela godinata e nebitna.
blizokRodenden(datum(D1,M,_),datum(D2,M,_)):- D1>=D2,Raz=D1-D2, Raz=<7.
blizokRodenden(datum(D1,M,_),datum(D2,M,_)):- D1<D2,Raz=D2-D1, Raz=<7.
blizokRodenden(datum(D1,M1,_),datum(D2,M2,_)):- MRaz is M1 - M2, MRaz=1,
    D1_new is D1+30, Raz is D1_new - D2, Raz=<7.
blizokRodenden(datum(D1,M1,_),datum(D2,M2,_)):- MRaz is M2 - M1, MRaz=1,
    D2_new is D2+30, Raz is D2_new - D1, Raz=<7.


%ZAD2
%Data
telefon(111111,petko,petkovski,[povik(222222,250),povik(101010,125)]).
telefon(222222,marija,petkovska,[povik(111111,350),povik(151515,113),povik(171717,122)]).
telefon(333333,ljubica,petkovska,[povik(555555,150),povik(101010,105)]).
telefon(444444,vasil,vasilev,[povik(171717,750)]).
telefon(555555,elena,vasileva,[povik(333333,250),povik(101010,225)]).
telefon(666666,krste,krstev,[povik(888888,75),povik(111111,65),povik(141414,50),povik(161616,111)]).
telefon(777777,biljana,krsteva,[povik(141414,235)]).
telefon(888888,igor,krstev,[povik(121212,160),povik(101010,225)]).
telefon(999999,kristina,krsteva,[povik(666666,110),povik(111111,112),povik(222222,55)]).
telefon(101010,julija,petrova,[]).
telefon(121212,bosko,petkovski,[povik(444444,235)]).
telefon(131313,gjorgji,vasilev,[povik(141414,125),povik(777777,165)]).
telefon(141414,katerina,petkovska,[povik(777777,315),povik(131313,112)]).
telefon(151515,petar,vasilev,[]).
telefon(161616,andrej,krstev,[povik(666666,350),povik(111111,175),povik(222222,65),povik(101010,215)]).
telefon(171717,martina,petkovska,[povik(222222,150)]).
sms(111111,[222222,999999,101010]).
sms(444444,[333333,121212,161616]).
sms(111111,[777777]).
sms(666666,[888888]).
sms(444444,[555555,121212,131313,141414]).
sms(666666,[777777,888888]).
sms(888888,[999999,151515]).
sms(171717,[131313,161616]).

%telefon(Broj,Ime,Prezime,Lista_na_pojdovni_povici)
%povik(Povikan_broj,Traenje)
%SMS(Broj_koj_ja_ispraka_porakata,Lista_broevi_koi_ja_dobivaat_porakata)

%Task 1
%najbroj(X,Y)  X ime, Y prezime na brojot koj ostvaril najmnogu povici
najbroj(X,Y):- maxRazgovori(Tel), getImePrez(Tel,X,Y).

%maxRazgovori(Tel) Tel e brojot koj napravil razgovori so najmnogu drugi broevi
maxRazgovori(Tel):-findall((T,N),brojPovici(T,N),[M|L]), max(L,M,Tel).

%max(L,(TempX,TempMax),MaxX) MaxX e najgolem element vo L kade L se sostoi od (X,N) elementi 
%kade N e svojstvo na X spored koe se bara najgolem, primer X e tel broj a N e broj na povici
%(TempX,TempMax) e momentalniot najgolem element
max([],(X,_),X):-!.
max([(X,N)|L],(_,TempMax),Y):- N>TempMax, max(L,(X,N),Y).
max([(_,N)|L],(TempX,TempMax),Y):- N=<TempMax, max(L,(TempX,TempMax),Y).

%brojPovici(T,N) N e broj na tel broevi so koj T ima napraveno razgovor
brojPovici(T,N):- setof(T2,razgovor(T,T2),L),dolzina(L,N).

%razgovor(T1,T2) True ako T1 i T2 imale telefonski razgovor
razgovor(T1,T2):-povikan(T1,T2).
razgovor(T1,T2):-povikan(T2,T1).

%povikan(Tel1,Tel2) True ako Tel2 go povikal Tel1
povikan(T1,T2):- telefon(T2,_,_,L),clen(povik(T1,_),L).

getImePrez(Tel,Ime,Prezime):-telefon(Tel,Ime,Prezime,_).


%Task 2
%omilen(X,Y)
omilen(X,Y):- maxVremetraenjeRazgovori(X,Y).

%maxVremetraenjeRazgovori(T1,T2) T2 e brojot so koj T1 ima ostvareno najgolemo vkupno traene na povici + sms
maxVremetraenjeRazgovori(X,Y):-findall((T,N),vremetraenjeRazgovori(X,T,N),[M|L]), max(L,M,Y),!.

%vremetraenjeRazgovori N e vkupno traene na povici + sms pomegu T1 i T2.
vremetraenjeRazgovori(T1,T2,N):- 
    vremetraenjePovici(T1,T2,N1), 
    brojPoraki(T1,T2,N2),
    N is N1 + 100*N2.

%vremetraenjePovici N e vkupno traene na povici pomegu T1 i T2
vremetraenjePovici(T1,T2,N):-
    vremetraenjeDojdovni(T1,T2,N1),
    vremetraenjeDojdovni(T2,T1,N2),
    N is N1+N2.

%vremetraenjePovici N e vkupno traene na dodjovni povici pomegu T1 i T2 (T2 go povikal T1)
vremetraenjeDojdovni(T1,T2,N):- telefon(T2,_,_,L),clen(povik(T1,N),L).
vremetraenjeDojdovni(T1,T2,0):- telefon(T2,_,_,L),not(clen(povik(T1,_),L)).

%brojPoraki(T1,T2,N) N e brojot na poraki pomegu T1 i T2
brojPoraki(T1,T2,N):-
    brojPrimeniSMS(T1,T2,N1),
    brojPrimeniSMS(T2,T1,N2),
    N is N1+N2.

%brojPrimeniSMS(T1,T2,N) N e brojot na poraki koj T1 gi primil od T2
brojPrimeniSMS(T1,T2,N):- bagof(_,primenaSMS(T1,T2),L), dolzina(L,N).
brojPrimeniSMS(T1,T2,0):- not(primenaSMS(T1,T2)).

%primenaSMS(T1,T2) True ako T2 mu ispratil poraka na T1
primenaSMS(T1,T2):- sms(T2,L), clen(T1,L).


%ZAD 3
%Data
klient(1,petko,petkov,[usluga(a,b,50,datum(12,12,2015),23),usluga(c,a,50,datum(7,12,2015),34),usluga(c,f,40,datum(7,11,2015),23)]).
klient(2,vasil,vasilev,[usluga(a,e,50,datum(25,12,2015),12),usluga(c,g,40,datum(17,11,2015),56),usluga(g,d,50,datum(17,12,2015),45),usluga(e,a,40,datum(24,12,2015),34)]).
klient(3,krste,krstev,[usluga(c,b,60,datum(31,12,2015),56),usluga(e,f,60,datum(31,12,2015),34)]).
klient(4,petar,petrov,[usluga(a,f,50,datum(25,12,2015),23),usluga(f,d,50,datum(25,12,2015),34)]).
klient(5,ivan,ivanov,[usluga(d,g,50,datum(7,12,2015),56),usluga(g,e,40,datum(25,12,2015),34)]).
klient(6,jovan,jovanov,[usluga(c,f,50,datum(5,12,2015),12),usluga(f,d,50,datum(27,12,2015),45)]).
klient(7,ana,aneva,[usluga(e,d,50,datum(11,12,2015),12),usluga(d,g,50,datum(11,12,2015),12)]).
klient(8,lidija,lideva,[usluga(e,g,50,datum(29,12,2015),45),usluga(f,b,50,datum(29,12,2015),34)]).
rastojanie(a,b,4).
rastojanie(a,c,7).
rastojanie(b,c,5).
rastojanie(b,d,3).
rastojanie(c,d,4).
rastojanie(b,e,6).
rastojanie(c,e,2).
rastojanie(b,f,8).
rastojanie(e,f,5).
rastojanie(f,g,3).


%Task1
%izbroj_lokacija(Lok,Br)
izbroj_lokacija(Lok,Br):-
    findall(_,(klient(_,_,_,U),clen(usluga(Lok,_,_,_,_),U)),L1),
    findall(_,(klient(_,_,_,U),clen(usluga(_,Lok,_,_,_),U)),L2),
    dolzina(L1,N1), dolzina(L2,N2), Br is N1+N2.


%Task2
%najmnogu_kilometri(X,Y).
najmnogu_kilometri(X,Y):-
    findall((S,N),pominatiKilometri(S,N),[M|L]),
    max(L,M,K),
    klient(K,X,Y,_),!.

%pominatiKilometri(X,N) N e brojot na pominati kilometri na liceto X
pominatiKilometri(X,N):-
    klient(X,_,_,L),
    pominatiKilometriR(L,N).

%pominatiKilometriR(L,N). L e lista od usligi a N e vkupnoto rastojanie pomegu pocetnite i destinaciskite mesta
pominatiKilometriR([],0).
pominatiKilometriR([usluga(A,B,_,_,_)|L],N):-
    pominatiKilometriR(L,Nr),
    najkratokPat(A,B,P), N is Nr + P.

%najkratokPat(A,B,N) N e najkratkoto rastojanie pomegu A i B.
najkratokPat(A,B,N):-setof(R,pat(A,B,R),[N|_]).

% D e rastojanieto pomegu A i B dokolku postoi direkten pat
patDirekten(A,B,D):-rastojanie(A,B,D).
patDirekten(A,B,D):-rastojanie(B,A,D).

%True ako postoi pat od A do B so dolzina N (bez ciklusi) 
pat(A,B,N):-najdiSiteMesta(L),
    izbrisi(L,A,L2),
    izbrisi(L2,B,L3),
    pat(L3,A,B,N).

pat(_,A,B,N):-patDirekten(A,B,N).
pat(L,A,B,N):-
    clen(C,L), izbrisi(L,C,L2),
    pat(L2,A,C,N1),patDirekten(C,B,N2),N is N1 + N2.

%najdiSiteMesta(L) gi vraka site mesta za koi ima rastojanie vo bazata na podatoci
najdiSiteMesta(L):-
    findall(A,rastojanie(A,_,_),La),
    findall(B,rastojanie(_,B,_),Lb),
    zalepi(La,Lb,Lab), izvadiDupli(Lab,L),!.

%append
zalepi([],L2,L2).
zalepi([X|L1],L2,[X|L3]):-zalepi(L1,L2,L3).

%izvadiDupli(L1,L2). L2 e L1 bez duplikati
izvadiDupli([],[]).
izvadiDupli([X|L1],L2):-clen(X,L1),izvadiDupli(L1,L2).
izvadiDupli([X|L1],[X|L2]):-not(clen(X,L1)),izvadiDupli(L1,L2).

%izbrisi(L1,X,L2). izbrisi gi site pojavuvanja na X vo L1 i rezultatot vrati go vo L2
izbrisi([],_,[]).
izbrisi([X|L1],X,L2):-izbrisi(L1,X,L2).
izbrisi([X|L1],Y,[X|L2]):- X\==Y, izbrisi(L1,Y,L2).


%Task3
%najmnogu_zarabotil(X) 
najmnogu_zarabotil(X):-
    findall((T,N),zarabotil(T,N),[M|L]),
    max(L,M,X),!.

%zarabotil(T,N)  taksistot so vozilo broj T zarabotil N vo dekemvri 2015
zarabotil(T,N):-
	siteUslugi(L),
	zarabotil(L,T,N).
zarabotil([],_,0).
zarabotil([usluga(A,B,C,datum(_,12,2015),T)|L],T,N):-
    zarabotil(L,T,N1),
    najkratokPat(A,B,P),
    N is N1 + P*C.
zarabotil([usluga(_,_,_,datum(_,M,God),Taxist)|L],T,N):-not((M\==12, God\==2015, Taxist\==T)),zarabotil(L,T,N).

%siteUslugi(L) L e lista od site uslugi vo bazata na podatoci
siteUslugi(L):-findall(U,klient(_,_,_,U),L2),izramniMatrica(L2,L).

%izramniMatrica(L1,L2) L2 e izramneta L1 kade L1 e matrica
izramniMatrica([],[]).
izramniMatrica([X|L],L2):-izramniMatrica(L,L3), zalepi(X,L3,L2).