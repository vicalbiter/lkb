%--------------------------------------------------
% Load and Save from files
%--------------------------------------------------


%KB open and save

open_kb(Route,KB):-
	open(Route,read,Stream),
	readclauses(Stream,X),
	close(Stream),
	atom_to_term(X,KB).

save_kb(Route,KB):-
	open(Route,write,Stream),
	writeq(Stream,KB),
	close(Stream).

readclauses(InStream,W) :-
        get0(InStream,Char),
        checkCharAndReadRest(Char,Chars,InStream),
	atom_chars(W,Chars).

checkCharAndReadRest(-1,[],_) :- !.  % End of Stream	
checkCharAndReadRest(end_of_file,[],_) :- !.

checkCharAndReadRest(Char,[Char|Chars],InStream) :-
        get0(InStream,NextChar),
        checkCharAndReadRest(NextChar,Chars,InStream).

%compile an atom string of characters as a prolog term
atom_to_term(ATOM, TERM) :-
	atom(ATOM),
	atom_to_chars(ATOM,STR),
	atom_to_chars('.',PTO),
	append(STR,PTO,STR_PTO),
	read_from_chars(STR_PTO,TERM).

%------------------------------
% Ejemplo:  
%------------------------------

%Cargar la base en una lista, imprimir la lista en consola y guardar todo en un nuevo archivo.
%No olvides poner las rutas correctas para localizar el archivo kb.txt en tu computadora!!!

ejemplo:-
	open_kb('kb.txt',KB),
	write('KB: '),
	write(KB),
	save_kb('new_kb.txt',KB).

%------------------------------
% Operators
%------------------------------
:- op(800,xfx,'=>').
:- op(801,xfx,'=>>').

%------------------------------
% List Utilities
%------------------------------

% Open and save KB
openk(KB):-
	open_kb('kb.txt', KB).
savek(KB):-
	save_kb('kb.txt', KB).

% member_of(X, L)
% Checks if X is a member of L
member_of(X, [X|_]).
member_of(X, [_|T]) :-
	member_of(X, T).

% get_class_of(X, KB, C)
% Gets the class of X inside the KB, and binds it to C.
get_class_of(_, [], unknown).
get_class_of(Id, [class(C,_,_,_,O)|_], C) :-
	member_of([id=>Id,_,_], O).
get_class_of(Id, [_|T], C) :-
	get_class_of(Id, T, C).

% substitute_element(X, Y, L, NL)
% Turns every element X in a list L into Y, and binds the new list to NL.
substitute_element(_, _, [], []).
substitute_element(X, Y, [X|T], [Y|Z]) :-
	substitute_element(X, Y, T, Z).
substitute_element(X, Y, [H|T], [H|Z]) :-
	substitute_element(X, Y, T, Z).

% delete_element(X, L, NL)
% Eliminates element X from a list L, and binds the new list to NL.
delete_element(_, [], []).
delete_element(X, [X|T], Y) :-
  delete_element(X, T, Y).
delete_element(X, [H|T], [H|Y]) :-
  delete_element(X, T, Y).

%------------------------------
% Create
%------------------------------

new_class(Name, Mother, OldKB, NewKB) :-
	append(OldKB, [class(Name, Mother, [], [], [])], NewKB).

new_object(ObjectId, Class, OldKB, NewKB) :-
	substitute_element(class(Class, Mother, Props, Rels, OldInst), class(Class, Mother, Props, Rels, NewInst), OldKB, NewKB),
	append(OldInst, [[id=>ObjectId, [], []]], NewInst).


new_class_property(Property, Weight, Class, OldKB, NewKB) :-
	substitute_element(class(Class, Mother, OldProps, Rels, Inst), class(Class, Mother, NewProps, Rels, Inst), OldKB, NewKB),
	append(OldProps, [[Property, Weight]], NewProps).


new_class_relation(Relation, Weight, Class, OldKB, NewKB) :-
	substitute_element(class(Class, Mother, Props, OldRels, Inst), class(Class, Mother, Props, NewRels, Inst), OldKB, NewKB),
	append(OldRels, [[Relation, Weight]], NewRels).


new_object_property(Property, Weight, ObjectId, OldKB, NewKB) :-
	get_class_of(ObjectId, OldKB, Class),
	substitute_element(class(Class, Mother, Props, Rels, OldInst), class(Class, Mother, Props, Rels, NewInst), OldKB, NewKB),
	substitute_element([id=>ObjectId, ObOldProps, ObRels], [id=>ObjectId, ObNewProps, ObRels], OldInst, NewInst),
	append(ObOldProps, [[Property, Weight]], ObNewProps).


new_object_relation(Relation, Weight, ObjectId, OldKB, NewKB) :-
	get_class_of(ObjectId, OldKB, Class),
	substitute_element(class(Class, Mother, Props, Rels, OldInst), class(Class, Mother, Props, Rels, NewInst), OldKB, NewKB),
	substitute_element([id=>ObjectId, ObProps, ObOldRels], [id=>ObjectId, ObProps, ObNewRels], OldInst, NewInst),
	append(ObOldRels, [[Relation, Weight]], ObNewRels).

%------------------------------
% Delete
%------------------------------

% Deletes class C from the list OldKB and binds the new list to NewKB
delete_class(C, OldKB, NewKB) :-
	delete_all_relations_with(C, OldKB, X),
	get_mother_class(C, OldKB, M),
	copy_objects_to_another_class(C, M, X, Y),
	change_children_mother_class(C, M, Y, Z),
	delete_element(class(C,_,_,_,_), Z, NewKB).

% get_mother_class(C, KB, M)
% Gets the mother class from the class C in KB, and binds it to M
get_mother_class(_, [], none).
get_mother_class(C, [class(C,M,_,_,_)|_], M).
get_mother_class(C, [_|T], M):-
	get_mother_class(C, T, M).

% Copies all the objects from class C in the list OldKB to a class X, and binds the new list to NewKB
copy_objects_to_another_class(C, X, OldKB, NewKB) :-
	get_object_list(C, OldKB, O),
	substitute_element(class(X, Mother, Props, Rels, OldInst), class(X, Mother, Props, Rels, NewInst), OldKB, NewKB),
	append(OldInst, O, NewInst).

% get_objects_list(C, KB, NL)
% Gets the object list from the class C in KB, and binds it to NL	
get_object_list(_, [], []).
get_object_list(C, [class(C,_,_,_,O)|_], O).
get_object_list(C, [_|T], O):-
	get_object_list(C, T, O).

%change_children_mother_class
% Changes the mother class of C's children to X
change_children_mother_class(C, X, OldKB, NewKB) :-
	get_class_children(C, OldKB, Children),
	change_classes_mother_class(Children, X, OldKB, NewKB).

%change_classes_mother_class(LC, X, OldKB, NewKB)
% Changes the mother class of a list of classes to X
change_classes_mother_class([],_,L,L).
change_classes_mother_class([C|T], X, OldKB, NewKB) :-
	change_class_mother_class(C, X, OldKB, NL),
	change_classes_mother_class(T, X, NL, NewKB).
		
% Changes the mother class of C to X
change_class_mother_class(C, X, OldKB, NewKB) :-
	substitute_element(class(C, _, Props, Rels, Inst), class(C, X, Props, Rels, Inst), OldKB, NewKB).

%-----------------------------------

%delete_object
delete_object(ObjectId, OldKB, NewKB) :-
	delete_all_relations_with(ObjectId, OldKB, X),
	get_class_of(ObjectId, X, C),
	substitute_element(class(C, Mother, Props, Rels, OldInst), class(C, Mother, Props, Rels, NewInst), X, NewKB),
	delete_element([id=>ObjectId,_,_],OldInst,NewInst).

%deletes_relations_with
delete_all_relations_with(_,[],[]).
delete_all_relations_with(Id, [class(C,M,P,OldR,OldI)|T], [class(C,M,P,NewR,NewI)|L]) :-
	delete_relations_with(Id, OldR, NewR),	
	delete_class_relations_with(Id, OldI, NewI),
	delete_all_relations_with(Id, T, L).

% deletes_list_relation_with
% Deletes relation with object on a class
delete_class_relations_with(_, [], []).
delete_class_relations_with(Id, [[id=>O,P,OldR]|T], [[id=>O,P,NewR]|L]) :-
	delete_relations_with(Id, OldR, NewR),
	delete_class_relations_with(Id, T, L).

% Deletes object from relations list
delete_relations_with(Id, OldR, NewR) :-
	delete_element([_=>Id,_], OldR, X),
	delete_element([not(_=>Id),_], X, NewR).

%------------------------------------

%get_properties_from_class
get_properties_from_class(_, [], []).
get_properties_from_class(C, [class(C,_,P,_,_)|_], P).
get_properties_from_class(C, [_|T], P) :-
	get_properties_from_class(C, T, P).

%get_properties_from_object
get_properties_from_object(_, [], []).
get_properties_from_object(O, [[id=>O,P,_]|_], P).
get_properties_from_object(O, [_|T], P) :-
	get_properties_from_object(O, T, P).

%get_relations_from_class
get_relations_from_class(_, [], []).
get_relations_from_class(C, [class(C,_,_,R,_)|_], R).
get_relations_from_class(C, [_|T], R) :-
	get_relations_from_class(C, T, R).

%get_relations_from_object(O, OL, R)
get_relations_from_object(_, [], []).
get_relations_from_object(O, [[id=>O,_,R]|_], R).
get_relations_from_object(O, [_|T], R) :-
	get_relations_from_object(O, T, R).

%------------------------------

%delete_property_of_class(P, C, OldKB, NewKB)
% Deletes a specific property of a class
delete_property_of_class(Property, C, OldKB, NewKB) :-
	get_properties_from_class(C, OldKB, OldP),
	delete_element([Property,_], OldP, NewP),
	substitute_element(class(C, M, _, R, O), class(C, M, NewP, R, O), OldKB, NewKB).

%delete_relation_of_class(R, C, OldKB, NewKB)
% Deletes a specific relation of a class
delete_relation_of_class(Relation, C, OldKB, NewKB) :-
	get_relations_from_class(C, OldKB, OldR),
	delete_element([Relation,_], OldR, NewR),
	substitute_element(class(C, M, P, _, O), class(C, M, P, NewR, O), OldKB, NewKB).

%delete_property_of_object(P, O, OldKB, NewKB)
% Deletes a specific property of an object
delete_property_of_object(Property, O, OldKB, NewKB) :-
	get_class_of(O, OldKB, C),		
	substitute_element(class(C, M, P, R, OldO), class(C, M, P, R, NewO), OldKB, NewKB),
	get_properties_from_object(O, OldO, OldP),
	delete_element([Property,_], OldP, NewP),
	substitute_element([id=>O,OldP,OR], [id=>O,NewP,OR], OldO, NewO).

%delete_relation_of_object(R, C, OldKB, NewKB)
delete_relation_of_object(Relation, O, OldKB, NewKB) :-
	get_class_of(O, OldKB, C),
	substitute_element(class(C, M, P, R, OldO), class(C, M, P, R, NewO), OldKB, NewKB),
	get_relations_from_object(O, OldO, OldR),
	delete_element([Relation,_], OldR, NewR),
	substitute_element([id=>O,OP,OldR], [id=>O,OP,NewR], OldO, NewO).

%------------------------------
% Extension
%------------------------------

% get_objects_ids(L, NL)
% get_objects_ids([[id=>X1,[],[]],[id=>X2,[],[]]..., NL)
% Gets all the object ids from a class' objects list

get_objects_ids([],[]).
get_objects_ids([[id=>X,_,_]|T], [X|L]) :-
	get_objects_ids(T, L).
get_objects_ids([_|T], L) :-
	get_objects_ids(T, L).

% get_objects_from_class(C, KB, L)
% Gets all the objects from a class C in the KB, and puts them on a list L
get_objects_from_class(_,[],[]).
get_objects_from_class(C, [class(C,_,_,_,O)|_], L) :-
	get_objects_ids(O, L).
get_objects_from_class(C, [_|T], L) :-
	get_objects_from_class(C, T, L).

% get_class_children
get_class_children(_, [], []).
get_class_children(C, [class(N,C,_,_,_)|T], L) :-
	get_class_children(C, T, L1),
	append([N],L1,L).
get_class_children(C, [_|T], L):-
	get_class_children(C, T, L).



	
	
	






















