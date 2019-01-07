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

% X is in the list L
is_in_list(_,[],unknown).
is_in_list(X,[not(X)|_],no).
is_in_list(X,[X|_],yes).
is_in_list(X,[_|T],A):-
        is_in_list(X,T,A).

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

% Delete repetitions in a list
delete_repetitions([],[]).
delete_repetitions([H|T],L):-
        delete_element(H,T,L1),
        delete_repetitions(L1,L2),
        append([H],L2,L).

% list_difference(L1,L2,L).
% L is the result of deleting all elements of L2 from L1
list_difference(L1,[],L1).
list_difference(L1,[H|T],L):-
    delete_element(H,L1,L2),
    list_difference(L2,T,L).

% pick_first_for_each(List,Choices)
% Picks first element of each element of List
pick_first_for_each([],[]).
pick_first_for_each([[H|_]|S], [H|Rest]):-
        pick_first_for_each(S,Rest).

% Intersection of two lists
intersect([],_,[]).
intersect(_,[],[]).
intersect([X|T],List2,[X|Rest]):-
        is_in_list(X,List2,yes),
        intersect(T,List2,Rest).

% Turn a list of lists of the form [[[a],[b]],[[c],[d]]] into [[a],[b],[c],[d]]
flatten_list([], []).
flatten_list([X|T], NL) :-
	flatten_list(T, L1),
	append(X, L1, NL).

%----------------------------------
% General routines for working with objects  
%----------------------------------

% get_class_objects(Class, KB, Objs)
% Gets the object list of Class in KB, no inheritance
get_class_objects(_, [], []).
get_class_objects(Class, [class(Class,_,_,_,_,_,Objs)|_], Objs).
get_class_objects(Class, [_|T], Objs):-
    get_class_objects(Class, T, Objs).

% get_classes_objects(Classes,KB,Objs)
% Gets the list of objects of a list of classes 
get_classes_objects([],_,[]).
get_classes_objects(_,[],[]).
get_classes_objects([H|T],B,Objs):-
     get_class_objects(H,B,HObjs),
     get_classes_objects(T,B,OthersObjs),
     append(HObjs,OthersObjs,Objs).

% get_class_of(X, KB, C)
% Gets the class of X inside the KB, and binds it to C.
get_class_of(_, [], unknown).
get_class_of(Id, [class(C,_,_,_,_,_,O)|_], C) :-
	is_member_of_object_list(Id, O).
get_class_of(Id, [_|T], C) :-
	get_class_of(Id, T, C).

% Tells if an id exist in an object list
is_member_of_object_list(Id, [[Ids,_,_,_,_]|_]) :-
	member_of(Id, Ids).
is_member_of_object_list(Id, [_|T]) :-
	is_member_of_object_list(Id, T).

% Tells if an id exist in an object list and binds the list to which it belongs to Ids
is_member_of_object_list(Id, [[Ids,_,_,_,_]|_], Ids) :-
	member_of(Id, Ids).
is_member_of_object_list(Id, [_|T], Ids) :-
	is_member_of_object_list(Id, T, Ids).

find_object_by_id(_,[],unknown).   
find_object_by_id(Id,[class(_,_,_,_,_,_,Objs)|_],Obj):-
        is_in_list(Obj,Objs,yes),
        is_member_of_object_list(Id,[Obj]).
find_object_by_id(Id,[_|T],Obj):-
        find_object_by_id(Id,T,Obj).

%----------------------------------
% Relatives of a class 
%----------------------------------

% get_class_parent(Class,KB,Parent).
% Gets the parent class of Class in KB
get_parent_class(_,[],unknown).
get_parent_class(Class,[class(Class,Parent,_,_,_,_,_)|_],Parent).
get_parent_class(Class,[_|T],Parent):- 
        get_parent_class(Class,T,Parent).

% get_class_ascendants(Class,KB,Ascendants)
% gets full list of ascendant (antecesor) classes of a class
get_class_ascendants(_,[],[]).
get_class_ascendants(Class,[class(Class,none,_,_,_,_,_)|_],[]).
get_class_ascendants(Class,B,Ascendants):-
      get_parent_class(Class,B,Parent),
      get_class_ascendants(Parent,B,ParentAscendants),
      append([Parent],ParentAscendants,Ascendants).
      
% get_classes_ascendants(Classes,KB,Ascendants)
% gets all ascendant classes of a list of classes
get_classes_ascendants([],_,[]).
get_classes_ascendants([H|T],B,Ascendants):-
        get_class_ascendants(H,B,HAscendants),
        get_classes_ascendants(T,B,OthersAscendants),
        append(HAscendants,OthersAscendants,Ascendants).

% get_class_children(Class,KB,Children)
% Gets the list of children of Class in KB
get_class_children(_,[],[]).
get_class_children(Class,[class(Child,Class,_,_,_,_,_)|T],Children):-
        get_class_children(Class,T,Siblings),
        append([Child],Siblings,Children).
get_class_children(Class,[_|T],Children):-
        get_class_children(Class,T,Children).

% get_classes_children(Classes,KB,Children)
% Gets all the child classes of a list of classes
get_classes_children([],_,[]).
get_classes_children([H|T],B,Children):-
        get_class_children(H,B,HChildren),
        get_classes_children(T,B,OthersChildren),
        append(HChildren,OthersChildren,Children).

% get_classes_descendants(Classes,KB,Descendants)
% Gets all descendant classes of a list of classes, including themselves
get_classes_descendants([],_,[]).
get_classes_descendants(Classes,B,Descendants):-
        get_classes_children(Classes,B,FirstLevel),
        get_classes_descendants(FirstLevel,B,NextLevels),
        append(Classes,NextLevels,Descendants).

% get_class_descendants(Class,KB,Descendants).
% Full list of descendant classes of a class
get_class_descendants(_,[],[]).
get_class_descendants(Class,B,Descendants):-
        get_class_children(Class,B,Children),
        get_classes_descendants(Children,B,Descendants).

%------------------------------------
% Properties, Relations and Preferences of Classes and Objects
%------------------------------------

%get_properties_from_class
get_properties_from_class(_, [], []).
get_properties_from_class(C, [class(C,_,P,_,_,_,_)|_], P).
get_properties_from_class(C, [_|T], P) :-
	get_properties_from_class(C, T, P).

%get_properties_preferences_from_class
get_properties_preferences_from_class(_, [], []).
get_properties_preferences_from_class(C, [class(C,_,_,PP,_,_,_)|_], PP).
get_properties_preferences_from_class(C, [_|T], PP) :-
	get_properties_preferences_from_class(C, T, PP).

%get_properties_from_object
get_properties_from_object(_, [], []).
get_properties_from_object(O, [[Ids,P,_,_,_]|_], P) :-
	member_of(O, Ids).
get_properties_from_object(O, [_|T], P) :-
	get_properties_from_object(O, T, P).

%get_properties_preferences_from_object
get_properties_preferences_from_object(_, [], []).
get_properties_preferences_from_object(O, [[Ids,_,PP,_,_]|_], PP) :-
	member_of(O, Ids).
get_properties_preferences_from_object(O, [_|T], PP) :-
	get_properties_preferences_from_object(O, T, PP).

%get_relations_from_class
get_relations_from_class(_, [], []).
get_relations_from_class(C, [class(C,_,_,_,R,_,_)|_], R).
get_relations_from_class(C, [_|T], R) :-
	get_relations_from_class(C, T, R).

%get_relations_preferences_from_class
get_relations_preferences_from_class(_, [], []).
get_relations_preferences_from_class(C, [class(C,_,_,_,_,PR,_)|_], PR).
get_relations_preferences_from_class(C, [_|T], PR) :-
	get_relations_preferences_from_class(C, T, PR).

%get_relations_from_object(O, OL, R)
get_relations_from_object(_, [], []).
get_relations_from_object(O, [[Ids,_,_,R,_]|_], R):-
	member_of(O, Ids).
get_relations_from_object(O, [_|T], R) :-
	get_relations_from_object(O, T, R).

%get_relations_preferences_from_object(O, OL, R)
get_relations_preferences_from_object(_, [], []).
get_relations_preferences_from_object(O, [[Ids,_,_,_,PR]|_], PR):-
	member_of(O, Ids).
get_relations_preferences_from_object(O, [_|T], PR) :-
	get_relations_preferences_from_object(O, T, PR).

%---------------------------------------------------
% Order pairs [property,weight] by weight in an
% ascending manner
%---------------------------------------------------
order_by_weights([],[]).
order_by_weights([[X,N],[Y,M]],[[X,N],[Y,M]]):-
        (N < M; N = M).
order_by_weights([[X,N],[Y,M]],[[Y,M],[X,N]]):-
        M < N.
order_by_weights([[X,N]|T],Ordered):-
        order_by_weights(T,S),
        insert_pair([X,N],S,Ordered).

insert_pair([X,N],[],[[X,N]]).
insert_pair([X,N],[[H,M]|T],Ordered):-
        (N < M; N = M),
        append([[X,N]],[[H,M]|T],Ordered).
insert_pair([X,N],[[H,M]|T],[[H,M]|TOrdered]):-
        insert_pair([X,N],T,TOrdered).

%****************************************************************
%---------------------------------------------------------------*
%-------------------------Creating------------------------------*
%---------------------------------------------------------------*
%****************************************************************

%-----------------------------------
% MAIN Creating routines
%-----------------------------------

% Adds a new class to the list OldKB of name Name, with a parent class Mother, and binds the new list to NewKB
new_class(Name, Mother, OldKB, NewKB) :-
	append(OldKB, [class(Name, Mother, [], [], [], [], [])], NewKB).

% Adds a new Property to a Class in the list OldKB, and stores the modified list in NewKB
% The argument "Property" should be of the form "property", or the form "att=>value" 
new_class_property(Property, Class, OldKB, NewKB) :-
	substitute_element(class(Class, Mother, OldProps, PrefP, Rels, PrefR, Inst), class(Class, Mother, NewProps, PrefP, Rels, PrefR, Inst), OldKB, NewKB),
	append(OldProps, [Property], NewProps).

% Adds a new Property Preference to a Class in the list OldKB, and stores the modified list in NewKB
% The argument "Property" should be of the form [ant_1,ant_2...ant_n]=>>[consequence,weight], where ant_k is a property of the form "property" or "att=>value"
new_class_property_preference(Property, Class, OldKB, NewKB) :-
	substitute_element(class(Class, Mother, Props, OldPrefP, Rels, PrefR, Inst), class(Class, Mother, Props, NewPrefP, Rels, PrefR, Inst), OldKB, NewKB),
	append(OldPrefP, [Property], NewPrefP).

% Adds a new Relation to a Class in the list OldKB, and stores the modified list in NewKB.
% The argument "Relation" should be of the form "att=>value"
new_class_relation(Relation, Class, OldKB, NewKB) :-
	substitute_element(class(Class, Mother, Props, PrefP, OldRels, PrefR, Inst), class(Class, Mother, Props, PrefP, NewRels, PrefR, Inst), OldKB, NewKB),
	append(OldRels, [Relation], NewRels).

% Adds a new Relation Preference to a Class in the list OldKB, and stores the modified list in NewKB
% The argument "Relation" should be of the form "[ant_1, ant_2,...,ant_n]=>>[consequence,weight]", where ant_k is a relation of the form "att=>value"
new_class_relation_preference(Relation, Class, OldKB, NewKB) :-
	substitute_element(class(Class, Mother, Props, PrefP, Rels, OldPrefR, Inst), class(Class, Mother, Props, PrefP, Rels, NewPrefR, Inst), OldKB, NewKB),
	append(OldPrefR, [Relation], NewPrefR).

%-------------------------------

% Adds a new object of Object id of the form [name_1, name_2...] to a Class in OldKB, and binds the new list to NewKB
% The input argument ObjectId should be a list, no matter if it only has one id
new_object(ObjectId, Class, OldKB, NewKB) :-
	substitute_element(class(Class, Mother, Props, PrefP, Rels, PrefR, OldInst), class(Class, Mother, Props, PrefP, Rels, PrefR, NewInst), OldKB, NewKB),
	append(OldInst, [[ObjectId, [], [], [], []]], NewInst).

% Adds a new object Property to the object corresponding an ObjectId
% Property should be of the form "property", or "att=>value". ObjectId can be any of the ids (aliases) of a certain Object.
new_object_property(Property, ObjectId, OldKB, NewKB) :-
	get_class_of(ObjectId, OldKB, Class),
	get_class_objects(Class, OldKB, OldInst),
	is_member_of_object_list(ObjectId, OldInst, ObIds),
	substitute_element(class(Class, Mother, Props, PrefP, Rels, PrefR, OldInst), class(Class, Mother, Props, PrefP, Rels, PrefR, NewInst), OldKB, NewKB),
	substitute_element([ObIds, ObOldProps, ObPrefP, ObRels, ObPrefR], [ObIds, ObNewProps, ObPrefP, ObRels, ObPrefR], OldInst, NewInst),
	append(ObOldProps, [Property], ObNewProps).

% Adds a new object Property to the object corresponding to an ObjectId
% Property should be of the form "[ant_1, ant_2,...,ant_n]=>>[consequence,weight]", where ant_k is a relation of the form "att=>value"
new_object_property_preference(Property, ObjectId, OldKB, NewKB) :-
	get_class_of(ObjectId, OldKB, Class),
	get_class_objects(Class, OldKB, OldInst),
	is_member_of_object_list(ObjectId, OldInst, ObIds),
	substitute_element(class(Class, Mother, Props, PrefP, Rels, PrefR, OldInst), class(Class, Mother, Props, PrefP, Rels, PrefR, NewInst), OldKB, NewKB),
	substitute_element([ObIds, ObProps, ObOldPrefP, ObRels, ObPrefR], [ObIds, ObProps, ObNewPrefP, ObRels, ObPrefR], OldInst, NewInst),
	append(ObOldPrefP, [Property], ObNewPrefP).

% Adds a new object Relation to the object corresponding to an ObjectId
% The argument "Relation" should be of the form "[ant_1, ant_2,...,ant_n]=>>[consequence,weight]", where ant_k is a relation of the form "att=>value"
new_object_relation(Relation, ObjectId, OldKB, NewKB) :-
	get_class_of(ObjectId, OldKB, Class),
	get_class_objects(Class, OldKB, OldInst),
	is_member_of_object_list(ObjectId, OldInst, ObIds),
	substitute_element(class(Class, Mother, Props, PrefP, Rels, PrefR, OldInst), class(Class, Mother, Props, PrefP, Rels, PrefR, NewInst), OldKB, NewKB),
	substitute_element([ObIds, ObProps, ObPrefP, ObOldRels, ObPrefR], [ObIds, ObProps, ObPrefP, ObNewRels, ObPrefR], OldInst, NewInst),
	append(ObOldRels, [Relation], ObNewRels).

% Adds a new Relation Preference to an object corresponding to an ObjectId
% The argument "Relation" should be of the form "[ant_1, ant_2,...,ant_n]=>>[consequence,weight]", where ant_k is a relation of the form "att=>value"
new_object_relation_preference(Relation, ObjectId, OldKB, NewKB) :-
	get_class_of(ObjectId, OldKB, Class),
	get_class_objects(Class, OldKB, OldInst),
	is_member_of_object_list(ObjectId, OldInst, ObIds),
	substitute_element(class(Class, Mother, Props, PrefP, Rels, PrefR, OldInst), class(Class, Mother, Props, PrefP, Rels, PrefR, NewInst), OldKB, NewKB),
	substitute_element([ObIds, ObProps, ObPrefP, ObRels, ObOldPrefR], [ObIds, ObProps, ObPrefP, ObRels, ObNewPrefR], OldInst, NewInst),
	append(ObOldPrefR, [Relation], ObNewPrefR).

%****************************************************************
%---------------------------------------------------------------*
%-------------------------Deleting------------------------------*
%---------------------------------------------------------------*
%****************************************************************

%------------------------------
% Functions for deleting the appereances of an object/class in every relation inside the KB
%------------------------------

%delete_all_relations_with_list
delete_all_relations_with_list([], L, L).
delete_all_relations_with_list([Id|T], OldKB, NewKB) :-
	delete_all_relations_with(Id, OldKB, X),
	delete_all_relations_with_list(T, X, NewKB).

%delete_all_relations_with
delete_all_relations_with(_,[],[]).
delete_all_relations_with(Id, [class(C,M,P,PP,OldR,PR,OldI)|T], [class(C,M,P,PP,NewR,PR,NewI)|L]) :-
	delete_relations_with(Id, OldR, NewR),
%	delete_relations_preferences_with(Id, OldPR, NewPR),	
	delete_class_relations_with(Id, OldI, NewI),
	delete_all_relations_with(Id, T, L).

% deletes_list_relation_with
% Deletes relation with a certain Id (class or object) on a class
delete_class_relations_with(_, [], []).
delete_class_relations_with(Id, [[Ids,P,PP,OldR,PR]|T], [[Ids,P,PP,NewR,PR]|L]) :-
	delete_relations_with(Id, OldR, NewR),
%	delete_relations_preferences_with(Id, OldPR, NewPR),
	delete_class_relations_with(Id, T, L).

% Deletes id from a relations list
delete_relations_with(Id, OldR, NewR) :-
	delete_element(_=>Id, OldR, X),
	delete_element(not(_=>Id), X, NewR).
delete_relations_preferences_with(Id, OldR, NewR) :-
	delete_element([_]=>>[_=>Id,_], OldR, X),
	delete_element([_]=>>[not(_=>Id),_], X, NewR).

%-----------------------------------
% MAIN Deleting routines
%-----------------------------------

% Deletes class C from the list OldKB and binds the new list to NewKB
% When deleting the class, all relations with C are deleted, all C's objects are transferred to C's parent class, and C's children's parent class is changed to C's parent class
delete_class(C, OldKB, NewKB) :-
	delete_all_relations_with(C, OldKB, X),
	get_parent_class(C, OldKB, M),
	copy_objects_to_another_class(C, M, X, Y),
	change_children_mother_class(C, M, Y, Z),
	delete_element(class(C,_,_,_,_,_,_), Z, NewKB).

% copy_objects_to_another_class
% Copies all the objects from class C in the list OldKB to a class X, and binds the new list to NewKB
copy_objects_to_another_class(C, X, OldKB, NewKB) :-
	get_class_objects(C, OldKB, O),
	substitute_element(class(X, Mother, Props, PrefP, Rels, PrefR, OldInst), class(X, Mother, Props, PrefP, Rels, PrefR, NewInst), OldKB, NewKB),
	append(OldInst, O, NewInst).

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
	substitute_element(class(C, _, Props, PrefP, Rels, PrefR, Inst), class(C, X, Props, PrefP, Rels, PrefR, Inst), OldKB, NewKB).

%delete_object
% Deletes the object that contains the id ObjectId inside its id list (aliases)
% When deleting the object, all relations to the object are deleted from the KB
delete_object(ObjectId, OldKB, NewKB) :-
	get_class_of(ObjectId, OldKB, C),
	get_class_objects(C, OldKB, OL),
	is_member_of_object_list(ObjectId, OL, IdList),
	delete_all_relations_with_list(IdList, OldKB, X),
	substitute_element(class(C, Mother, Props, PrefP, Rels, PrefR, OldInst), class(C, Mother, Props, PrefP, Rels, PrefR, NewInst), X, NewKB),
	delete_element([IdList,_,_,_,_],OldInst,NewInst).

%-----------------------------------

%delete_property_of_class(P, C, OldKB, NewKB)
% Deletes a specific property of a class C
% Property should be a property already present in the class
delete_property_of_class(Property, C, OldKB, NewKB) :-
	get_properties_from_class(C, OldKB, OldP),
	delete_element(Property, OldP, NewP),
	substitute_element(class(C, M, _, PP, R, PR, O), class(C, M, NewP, PP, R, PR, O), OldKB, NewKB).

%delete_property_preference_of_class(P, C, OldKB, NewKB)
% Deletes a specific property preference of a class C
% Property should be property preference already present in the class
delete_property_preference_of_class(Property, C, OldKB, NewKB) :-
	get_properties_preferences_from_class(C, OldKB, OldPP),
	delete_element(Property, OldPP, NewPP),
	substitute_element(class(C, M, P, _, R, PR, O), class(C, M, P, NewPP, R, PR, O), OldKB, NewKB).

%delete_relation_of_class(R, C, OldKB, NewKB)
% Deletes a specific relation of a class C
% Relation should be a relation already present in the class
delete_relation_of_class(Relation, C, OldKB, NewKB) :-
	get_relations_from_class(C, OldKB, OldR),
	delete_element(Relation, OldR, NewR),
	substitute_element(class(C, M, P, PP, _, PR, O), class(C, M, P, PP, NewR, PR, O), OldKB, NewKB).

%delete_relation_preference_of_class(R, C, OldKB, NewKB)
% Deletes a specific relation preference of a class C
% Relation should be a relation preference already present in the class
delete_relation_preference_of_class(Relation, C, OldKB, NewKB) :-
	get_relations_preferences_from_class(C, OldKB, OldPR),
	delete_element(Relation, OldPR, NewPR),
	substitute_element(class(C, M, P, PP, R, _, O), class(C, M, P, PP, R, NewPR, O), OldKB, NewKB).

%-------------------------------------

%delete_property_of_object(P, O, OldKB, NewKB)
% Deletes a specific property of an object O
% Property should be a property already present in the object's properties list
delete_property_of_object(Property, O, OldKB, NewKB) :-
	get_class_of(O, OldKB, C),		
	substitute_element(class(C, M, P, PP, R, PR, OldO), class(C, M, P, PP, R, PR, NewO), OldKB, NewKB),
	get_properties_from_object(O, OldO, OldP),
	delete_element(Property, OldP, NewP),
	substitute_element([Ids,OldP,OPP,OR,OPR], [Ids,NewP,OPP,OR,OPR], OldO, NewO).

%delete_property_preference_of_object(P, O, OldKB, NewKB)
% Deletes a specific property preference of an object O
% Property should be a property preference already present in the object's properties preferences list
delete_property_preference_of_object(Property, O, OldKB, NewKB) :-
	get_class_of(O, OldKB, C),		
	substitute_element(class(C, M, P, PP, R, PR, OldO), class(C, M, P, PP, R, PR, NewO), OldKB, NewKB),
	get_properties_preferences_from_object(O, OldO, OldOPP),
	delete_element(Property, OldOPP, NewOPP),
	substitute_element([Ids,OP,OldOPP,OR,OPR], [Ids,OP,NewOPP,OR,OPR], OldO, NewO).

%delete_relation_of_object(R, C, OldKB, NewKB)
% Deletes a specific relation of an object O
% Relation should be a relation already present in the object's relations list
delete_relation_of_object(Relation, O, OldKB, NewKB) :-
	get_class_of(O, OldKB, C),
	substitute_element(class(C, M, P, PP, R, PR, OldO), class(C, M, P, PP, R, PR, NewO), OldKB, NewKB),
	get_relations_from_object(O, OldO, OldR),
	delete_element(Relation, OldR, NewR),
	substitute_element([Ids,OP,OPP,OldR,OPR], [Ids,OP,OPP,NewR,OPR], OldO, NewO).

%delete_relation_preference_of_object(R, C, OldKB, NewKB)
% Deletes a specific relation preference of an object O
% Relation should be a relation preference already present in the object's relations preferences list
delete_relation_preference_of_object(Relation, O, OldKB, NewKB) :-
	get_class_of(O, OldKB, C),
	substitute_element(class(C, M, P, PP, R, PR, OldO), class(C, M, P, PP, R, PR, NewO), OldKB, NewKB),
	get_relations_preferences_from_object(O, OldO, OldOPR),
	delete_element(Relation, OldOPR, NewOPR),
	substitute_element([Ids,OP,OPP,OR,OldOPR], [Ids,OP,OPP,OR,NewOPR], OldO, NewO).

%****************************************************************
%---------------------------------------------------------------*
%-------------------------Changing------------------------------*
%---------------------------------------------------------------*
%****************************************************************

%------------------------------
% Functions for deleting the appereances of an object/class in every relation inside the KB
%------------------------------

%substitute_all_relations_with
substitute_all_relations_with(_,_,[],[]).
substitute_all_relations_with(Id, NewId, [class(C,M,P,PP,OldR,PR,OldI)|T], [class(C,M,P,PP,NewR,PR,NewI)|L]) :-
	substitute_relations_with(Id, NewId, OldR, NewR),
%	substitute_relations_preferences_with(Id, NewId, OldPR, NewPR),	
	substitute_class_relations_with(Id, NewId, OldI, NewI),
	substitute_all_relations_with(Id, NewId, T, L).

% substitutes_list_relation_with
% substitutes relation with a certain Id (class or object) on a class
substitute_class_relations_with(_, _, [], []).
substitute_class_relations_with(Id, NewId, [[Ids,P,PP,OldR,PR]|T], [[Ids,P,PP,NewR,PR]|L]) :-
	substitute_relations_with(Id, NewId, OldR, NewR),
%	substitute_relations_preferences_with(Id, NewId, OldPR, NewPR),
	substitute_class_relations_with(Id, NewId, T, L).

% substitutes id from a relations list
substitute_relations_with(Id, NewId, OldR, NewR) :-
	substitute_element(Ant=>Id, Ant=>NewId, OldR, X),
	substitute_element(not(Ant=>Id), not(Ant=>NewId), X, NewR).
substitute_relations_preferences_with(Id, NewId, OldR, NewR) :-
	substitute_element([Ant1]=>>[Ant2=>Id,W], [Ant1]=>>[Ant2=>NewId,W], OldR, X),
	substitute_element([Ant1]=>>[not(Ant2=>Id),W], [Ant1]=>>[not(Ant2=>NewId),W], X, NewR).

%-----------------------------------
% MAIN Changing routines
%-----------------------------------

%change_class_name(C, OldKB, NewKB)
% Changes the class C's name of C to NewName
change_class_name(C, NewName, OldKB, NewKB) :-
	substitute_all_relations_with(C, NewName, OldKB, X),
	change_children_mother_class(C, NewName, X, Y),
	substitute_element(class(C, M, P, PP, R, PR, O), class(NewName, M, P, PP, R, PR, O), Y, NewKB).

%change_object_name(ObjectId, OldKB, NewKB)
% Changes the object ObjectId's id to NewObjectId (all its other aliases remain the same)
change_object_name(ObjectId, NewObjectId, OldKB, NewKB) :-
	get_class_of(ObjectId, OldKB, C),
	get_class_objects(C, OldKB, OL),
	is_member_of_object_list(ObjectId, OL, IdList),
	substitute_all_relations_with(ObjectId, NewObjectId, OldKB, X),
	substitute_element(ObjectId, NewObjectId, IdList, NewIdList),
	substitute_element([IdList,OP,OPP,OR,ORR], [NewIdList,OP,OPP,OR,ORR], OL, NewOL),
	substitute_element(class(C, M, P, PP, R, PR, OL), class(C, M, P, PP, R, PR, NewOL), X, NewKB).

%change_property_of_class
% Changes a specific Property of a class C to NewProperty
% This function overwrites Property. To change a property value, this needs to be used as in the example: change_property_of_class(size=>medium, size=>small, penguins, KB, NewKB)
change_property_of_class(Property, NewProperty, C, OldKB, NewKB) :-
	get_properties_from_class(C, OldKB, OldP),
	substitute_element(Property, NewProperty, OldP, NewP),
	substitute_element(class(C, M, _, PP, R, PR, O), class(C, M, NewP, PP, R, PR, O), OldKB, NewKB).

%change_property_preference_of_class
% Changes a specific Property Preference of a class C to NewPropertyPref
% This function always overwrites PropertyPref. To change the weight of a certain PropertyPref, both the PropertyPref and the NewPropertyPref should be written out in the form [ant_1,ant_2...ant_n]=>>[consequence,weight]
change_property_preference_of_class(PropertyPref, NewPropertyPref, C, OldKB, NewKB) :-
	get_properties_preferences_from_class(C, OldKB, OldPP),
	substitute_element(PropertyPref, NewPropertyPref, OldPP, NewPP),
	substitute_element(class(C, M, P, _, R, PR, O), class(C, M, P, NewPP, R, PR, O), OldKB, NewKB).

%change_relation_of_class
% Changes a specific Relation of a class C to NewRelation
% This function overwrites Relation. To change a property relation (e.g. to modify who is related to), this needs to be used as in the example: change_relation_of_class(friend=>birds, friend=>fish, penguins, KB, NewKB)
change_relation_of_class(Relation, NewRelation, C, OldKB, NewKB) :-
	get_relations_from_class(C, OldKB, OldR),
	substitute_element(Relation, NewRelation, OldR, NewR),
	substitute_element(class(C, M, P, PP, _, PR, O), class(C, M, P, PP, NewR, PR, O), OldKB, NewKB).

%change_relation_preference_of_class
% Changes a specific Relation Preference of a class C to NewRelationPref
% This function always overwrites RelationPref. To change the weight of a certain RelationPref, both the RelationPref and the NewRelationPref should be written out in the form [ant_1,ant_2...ant_n]=>>[consequence,weight]
change_relation_preference_of_class(RelationPref, NewRelationPref, C, OldKB, NewKB) :-
	get_relations_preferences_from_class(C, OldKB, OldPR),
	substitute_element(RelationPref, NewRelationPref, OldPR, NewPR),
	substitute_element(class(C, M, P, PP, R, _, O), class(C, M, P, PP, R, NewPR, O), OldKB, NewKB).

%change_property_of_object
% Changes a specific Property of an object that has an id of O to NewProperty
% This function overwrites Property. To change a property value, this needs to be used as in the example: change_property_of_object(size=>medium, size=>small, arthur, KB, NewKB)
change_property_of_object(Property, NewProperty, O, OldKB, NewKB) :-
	get_class_of(O, OldKB, C),
	substitute_element(class(C, M, P, PP, R, PR, OldO), class(C, M, P, PP, R, PR, NewO), OldKB, NewKB),
	get_properties_from_object(O, OldO, OldP),
	substitute_element(Property, NewProperty, OldP, NewP),
	substitute_element([Ids,OldP,OPP,OR,OPR], [Ids,NewP,OPP,OR,OPR], OldO, NewO),
	member_of(O, Ids).

%change_property_preference_of_object(P, O, OldKB, NewKB)
% Changes a specific Property Preference PropertyPref of an object with id O to NewPropertyPref
% This function always overwrites PropertyPref. To change the weight of a certain PropertyPref, both the PropertyPref and the NewPropertyPref should be written out in the form [ant_1,ant_2...ant_n]=>>[consequence,weight]
change_property_preference_of_object(PropertyPref, NewPropertyPref, O, OldKB, NewKB) :-
	get_class_of(O, OldKB, C),		
	substitute_element(class(C, M, P, PP, R, PR, OldO), class(C, M, P, PP, R, PR, NewO), OldKB, NewKB),
	get_properties_preferences_from_object(O, OldO, OldOPP),
	substitute_element(PropertyPref, NewPropertyPref, OldOPP, NewOPP),
	substitute_element([Ids,OP,OldOPP,OR,OPR], [Ids,OP,NewOPP,OR,OPR], OldO, NewO).

%change_relation_of_object(R, C, OldKB, NewKB)
% Changes a specific Relation of an object with id O to NewRelation
% This function overwrites Relation. To change a relation value, this needs to be used as in the example: change_relation_of_object(eat=>fish, eat=>plants, arthur, KB, NewKB)
change_relation_of_object(Relation, NewRelation, O, OldKB, NewKB) :-
	get_class_of(O, OldKB, C),
	substitute_element(class(C, M, P, PP, R, PR, OldO), class(C, M, P, PP, R, PR, NewO), OldKB, NewKB),
	get_relations_from_object(O, OldO, OldR),
	substitute_element(Relation, NewRelation, OldR, NewR),
	substitute_element([Ids,OP,OPP,OldR,OPR], [Ids,OP,OPP,NewR,OPR], OldO, NewO).

%change_relation_preference_of_object(R, C, OldKB, NewKB)
% Changes a specific Relation Preference RelationPref of an object with id O to NewRelationPref
% This function always overwrites RelationPref. To change the weight of a certain RelationPref, both the RelationPref and the NewRelationPref should be written out in the form [ant_1,ant_2...ant_n]=>>[consequence,weight]
change_relation_preference_of_object(RelationPref, NewRelationPref, O, OldKB, NewKB) :-
	get_class_of(O, OldKB, C),
	substitute_element(class(C, M, P, PP, R, PR, OldO), class(C, M, P, PP, R, PR, NewO), OldKB, NewKB),
	get_relations_preferences_from_object(O, OldO, OldOPR),
	substitute_element(RelationPref, NewRelationPref, OldOPR, NewOPR),
	substitute_element([Ids,OP,OPP,OR,OldOPR], [Ids,OP,OPP,OR,NewOPR], OldO, NewO).

%****************************************************************
%---------------------------------------------------------------*
%-------------------------Consulting----------------------------*
%---------------------------------------------------------------*
%****************************************************************

%------------------------------------
% Functions for inferring properties/relations from preferences
%------------------------------------

%inferences(Rules, Pref, Prem, L)
inferences([], _, _, []).
inferences([X =>> Y|T], Pref, Prem, [Y|L]) :-
	infere(X =>> Y, Pref, Prem, yes),
	inferences(T, Pref, Prem, L).
inferences([X =>> Y|T], Pref, Prem, L) :-
	infere(X =>> Y, Pref, Prem, no),
	inferences(T, Pref, Prem, L). 

%infere(Rule, Pref, Prem, Ans)
% Say if a rule can be inferred from a set of Preferences and Premises, and bind the answer to Ans
infere(X =>> _, Pref, Prem, Ans) :-
	validate_antecedents(X, Pref, Prem, R),
	all_yes(R, Ans).

% validate_antecedents(Ant, Pref, Prem, R)
% Check if every element of Ant is either explicitly stated in the Premises, or if they can be inferred from the list of Preferences
validate_antecedents([], _, _, []).
validate_antecedents([X|T], Pref, Prem, [R|L]) :-
	validate_antecedent(X, Pref, Prem, R),
	validate_antecedents(T, Pref, Prem, L).

% validate_antecedent
% Check if a certain property/relation X is either explicitly stated in the Premises, or if it can be inferred from the list of preferences
validate_antecedent(_,_,[],no).
validate_antecedent(X, _, Prem, yes) :-
	is_in_list(X, Prem, yes).
validate_antecedent(X, Pref, Prem, yes) :-
	is_consequent_of_pref([X], Pref, P, yes),
	extract_antecedents(P, Ant),
	validate_antecedents(Ant, Pref, Prem, L),
	all_yes(L, yes).
validate_antecedent(_,_,_,no).

% Extract the antecedent from a Rule of the form Ant =>> Consequence
extract_antecedents(Ant =>> _, Ant).

%all_yes(L, R)
% Check if all the elements in a certain list L are equal to "yes", and bind the answer to R.
all_yes([], yes).
all_yes([yes|T], R) :-
	all_yes(T, R).
all_yes([no|_], no).

%is_consequent_of_pref(X, Pref, P, R)
% Say if a certain X is the consequence of a certain rule R in a list of Preferences, and bind the answer to R
is_consequent_of_pref(_, [], _, no).
is_consequent_of_pref([X], [Ant =>> [X,W]|_], Ant =>> [X,W], yes).
is_consequent_of_pref(X, [_|T], P, R) :-
	is_consequent_of_pref(X, T, P, R).


%--------------------------------------
% Routines for working with properties 
%--------------------------------------
% Gets the properties of a list of classes
get_classes_properties(_,[],[]).
get_classes_properties([],_,[]).
get_classes_properties([H|T],B,Properties):-
        get_class_properties(H,B,HProperties),
        get_classes_properties(T,B,OthersProperties),
        append(HProperties,OthersProperties,Properties).

% get_class_properties_preferences(Class,KB,PrefProps).
% Gets the list of Properties Preferences of the Class
get_class_properties_preferences(_,[],[]).
get_class_properties_preferences(Class,[class(Class,_,_,PrefProps,_,_,_)|_],PrefProps).
get_class_properties_preferences(Class,[_|T],PrefProps):-
        get_class_properties_preferences(Class,T,PrefProps).

% get_classes_properties_preferences(Classes,KB,PropPrefs).
% Gets the property preferences  of a list of classes
get_classes_properties_preferences([],_,[]).
get_classes_properties_preferences([H|T],B,PropPrefs):-
        get_class_properties_preferences(H,B,HPropPrefs),
        get_classes_properties_preferences(T,B,OthersPropPrefs),
        append(HPropPrefs,OthersPropPrefs,PropPrefs).

% Gets the explicit properties of an object 
get_local_object_properties([_,Properties,_,_,_],Properties).

% Gets the explicit properties preferences of an object
get_object_properties_preferences([_,_,ObjPropPref,_,_],ObjPropPref).

% Full properties of a class, no specificity
all_class_properties(_,[],[]).
all_class_properties(Class,B,Properties):-
    get_local_class_properties(Class,B,LocalProperties),
    get_class_ascendants(Class,B,Ascendants),
    get_local_classes_properties(Ascendants,B,AscendantsProperties),
    append(LocalProperties,AscendantsProperties,Properties).

% Properties in present class, no inheritance
get_local_class_properties(_,[],[]).
get_local_class_properties(Class,[class(Class,_,Properties,_,_,_,_)|_],Properties).
get_local_class_properties(Class,[_|T],Properties):-
        get_local_class_properties(Class,T,Properties).

% Properties of a list of classes
get_local_classes_properties([],_,[]).
get_local_classes_properties(_,[],[]).
get_local_classes_properties([H|T],B,Properties):-
     get_local_class_properties(H,B,HProperties),
     get_local_classes_properties(T,B,OthersProperties),
     append(HProperties,OthersProperties,Properties).

% pick_objs_with_property(Property,KB,IdsList,IdswProperty)
% Picks all objects in a list for which Property holds
pick_objs_with_property(_,_,[],[]).
pick_objs_with_property(_,[],_,[]).
pick_objs_with_property(Property,B,[Id|T],[R|Rest]):-
        get_object_properties(Id,B,Properties),
        is_in_list(Property,Properties,R),
        pick_objs_with_property(Property,B,T,Rest).
pick_objs_with_property(Property,B,[_|T],Rest):-
        pick_objs_with_property(Property,B,T,Rest).

resolve_objects([], [], []).
resolve_objects([Id|T], [yes|RL], [Id|L]) :-
	resolve_objects(T, RL, L).
resolve_objects([_|T], [_|RL], L) :-
	resolve_objects(T, RL, L).

%--------------------------------------
% Routines for working with relations 
%--------------------------------------
% Gets the relations of a list of classes
get_classes_relations(_,[],[]).
get_classes_relations([],_,[]).
get_classes_relations([H|T],B,Relations):-
        get_class_relations(H,B,HRelations),
        get_classes_relations(T,B,OthersRelations),
        append(HRelations,OthersRelations,Relations).

% get_class_relations_preferences(Class,KB,PrefRels).
% Gets the list of Relations Preferences of the Class
get_class_relations_preferences(_,[],[]).
get_class_relations_preferences(Class,[class(Class,_,_,_,_,PrefRels,_)|_],PrefRels).
get_class_relations_preferences(Class,[_|T],PrefRels):-
        get_class_relations_preferences(Class,T,PrefRels).

% get_classes_relations_preferences(Classes,KB,PropRels).
% Gets the relation preferences  of a list of classes
get_classes_relations_preferences([],_,[]).
get_classes_relations_preferences([H|T],B,RelPrefs):-
        get_class_relations_preferences(H,B,HRelPrefs),
        get_classes_relations_preferences(T,B,OthersRelPrefs),
        append(HRelPrefs,OthersRelPrefs,RelPrefs).

% Gets the explicit relations of an object 
get_local_object_relations([_,_,_,Relations,_],Relations).

% Gets the explicit relations preferences of an object
get_object_relations_preferences([_,_,_,_,ObjRelPref],ObjRelPref).

% Full relations of a class, no specificity
all_class_relations(_,[],[]).
all_class_relations(Class,B,Relations):-
    get_local_class_relations(Class,B,LocalRelations),
    get_class_ascendants(Class,B,Ascendants),
    get_local_classes_relations(Ascendants,B,AscendantsRelations),
    append(LocalRelations,AscendantsRelations,Relations).

% Relations in present class, no inheritance
get_local_class_relations(_,[],[]).
get_local_class_relations(Class,[class(Class,_,_,_,Relations,_,_)|_],Relations).
get_local_class_relations(Class,[_|T],Relations):-
        get_local_class_relations(Class,T,Relations).

% Relations of a list of classes
get_local_classes_relations([],_,[]).
get_local_classes_relations(_,[],[]).
get_local_classes_relations([H|T],B,Relations):-
     get_local_class_relations(H,B,HRelations),
     get_local_classes_relations(T,B,OthersRelations),
     append(HRelations,OthersRelations,Relations).

% pick_objs_with_relation(Relation,KB,IdsList,IdswRelation)
% Picks all objects in a list for which Relation holds
pick_objs_with_relation(_,_,[],[]).
pick_objs_with_relation(_,[],_,[]).
pick_objs_with_relation(Relation,B,[Id|T],[R|Rest]):-
        get_object_relations(Id,B,Relations),
	exists_relation(Relation, Relations, B, R),
%	is_in_list(Relation,Relations,R),
        pick_objs_with_relation(Relation,B,T,Rest).
pick_objs_with_relation(Relation,B,[_|T],Rest):-
        pick_objs_with_relation(Relation,B,T,Rest).

exists_relation(_, [], _, no).
exists_relation(_, _, [], no).
exists_relation(Relation, [Relation|_], _, yes).
exists_relation(Ant=>OtherId, [Ant=>Id|_], KB, yes) :-
	are_same_object(Id, OtherId, KB, yes).
exists_relation(not(Ant=>OtherId), [not(Ant=>Id)|_], KB, yes) :-
	are_same_object(Id, OtherId, KB, yes).
exists_relation(Rel, [_|T], KB, R) :-
	exists_relation(Rel, T, KB, R).

are_same_object(Id1, Id2, KB, yes) :-
	get_class_of(Id1, KB, C1),
	get_class_objects(C1, KB, OL),
	is_member_of_object_list(Id2, OL).
are_same_object(_, _, _, no).

% pick_objs_with
% Build a list of the objects that have a certain Relation, of the form
% [Object1: [objects it has a Relation with], Object2: [...], ...]
build_objects_with_relations_list(_,[],_,[]). 
build_objects_with_relations_list(Relation, [Object|T], KB, ObjWithRelations) :-
	get_object_relations(Object, KB, Rels),	
	match_relation(Relation, Rels, Matches),
	build_objects_with_relations_list(Relation, T, KB, L1),
	append([Object:Matches], L1, ObjWithRelations).
build_objects_with_relations_list(Relation, [_|T], KB, ObjWithRelations) :-
	build_objects_with_relations_list(Relation, T, KB, ObjWithRelations).

match_relation(_,[],[]).
match_relation(Relation, [Relation=>X|T], [X|L]) :-
	match_relation(Relation, T, L).
match_relation(not(Relation), [not(Relation=>X)|T], [X|L]) :-
	match_relation(not(Relation), T, L).
match_relation(Relation, [_|T], L) :-
	match_relation(Relation, T, L).

clean_objects_with_relations_list([], []).
clean_objects_with_relations_list([X:[Y|Y1]|T], [X:[Y|Y1]|L]) :-
	clean_objects_with_relations_list(T, L).
clean_objects_with_relations_list([_|T], L) :-
	clean_objects_with_relations_list(T, L).

% Build a list of the objects that have a certain property
build_objects_with_properties_list(_,[],_,[]).
build_objects_with_properties_list(Property, [Object|T], KB, ObjWithProperties) :-
	get_object_properties(Object, KB, Props),
	match_property(Property, Props, Matches),
	build_objects_with_properties_list(Property, T, KB, L1),
	append([Object:Matches], L1, ObjWithProperties).

match_property(_,[],unknown).
match_property(Property, [Property=>X|_], X).
match_property(not(Property), [not(Property=>X)|_], X).
match_property(Property, [not(Property)|_], no).
match_property(Property, [Property|_], yes).
match_property(Property, [_|T], X) :-
	match_property(Property, T, X).


%-------------------------------
% Resolve the properties and relations of a class
%-------------------------------

% Removes repetitions, inconsistencies, applies inferences,
% thus gets full properties 
infer_and_clean_props(PrefProps,First,Properties):-
        properties_cleanup(First,CleanL),
%       inferences(PrefProps,PrefProps,CleanL,Inferred),
	findall(L, inferences(PrefProps,PrefProps,CleanL,L), AllInfLists),
	flatten_list(AllInfLists,AllInf),
	delete_repetitions(AllInf, Inferred),
        order_by_weights(Inferred,OrdInferred),
        resolve_by_weights(OrdInferred,Resolved),
        append(CleanL,Resolved,Aux),
        properties_cleanup(Aux,Properties).

% Removes repetitions, inconsistencies, applies inferences,
% thus gets full properties 
infer_and_clean_rels(PrefRels,First,KB,Relations):-
        relations_cleanup(First,CleanL),
%       inferences(PrefRels,PrefRels,CleanL,Inferred),
	findall(L, inferences(PrefRels,PrefRels,CleanL,L), AllInfLists),
	flatten_list(AllInfLists,AllInf),
	delete_repetitions(AllInf, Inferred),
        order_by_weights(Inferred,OrdInferred),
        resolve_by_weights(OrdInferred,Resolved),
        append(CleanL,Resolved,Aux1),
	transform_relations(Aux1,KB,Aux2),
        relations_cleanup(Aux2,Relations).

% resolve_by_weights([[prop1,w1],...,[propK,wK]],L)
% Picks properties with lower weight
resolve_by_weights([],[]).
resolve_by_weights([[not(X),_]|T],[not(X)|Rest]):-
        delete_element([X,_],T,Aux),
        resolve_by_weights(Aux,Rest).
resolve_by_weights([[X,_]|T],[X|Rest]):-
        delete_element([not(X),_],T,Aux),
        resolve_by_weights(Aux,Rest).
resolve_by_weights([_|T],L):-
        resolve_by_weights(T,L).
	
% Infer new properties using properties and preferences, 
% Returns list [[prop1,w1],...,[propK,wK]] 
properties_cleanup([],[]).
properties_cleanup(Properties,Solved):-
        delete_repetitions(Properties,Aux1),
        overwrite_attribute(Aux1,Aux2),
        delete_inconsistencies(Aux2,Solved).

relations_cleanup([],[]).
relations_cleanup(Relations,Solved):-
        delete_repetitions(Relations,Aux1),
        delete_inconsistencies(Aux1,Solved).

overwrite_attribute([],[]).
overwrite_attribute([X => Y|T],[X => Y|Rest]):-
        delete_element(X => _,T,Aux),
        overwrite_attribute(Aux,Rest).
overwrite_attribute([H|T],[H|L]):-
        overwrite_attribute(T,L).

delete_inconsistencies([],[]).
delete_inconsistencies([not(X)|T],[not(X)|Rest]):-
        delete_element(X,T,Aux),
        delete_inconsistencies(Aux,Rest).
delete_inconsistencies([X|T],[X|Rest]):-
        delete_element(not(X),T,Aux),
        delete_inconsistencies(Aux,Rest).
delete_inconsistencies([_|T],Solved):-
        delete_inconsistencies(T,Solved).

% Transforms a list of relations into a list of to-Object relations only
transform_relations([],_,[]).
transform_relations([Relation|T], KB, Relations) :-
	extract_relation_components(Relation, _, Class),
	is_class(Class, KB),
	transform_to_object_relations(Relation, KB, L1),
	transform_relations(T, KB, L2),
	append(L1, L2, Relations).
transform_relations([Relation|T], KB, [Relation|L]) :-
	transform_relations(T, KB, L).

extract_relation_components(Ant=>Class, Ant, Class).
extract_relation_components(not(Ant=>Class), Ant, Class).

is_class(Class, KB) :-
	member_of(class(Class,_,_,_,_,_,_),KB).

% Transforms a to-Class relation to a list of to-Object relations
transform_to_object_relations(Ant=>Class, KB, L) :-
	get_class_extension(Class, KB, OL),
	build_relations_list(Ant, OL, L).
transform_to_object_relations(not(Ant=>Class), KB, L) :-
	get_class_extension(Class, KB, OL),
	build_not_relations_list(Ant, OL, L).

build_relations_list(_, [], []).
build_relations_list(Ant, [Id|T], [Ant=>Id|L]) :-
	build_relations_list(Ant, T, L).

build_not_relations_list(_, [], []).
build_not_relations_list(Ant, [Id|T], [not(Ant=>Id)|L]) :-
	build_not_relations_list(Ant, T, L).

print_extension([],[],[]).
print_extension([H|T],ObjsNotwProp,[H:yes|Rest]):-
        print_extension(T,ObjsNotwProp,Rest).
print_extension([],[H|T],[H:no|Rest]):-
        print_extension([],T,Rest).

%-----------------------------------
% MAIN Consulting routines
%-----------------------------------
% class_extension(Class,KB,Objs)
% The full list of the objects of Class in KB, observing inheritance
get_class_extension(_,[],[]).
get_class_extension(Class,B,Objs):-
    	get_class_objects(Class,B,ClassOnlyObjs),
    	get_class_descendants(Class,B,Descendants),
    	get_classes_objects(Descendants,B,DescendantsObjs),
    	append(ClassOnlyObjs,DescendantsObjs,Aux1),
    	pick_first_for_each(Aux1,Aux2),
    	pick_first_for_each(Aux2,Objs).

% get_object_membership(Id,KB,Classes). 
% The classes an object belongs to, observing inheritance
get_object_membership(_,[],[]).
get_object_membership(Id,B,Classes):-
    	get_class_of(Id,B,Class),
    	get_class_ascendants(Class,B,Ascendants),
    	append([Class],Ascendants,Classes).

% get_class_properties(Class, KB, Properties)
% Gets the full list of properties of Class in KB, observing preferences
get_class_properties(Class,B,Properties):-
        all_class_properties(Class,B,ExplicitProperties),
        get_class_properties_preferences(Class,B,PrefProps),
        infer_and_clean_props(PrefProps,ExplicitProperties,Properties).

% get_object_properties(Obj,KB,Properties).        
% Gets the full list of properties of Class in KB, observing preferences
get_object_properties(_,[],[]). 
get_object_properties(Id,B,Properties):-
        find_object_by_id(Id,B,Obj),
        get_local_object_properties(Obj,LocalProperties),
        get_object_membership(Id,B,Classes),
        get_classes_properties(Classes,B,InheritedProps),
        append(LocalProperties,InheritedProps,AllProps),
        get_object_properties_preferences(Obj,ObjPropPref),
        get_classes_properties_preferences(Classes,B, PropPref),
        append(ObjPropPref,PropPref,AllPropPref),
        infer_and_clean_props(AllPropPref,AllProps,Properties).

% get_class_relations(Class, KB, Relations)
% Gets the full list of relations of Class in KB, observing preferences
get_class_relations(Class,B,Relations):-
        all_class_relations(Class,B,ExplicitRelations),
        get_class_relations_preferences(Class,B,PrefRels),
        infer_and_clean_rels(PrefRels,ExplicitRelations,B,Relations).

% get_object_relations(Obj,KB,Relations).        
% Gets the full list of relations of Class in KB, observing preferences
get_object_relations(_,[],[]). 
get_object_relations(Id,B,Relations):-
        find_object_by_id(Id,B,Obj),
        get_local_object_relations(Obj,LocalRelations),
        get_object_membership(Id,B,Classes),
        get_classes_relations(Classes,B,InheritedRels),
        append(LocalRelations,InheritedRels,AllRels),
        get_object_relations_preferences(Obj,ObjRelPref),
        get_classes_relations_preferences(Classes,B, RelPref),
        append(ObjRelPref,RelPref,AllRelPref),
        infer_and_clean_rels(AllRelPref,AllRels,B,Relations).

% get_property_extension(Property, KB, Objs).
% Gets the list of objects for which Property holds
get_property_extension(_,[],[]).
get_property_extension(Property,B,Extension):-
        get_class_extension(top,B,AllObjs),
	build_objects_with_properties_list(Property, AllObjs, B, Extension).

% Gets the list of objects that has a certain Relation with other objects
% The argument Relation should be the "value" part of a relation "value=>attribute"
% The output is a list of the form [Object1:[objects it has a Relation with], Object2:[...],...]
get_relation_extension(_,[],[]).
get_relation_extension(Relation,B,Extension):-
        get_class_extension(top,B,AllObjs),
	build_objects_with_relations_list(Relation, AllObjs, B, Aux),
	clean_objects_with_relations_list(Aux, Extension).

%********************************************************%
%********************************************************%
%********************************************************%
%********************************************************%
%********************************************************%

%****************************************************************
%---------------------------------------------------------------*
%---------------------Supermarket Robot-------------------------*
%---------------------------------------------------------------*
%****************************************************************

%********************************************************%
%********************************************************%
%********************************************************%
%********************************************************%
%********************************************************%

%****************************************************************
%---------------------------------------------------------------*
%--------General Functions for Knowledge Extraction-------------*
%---------------------------------------------------------------*
%****************************************************************


% Get list of the ideal locations of the items in GF
get_ideal_locations_gf(KB, NShelves, IdealGF) :-
	get_class_extension(items, KB, AllItems),
	get_shelves_placement(KB, AllItems, idealLoc, ItemPlacement),
%	get_number_of_shelves(Shelves, NShelves),
	place_items_in_shelves(AllItems, 1, NShelves, ItemPlacement, IdealGF),
	!.

% Get list of the observed locations of the items in GF
get_observed_locations_gf(KB, NShelves, ObservedGF) :-
	get_class_extension(items, KB, AllItems),
	get_shelves_placement(KB, AllItems, observedLoc, ItemPlacement),
%	get_number_of_shelves(Shelves, NShelves),
	place_items_in_shelves(AllItems, 1, NShelves, ItemPlacement, ObservedGF),
	!.

% Get list of the diagnosed locations of the items in GF
get_diagnosed_locations_gf(KB, NShelves, DiagnosedGF) :-
	get_class_extension(items, KB, AllItems),
	get_shelves_placement(KB, AllItems, diagnosedLoc, ItemPlacement),
%	get_number_of_shelves(Shelves, NShelves),
	place_items_in_shelves(AllItems, 1, NShelves, ItemPlacement, DiagnosedGF),
	!.

% Get list of the real locations of the items in GF
get_real_locations_gf(KB, NShelves, DiagnosedGF) :-
	get_class_extension(items, KB, AllItems),
	get_shelves_placement(KB, AllItems, realLoc, ItemPlacement),
%	get_number_of_shelves(Shelves, NShelves),
	place_items_in_shelves(AllItems, 1, NShelves, ItemPlacement, DiagnosedGF),
	!.


% Get the ideal/observed/diagnosed shelf an item according to a "Property" parameter (this parameter can be idealLoc, observedLoc or diagnosedLoc
get_shelves_placement(_, [], _, []).
get_shelves_placement(KB, [I|Items], Property, [IPlacement|ItemPlacement]) :-
	get_explicit_object_properties(I, KB, IProps),
	get_property_from_list(Property, IProps, Shelf),
	get_explicit_object_properties(Shelf, KB, SProps),
	get_property_from_list(id, SProps, IPlacement), 
	get_shelves_placement(KB, Items, Property, ItemPlacement).

get_explicit_object_properties(ItemID, KB, Properties) :-
	find_object_by_id(ItemID,KB,Object),
        get_local_object_properties(Object,Properties).

get_property_from_list(_, [], unknown).
get_property_from_list(Property, [Property=>Value|_], Value).
get_property_from_list(Property, [_|L], Value) :-
	get_property_from_list(Property, L, Value).

% Get list of placed items
% Get a list of all the items that have property isPlaced => yes, that is, those items that are believed/known to be placed in the shelves.
get_list_of_placed_items(KB, PlacedItems) :-
	get_class_extension(items, KB, AllItems),
	validate_placed_items(KB, AllItems, PlacedItems),
	!.

% Auxiliary function to check which items in a list of items are known/reported to be in the world
validate_placed_items(_, [], []).
validate_placed_items(KB, [Item|Items], [Item|PlacedItems]) :-
	get_explicit_object_properties(Item, KB, ItemProperties),
	get_property_from_list(isPlaced, ItemProperties, yes),
	validate_placed_items(KB, Items, PlacedItems).
validate_placed_items(KB, [_|Items], PlacedItems) :-
	validate_placed_items(KB, Items, PlacedItems).


% Gets the list of the shelves that have already been visited by the robot. ObservedShelves is of the form [1 1 0 ...]. If shelf[i] = 1, the shelf with id i has already been visited. If it's 0, then it hasn't.  
get_list_of_visited_shelves(KB, ObservedShelves) :-
	get_class_extension(world, KB, Places),
	validate_shelf_types(KB, Places, Shelves),
	get_visited_property_from_shelves(KB, Shelves, Shelves, 1, ShelvesYesNo),
	transform_yesno_to_zeroone(ShelvesYesNo, ObservedShelves),
	!.

% Auxiliary functions to get the list of visited shelves
transform_yesno_to_zeroone([], []).
transform_yesno_to_zeroone([no|YesNo], [0|ZeroOne]) :-
	transform_yesno_to_zeroone(YesNo, ZeroOne).
transform_yesno_to_zeroone([yes|YesNo], [1|ZeroOne]) :-
	transform_yesno_to_zeroone(YesNo, ZeroOne).

validate_shelf_types(_, [], []).
validate_shelf_types(KB, [Place|Places], [Place|Shelves]) :-
	get_explicit_object_properties(Place, KB, PlaceProperties),
	get_property_from_list(type, PlaceProperties, shelf),
	validate_shelf_types(KB, Places, Shelves).
validate_shelf_types(KB, [_|Places], Shelves) :-
	validate_shelf_types(KB, Places, Shelves).

%get_visited_property_from_shelves(_, [], _, []).
%get_visited_property_from_shelves(KB, [Shelf|Shelves], X, [ObservedStatus|ObsShelves]) :-
%	get_explicit_object_properties(Shelf, KB, ShelfProperties),
%	get_property_from_list(id, ShelfProperties, X),
%	get_property_from_list(visited, ShelfProperties, ObservedStatus),
%	Y is X + 1,
%	get_visited_property_from_shelves(KB, Shelves, Y, ObsShelves).


%%%
get_visited_property_from_shelves(_, [], _, _, []).
get_visited_property_from_shelves(KB, [_|Shelves], TestShelves, X, [Visited|VisitedShelves]) :-
	test_vp(KB, TestShelves, 1, Visited),
	Y is X + 1,
	get_visited_property_from_shelves(KB, Shelves, TestShelves, Y, VisitedShelves).

test_vp(KB, [_|Shelves], X, Visited) :-
	Y is X + 1,
	test_vp(KB, Shelves, Y, Visited).
test_vp(KB, [Shelf|_], X, Visited) :-
	get_explicit_object_properties(Shelf, KB, ShelfProperties),
	get_property_from_list(id, ShelfProperties, X),
	get_property_from_list(visited, ShelfProperties, Visited).

% Get the contents of the shelf with the property id=>ShelfID
get_items_in_shelf(ShelfID, ShelfID, [Shelf|_], Shelf).
get_items_in_shelf(X, ShelfID, [_|ItemsGF], Shelf) :-	
	Y is X + 1,	
	get_items_in_shelf(Y, ShelfID, ItemsGF, Shelf).

%****************************************************************
%---------------------------------------------------------------*
%----------------------Robot Actions----------------------------*
%---------------------------------------------------------------*
%****************************************************************

%These functions define how the robot interacts with the world.

%------------------------------------
% High-Level Actions
%------------------------------------

%robot_get_order(Goal) -> Gets the client order
%robot_diagnose(KB, Diagnostic) -> Builds a diagnostic, updates KB (diagnosedLoc for every item)
robot_diagnose(KB, Diagnostic, NewKB) :-
	get_list_of_placed_items(KB, Items),
	get_ideal_locations_gf(KB, 3, IdealGF),
	get_observed_locations_gf(KB, 3, ObsGF),
	get_list_of_visited_shelves(KB, ObsShelves),
	diagnostic(Items, 3, IdealGF, ObsGF, ObsShelves, Diagnostic),
	transformation_df_to_gf(Diagnostic, DiagnosticGF),
	update_diagnosed_location(KB, 1, 3, DiagnosticGF, NewKB).

update_diagnosed_location(KB, ShelfID, Y, [Shelf|DiagnosticGF], NewKB) :-
	get_class_extension(KB, world, Places),
	validate_shelf_types(KB, Places, Shelves).
	
% We have a list of shelves with a containment of items
% Given an id, get the label of the shelf with that id
get_name_of_shelf(KB, ID, ShelfName) :-
	get_class_extension(world, KB, Places),
	validate_shelf_types(KB, Places, Shelves),
	get_name_of_shelf_with_id(KB, ID, Shelves, ShelfName),
	!.

get_name_of_shelf_with_id(_, _, [], []).
get_name_of_shelf_with_id(KB, ID, [Shelf|_], Shelf) :-
	get_explicit_object_properties(Shelf, KB, ShelfProperties),
	get_property_from_list(id, ShelfProperties, ID).	
get_name_of_shelf_with_id(KB, ID, [_|Shelves], Shelf) :-
	get_name_of_shelf_with_id(KB, ID, Shelves, Shelf).

%robot_make_decision(¿?) -> Builds a decision list, updates KB (decisionList)
%robot_make_plan(¿?) -> Builds a plan, updates KB (plan)
%robot_execute_plan -> TBD

%------------------------------------
% Low-Level Actions
%------------------------------------


%robot_move(KB, LocationID, Result, NewKB)
% Input: KB, a location label
% Output: A result (success/failure)
% Description: Attempt to move from the robot's current position to the LocationID.
% If successful: Update the robot's current position in the KB (the new property will be positionID => LocationID).
% Fails if: Mechanical failure (the robot couldn't get to the location -> determined in the simulation by the robot's property probMove)
robot_move(KB, LocationID, Result, NewKB) :-
	get_explicit_object_properties(robbie, KB, RobotProperties),
	get_property_from_list(probMove, RobotProperties, ProbMove),
	%validate_existing_location?	
	random(RNG),	
	robot_attempt(ProbMove, RNG, Result),
	robot_attempt_move(KB, LocationID, Result, NewKB),
	!.

robot_attempt_move(KB, LocationID, success, NewKB) :-
	change_property_of_object(positionLabel=>_, positionLabel=>LocationID, robbie, KB, NewKB), 
	atom_concat('Robot moved successfully to ', LocationID, Message),
	writeln(Message).
robot_attempt_move(KB, _, failure, KB) :-
	writeln('Robot failed while performing a move action').


%robot_search(KB, Result, NewKB)				
% Input: A location.
% Output: A result (success/failure).
% Description: Attempt to see every item that has property realLoc => robotPosition, i.e. perform a robot_search_one_item over those items. 
% If successful: Update the current location "visited" property to visited=>yes
% If unsusccesful: Same as if succesful, plus update the "isPlaced" property of the items at the current location to isPlaced=>yes
% Fails if: This action can fail either by a mechanical failure (one of the searches failed), or if, after finishing all the searches, any of the seen item's diagnosedLoc != observedLocation.
robot_search(KB, success, NewKB) :-
	get_explicit_object_properties(robbie, KB, RobotProperties),
	get_property_from_list(positionLabel, RobotProperties, RobotPosition), 
	get_explicit_object_properties(RobotPosition, KB, PosProperties),
	get_property_from_list(id, PosProperties, RobotPositionID),	%Get Robot Position
	get_real_locations_gf(KB, 3, RealLocations),			%Get real contents	
	get_items_in_shelf(1, RobotPositionID, RealLocations, RealItemsInShelf),
	get_diagnosed_locations_gf(KB, 3, DiagnosedLocations),		%Get diagnosed contents
	get_items_in_shelf(1, RobotPositionID, DiagnosedLocations, DiagnosedItemsInShelf),
	lists_are_equal(RealItemsInShelf, DiagnosedItemsInShelf),	%Compare diagnosed/real contents
	robot_search_all_items(KB, RealItemsInShelf, AuxKB),
	change_property_of_object(visited=>_, visited=>yes, RobotPosition, AuxKB, NewKB),
	writeln('Robot did not find any inconsistencies with its beliefs').

robot_search(KB, failure, NewKB) :-
	get_explicit_object_properties(robbie, KB, RobotProperties),
	get_property_from_list(positionLabel, RobotProperties, RobotPosition), 
	get_explicit_object_properties(RobotPosition, KB, PosProperties),
	get_property_from_list(id, PosProperties, RobotPositionID),	%Get Robot Position
	get_real_locations_gf(KB, 3, RealLocations),			%Get real contents	
	get_items_in_shelf(1, RobotPositionID, RealLocations, RealItemsInShelf),
	get_diagnosed_locations_gf(KB, 3, DiagnosedLocations),		%Get diagnosed contents
	get_items_in_shelf(1, RobotPositionID, DiagnosedLocations, DiagnosedItemsInShelf),
	not(lists_are_equal(RealItemsInShelf, DiagnosedItemsInShelf)),	%Compare diagnosed/real contents
	robot_search_all_items(KB, RealItemsInShelf, Aux1KB),	
	change_property_of_object(visited=>_, visited=>yes, RobotPosition, Aux1KB, Aux2KB),
	update_placed_items(Aux2KB, RealItemsInShelf, NewKB), 
	writeln('Robot found inconsistencies with its beliefs').

update_placed_items(KB, [], KB).
update_placed_items(KB, [Item|Items], NewKB) :-
	change_property_of_object(isPlaced=>_, isPlaced=>yes, Item, KB, AuxKB),
	update_placed_items(AuxKB, Items, NewKB).

robot_search_all_items(KB, [], KB).
robot_search_all_items(KB, [Item|Items], NewKB) :-
	robot_search_one_item(KB, Item, _, AuxKB),
	robot_search_all_items(AuxKB, Items, NewKB).

%Success Criteria #1 .- The ItemID was observed in this shelf. 
%Success Criteria #2 .- The items observed in the shelf correspond to the robot's diagnosis.
%Criteria #1 is contained in Criteria #2, as, by definition, if the robot came to this shelf to search for an item, it believes (diagnosed) that the item is in this shelf.


%robot_search_one_item(KB, ItemID, Result, NewKB)
% Input: An item
% Output: A result (success/failure)
% Description: Attempt to search an item at the robot's current position.
% If successful: Update the item's current observedLoc to the robot's current position (observedLoc => robotPosition).
% Fails if: Mechanical failure (the robot couldn't see the item -> determined in the simulation by the item's property probSeen), or placement failure (the search was made successfully but the item was not in the shelf).
robot_search_one_item(KB, ItemID, Result, NewKB) :-
	get_explicit_object_properties(robbie, KB, RobotProperties),
	get_property_from_list(positionLabel, RobotProperties, RobotPosition),
	get_explicit_object_properties(ItemID, KB, ItemProperties),
	get_property_from_list(realLoc, ItemProperties, ItemLocation),
	get_property_from_list(probSeen, ItemProperties, ProbSeen),
	random(RNG),
	robot_attempt(ProbSeen, RNG, ResultPrelim),
	robot_attempt_search(KB, ItemID, RobotPosition, ItemLocation, ResultPrelim, Result, NewKB),
	!.

%Case 1.- The search action was performed successfully, and the item is in the robot's current position.
robot_attempt_search(KB, ItemID, RobotPosition, RobotPosition, success, success, NewKB) :-
	change_property_of_object(observedLoc=>_, observedLoc=>RobotPosition, ItemID, KB, NewKB),
	atom_concat('The ', ItemID, Mes1),
	atom_concat(Mes1, ' was found', Message),
	writeln(Message).
	%writeln('The item was found').

%Case 2.- The search action was performed successfully, but the item is not here
robot_attempt_search(KB, _, _, _, success, failure, KB).	
	%writeln('The item was not found').

%Case 2.- The search action couldn't be performed
robot_attempt_search(KB, ItemID, _, _, failure, failure, KB) :-
	atom_concat('Robot is not sure if the ', ItemID, Mes1),
	atom_concat(Mes1, ' is in the current location', Message),
	writeln(Message).
%	writeln('Robot is not sure if the item is in the current location').


%robot_pick(KB, ItemID, Result, NewKB)
% Input: An item
% Output: A result (success/failure)
% Attempt to pick an item at the robot's current position. Update KB if the attempt was successful.
% If successful: Update the item's current observedLoc and realLoc to the robot's hand (observedLoc => robotHand, realLoc => robotHand). Update one of the empty robot's hands to ItemID.
% Fails if: Mechanical failure (the robot couldn't pick the item -> determined in the simulation by the item's property probPicked)
robot_pick(KB, ItemID, Result, NewKB) :-
	get_explicit_object_properties(ItemID, KB, ItemProperties),
	get_property_from_list(probPicked, ItemProperties, ProbPicked),
	%validate_item_is_at_robot_position?
	random(RNG),
	robot_attempt(ProbPicked, RNG, Result),
	robot_attempt_pick(KB, ItemID, Result, NewKB),
	!.

robot_attempt_pick(KB, ItemID, success, NewKB) :-
	change_property_of_object(realLoc=>_,realLoc=>robot, ItemID, KB, Aux1KB),
	change_property_of_object(observedLoc=>_,observedLoc=>robot, ItemID, Aux1KB, Aux2KB),
	get_explicit_object_properties(robbie, Aux2KB, RobotProperties),
	get_property_from_list(hands, RobotProperties, Hands),
	place_item_in_robot_hands(ItemID, Hands, NewHands),
	change_property_of_object(hands=>_, hands=>NewHands, robbie, Aux2KB, NewKB), 
	atom_concat('The ', ItemID, Mes1),
	atom_concat(Mes1, ' was picked up by the robot', Message),
	writeln(Message).
	%writeln('The item was picked up by the robot').

robot_attempt_pick(KB, ItemID, failure, KB) :-
	atom_concat('The ', ItemID, Mes1),
	atom_concat(Mes1, ' could not be picked up', Message),
	writeln(Message).
	%writeln('The item could not be picked up').

place_item_in_robot_hands(Item, [empty, empty], [Item, empty]).
place_item_in_robot_hands(Item, [X, empty], [X, Item]).


%robot_place(KB, ItemID, Result, NewKB)
% Input: An item
% Output: A result (success/failure)
% Attempt to place an item in the robot's hands at the robot's current position. Update KB if the attempt was successful.
% If successful: Update the item's current observedLoc and realLoc to the robot's current position (observedLoc => robotPosition, realLoc => robotPosition). Update the robot hand's in which the ItemID was placed to empty.
% Fails if: Mechanical failure (the robot couldn't place the item -> determined in the simulation by the item's property probPlaced.
robot_place(KB, ItemID, Result, NewKB) :-
	get_explicit_object_properties(ItemID, KB, ItemProperties),
	get_property_from_list(probPlaced, ItemProperties, ProbPlaced),
	%validate_item_is_in_robot_hands?
	random(RNG),
	robot_attempt(ProbPlaced, RNG, Result),
	robot_attempt_place(KB, ItemID, Result, NewKB),
	!.

robot_attempt_place(KB, ItemID, success, NewKB) :-
	get_explicit_object_properties(robbie, KB, RobotProperties),
	get_property_from_list(positionLabel, RobotProperties, RobotPosition),
	change_property_of_object(realLoc=>_, realLoc=>RobotPosition, ItemID, KB, Aux1KB),
	change_property_of_object(observedLoc=>_, observedLoc=>RobotPosition, ItemID, Aux1KB, Aux2KB),
	get_property_from_list(hands, RobotProperties, Hands),
	remove_item_from_robot_hands(ItemID, Hands, NewHands),
	change_property_of_object(hands=>_, hands=>NewHands, robbie, Aux2KB, NewKB),
	atom_concat('The ', ItemID, Mes1),
	atom_concat(Mes1, ' was placed successfully', Message),
	writeln(Message).

robot_attempt_place(KB, ItemID, failure, KB) :-
	atom_concat('The ', ItemID, Mes1),
	atom_concat(Mes1, ' could not be placed', Message),
	writeln(Message). 

remove_item_from_robot_hands(Item, [Item, X], [empty, X]).
remove_item_from_robot_hands(Item, [X, Item], [X, empty]).


	

%------------------------------------
% Auxiliary functions
%------------------------------------

% Attempt to perform an action with a certain probability of success. If the random number generator generates a number less than the SuccessProbability, the robot performs the action successfully.
robot_attempt(SuccessProbability, RandomNumber, success) :-
	RandomNumber < SuccessProbability.
robot_attempt(SuccessProbability, RandomNumber, failure) :-
	RandomNumber >= SuccessProbability.
	


%****************************************************************
%---------------------------------------------------------------*
%-------------------------Simulation----------------------------*
%---------------------------------------------------------------*
%****************************************************************

%robot_get_order(Goal)
%robot_diagnose(KB, 


%****************************************************************
%---------------------------------------------------------------*
%----------------------Other Functions--------------------------*
%---------------------------------------------------------------*
%****************************************************************

%------------------------------------
% Auxiliary functions for working with lists
%------------------------------------

% suffix(X, Y)
% Tells if X is a suffix of Y
suffix(Xs, Xs).
suffix(Xs, [_|Ys]) :-
	suffix(Xs, Ys).

% sublist(X, Y)
% Tells if X is a sublist of Y
sublist(Xs, Ys) :-
	prefix(Ps, Ys), suffix(Xs, Ps).
sublist(Xs, Ys) :-
	prefix(Xs, Ss), suffix(Ss, Ys).
sublist(Xs, Ys) :-
	prefix(Xs, Ys).
sublist(Xs, [_|Ys]) :-
	sublist(Xs, Ys).
sublist(Xs, AsXsBs) :-
	append(_, XsBs, AsXsBs),
	append(Xs, _, XsBs).
sublist(Xs, AsXsBs) :-
	append(AsXs, _, AsXsBs),
	append(_, Xs, AsXs).

% are_sublists(X, Y)
% X is a list of lists
% Tells if all lists inside X are sublists of Y
are_sublists([], _).
are_sublists([X|T], List) :-
	sublist(X, List),
	are_sublists(T, List),
	!.

%****************************************************************
%---------------------------------------------------------------*
%---------------------Diagnostic Module-------------------------*
%---------------------------------------------------------------*
%****************************************************************

%-----------------------------------
% Utilities for choosing a diagnostic
%-----------------------------------

% Build a list of tuples of the form <Diagnostic, #misplaced_items>
build_list_of_tuples([], []).
build_list_of_tuples([L|T1], [[L,Count]|T2]) :-
	count_placed_items(L, Count),
	build_list_of_tuples(T1, T2).

% Find the tuple <L, Count> with the minimum count inside a list of tuples
min_count([Min], Min).
min_count([[L1,Count1],[_,Count2]|T], Min) :-
	Count1 =< Count2,
	min_count([[L1, Count1]|T], Min).
min_count([[_,Count1],[L2,Count2]|T], Min) :-
	Count1 > Count2,
	min_count([[L2, Count2]|T], Min).

% Find the tuple <L, Count> with the maximum count inside a list of tuples
max_count([Max], Max).
max_count([[L1,Count1],[_,Count2]|T], Max) :-
	Count1 > Count2,
	max_count([[L1, Count1]|T], Max).
max_count([[_,Count1],[L2,Count2]|T], Max) :-
	Count1 =< Count2,
	max_count([[L2, Count2]|T], Max).

% Dilligence Heuristic - Count the number of items that the assistant put into the shelves
count_placed_items([], 0).
count_placed_items([misplace(_)|Diagnostic], X) :-
	count_placed_items(Diagnostic, Y),
	X is Y + 9,
	!.
count_placed_items([place(_)|Diagnostic], X) :-
	count_placed_items(Diagnostic, Y),
	X is Y + 10,
	!.
count_placed_items([_|T], X) :-
	count_placed_items(T, X).

% Laziness Heuristic

%-----------------------------------
% Diagnostic form to general form transformation
%-----------------------------------

% Transform all the actions inside a diagnostic

transformation_df_to_gf(D, DGF) :-
	transformation_df_to_nf(D, DNF),
	transformation_nf_to_gf(DNF, DGF).

% Transform a diagnostic from the form [shelfID1=>[Items], shelfID2=>[Items]...] into the general form [[Objects believed to be in shelf1], [Objects believed to be in shelf2],...]
transformation_nf_to_gf(D, DGF):-
	get_shelf1(D, S1),
	get_shelf2(D, S2),
	get_shelf3(D, S3),
	append([S1], [S2], Aux1),
	append(Aux1, [S3], DGF),
	!.

get_shelf1([], []).
get_shelf1([shelf1=>X|_], X).
get_shelf1([_|T], X) :-
	get_shelf1(T, X).
get_shelf2([], []).
get_shelf2([shelf2=>X|_], X).
get_shelf2([_|T], X) :-
	get_shelf2(T, X).
get_shelf3([], []).
get_shelf3([shelf3=>X|_], X).
get_shelf3([_|T], X) :-
	get_shelf3(T, X).

% Transform a diagnostic of the form [place(X), misplace(Y), move(shelfID)...] into the form [shelfID1=>[Items], shelfID2=>[Items]...]  
transformation_df_to_nf(L, D):-
	find_all_moves(L, Moves),
	flatten_action(L, FL),
	split_all(FL, Moves, D),
	!.

% Eliminates all prefixes "place" and "misplace" from a list of actions
flatten_action([], []).
flatten_action([place(X)|L], [X|A]) :-
	flatten_action(L, A).
flatten_action([misplace(X)|L], [X|A]) :-
	flatten_action(L, A).
flatten_action([move(X)|L], [move(X)|A]) :-
	flatten_action(L, A).

% Splits a list in DF into multiple lists in NF
split_all(_, [], []).
split_all(L, [move(X)|Moves], D) :-
	split(move(X), L, LM, RM),
	split_all(RM, Moves, D1),
	append([X=>LM], D1, D).

find_all_moves([], []).
find_all_moves([move(X)|L], [move(X)|Moves]) :-
	find_all_moves(L, Moves).
find_all_moves([_|L], Moves) :-
	find_all_moves(L, Moves).

split(X, L, LM, RM) :-
	leftmost(X, L, LM),
	rightmost(X, L, RM).

leftmost(X, [X|_], []).
leftmost(X, [H|T], [H|L]) :-
	leftmost(X, T, L).

rightmost(X, L, RM) :-
	reverse(L, L1),
	leftmost(X, L1, RM1),
	reverse(RM1, RM).


%-----------------------------------
% General form to diagnostic form transformation
%-----------------------------------
transformation_gf_to_df(ListGF, Ideal, ListDF) :-
	transformation_gf_to_df_all(ListGF, Ideal, 0, Aux1),
	clean_lists(Aux1, Aux2),
	flatten_list(Aux2, ListDF).

transformation_gf_to_df_all([], _, _, []).
transformation_gf_to_df_all([GF|ListGF], Ideal, X, [DF|ListDF]) :-
	Y is X + 1,
	transformation_gf_to_df_single(GF, Ideal, Y, DF),
	transformation_gf_to_df_all(ListGF, Ideal, Y, ListDF).

transformation_gf_to_df_single([], _, X, [move(Shelf)]) :-
	atom_concat(shelf, X, Shelf).
transformation_gf_to_df_single([Item|GF], Ideal, X, [place(Item)|DF]) :-
	get_shelf_id(Item, Ideal, 1, X),
	transformation_gf_to_df_single(GF, Ideal, X, DF).
transformation_gf_to_df_single([Item|GF], Ideal, X, [misplace(Item)|DF]) :-
	get_shelf_id(Item, Ideal, 1, IdealID),
	different_ids(IdealID, X),
	transformation_gf_to_df_single(GF, Ideal, X, DF).
	
%-------------------------------------
% Functions for list transformation
%-------------------------------------
different_ids(ID1, ID2) :-
	ID1 > ID2.
different_ids(ID1, ID2) :-
	ID1 < ID2.

% get_shelf_id(Item, Shelves, InitialCounter, ID).
% Gets the shelf ID in which the item is placed, given a list of Shelves (InitialCounter must be always set to 1 when calling this function.
get_shelf_id(_, [], _, 0).
get_shelf_id(X, [IShelf|_], ID, ID) :-
	member_of(X, IShelf).
get_shelf_id(X, [IShelf|Shelves], CurrentShelf, ID) :-
	not(member_of(X, IShelf)),
	NextShelf is CurrentShelf + 1,
	get_shelf_id(X, Shelves, NextShelf, ID).

% Clean lists (intermediate step between GF to DF transformation)
clean_lists([], []).
clean_lists([A|L1], [B|L2]) :-
	clean_obs(A, B),
	clean_lists(L1, L2),
	!.

clean_obs([move(_)], []).
clean_obs(L, L).


%-------------------------------------
% Diagnostic generation
%-------------------------------------

% Diagnostic Form: [action1(item1), action2(item2)...actionN(itemN)]
% General Form: [[Items in shelf1], [Items in shelf2], [Items in shelf3]]

% Build a complete diagnostic
diagnostic(Items, NShelves, Ideal, ObsGF, ObsShelves, Diagnostic) :-
	findall(D, suggest_diagnostic_df_with_obs(Items, NShelves, Ideal, ObsGF, ObsShelves, D), LD),
	build_list_of_tuples(LD, LDH),
	max_count(LDH, [Diagnostic,_]).

% Given a list of items, a list of ideal places for those items in GF, a list of observed places for those items in GF, and a list of the shelves that have already been observed, suggest a diagnostic in DF
suggest_diagnostic_df_with_obs(Items, NShelves, Ideal, ObsGF, ObsShelves, Diagnostic) :-
	suggest_diagnostic_gf_with_obs(Items, NShelves, ObsGF, ObsShelves, DGF),
	transformation_gf_to_df(DGF, Ideal, Diagnostic).

% Given a list of items, and a list of ideal places for those items in GF, suggest a diagnostic in DF.
suggest_diagnostic_df(Items, NShelves, Ideal, Diagnostic) :-
	suggest_diagnostic_gf(Items, NShelves, DGF),
	transformation_gf_to_df(DGF, Ideal, Diagnostic).

% Given a list of items, a list (GF) of observed items, and a list of the shelves that have already been visited, suggest a diagnostic in general form
suggest_diagnostic_gf_with_obs(Items, NShelves, ObsGF, ObsShelves, DiagnosticGF) :-
	suggest_diagnostic_gf(Items, NShelves, DiagnosticGF),
	matches_observations(ObsGF, ObsShelves, DiagnosticGF).

% Make sure that, if a shelf has been observed (the observations match the diagnostic)
matches_observations([], [], []).
matches_observations([_|ObsGF], [0|ObsShelves], [_|DGF]) :-
	matches_observations(ObsGF, ObsShelves, DGF).
matches_observations([OShelf|ObsGF], [1|ObsShelves], [DShelf|DGF]) :-
	lists_are_equal(OShelf, DShelf),
	matches_observations(ObsGF, ObsShelves, DGF).

lists_are_equal(L1, L2) :-
	all_elements_in(L1, L2),
	all_elements_in(L2, L1).

all_elements_in([], _).
all_elements_in([X|T], L) :-
	member_of(X, L),
	all_elements_in(T, L).
	
% suggest_diagnostic_gf(Items, N, DiagnosticGF)
% Given a list of Items and a number N of shelves, suggest a diagnostic in general form
suggest_diagnostic_gf(Items, NShelves, DiagnosticGF) :-
	generate_list_of_items_placement(Items, NShelves, ItemsPlacement),
	place_items_in_shelves(Items, 1, NShelves, ItemsPlacement, DiagnosticGF).

% Distribute all Items across the N shelves, according to a placement list.
place_items_in_shelves(Items, NShelves, NShelves, IPlacement, [Shelf]) :-
	place_items_in_shelf(NShelves, Items, IPlacement, Shelf).
place_items_in_shelves(Items, CurrentShelfID, NShelves, IPlacement, [Shelf|Shelves]) :-
	place_items_in_shelf(CurrentShelfID, Items, IPlacement, Shelf),
	NextShelfID is CurrentShelfID + 1,
	NextShelfID =< NShelves,
	place_items_in_shelves(Items, NextShelfID, NShelves, IPlacement, Shelves).

place_items_in_shelf(_, [], [], []).
place_items_in_shelf(ID, [Item|Items], [ID|Shelves], [Item|Shelf]) :-
	place_items_in_shelf(ID, Items, Shelves, Shelf).
place_items_in_shelf(ID, [_|Items], [OtherID|Shelves], Shelf) :-
	different_ids(ID, OtherID),
	place_items_in_shelf(ID, Items, Shelves, Shelf). 

%generate_list(ListOfItems, N, ListOfShelves)
% Generation of all list of #Items elements containing numbers between 0 and N. Each element in this list corresponds to the shelf in which the element in the list of items was placed.
generate_list_of_items_placement([], _, []).
generate_list_of_items_placement([_|T], NShelves, [X|L]) :-
	pick_number_less_than(X, NShelves),
	generate_list_of_items_placement(T, NShelves, L).

pick_number_less_than(X, N) :-
	pick_number(X),
	X =< N.

pick_number(0).	%Item wasn't placed in any shelf
pick_number(1). %Item was placed in shelf 1
pick_number(2). %Item was placed in shelf 2
pick_number(3). %...
pick_number(4). %...
pick_number(5). %...


















