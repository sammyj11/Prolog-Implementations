ibt(empty).
ibt(node(N, L, R)):- integer(N), ibt(L), ibt(R).

size(empty,0).    /* Axiomatic case when empty tree given then size is 0 */
size(node(_,L,R),K):-  size(L,M) , size(R,J)  , K1 is M+J,  K is K1 +1.  /* recursive case that size of tree must be 1 + size of left child + size of right child as number of nodes of tree is sum of nodes of (left and right)subtree of rootnode of tree +1 to count the rootnode */

maxC(X,Y,X):- X>=Y.
maxC(X,Y,Y) :- Y> X.  /* A helper function to help calcualte max of 2 elements */

height(empty,0).
height(node(_,L,R),K) :- height(L, M), Q is M,  height(R,N), O is N, maxC(Q,O,T), TV is T, K is TV+1. /* the height of a tree is 1 + max(height(LBT),height(RBT)) becuase this tree will have height greater than its subtrees and it can dffer from max value by 1 as rootnode is 1 lvl abpve its subtrees so recursive relation will hold via induction */

preorder(empty,[]).
preorder(node(N,L,R),[H|Ts]):-  H is N, preorder(L,Ts1),  preorder(R,Ts2), append(Ts1,Ts2,Ts). /* by definition, preorder is printing order in which we print rootnode, then leftsubtree's preorder then right subtree's preorder */

inorder(empty,[]).
inorder(node(N,L,R),G):-   inorder(L,Ts1),  inorder(R,Ts2), append(Ts1,[N],Ts), append(Ts,Ts2,G). /* by definition , inorder is printing order in which we print inorder of left subtree , then value of rootnode, then right subtree's inorder */

postorder(empty,[]).
postorder(node(N,L,R),G):-   postorder(L,Ts1),  postorder(R,Ts2), append(Ts1,Ts2,Ts), append(Ts,[N],G). /* by definition , postorder is printing order in which we print pstorder of left subtree , then right subtree's postorder, then value of rootnode */

tlPreorderHelp(empty,[],[],[]).
tlPreorderHelp(empty,Ts,K,L):-accesser(K,0,Elem), splitter(K,0,_,L2),tlPreorderHelp(Elem,Ts,L2,L).
tlPreorderHelp(empty,Ts,[],L):- L = Ts.
tlPreorderHelp(node(N,empty,empty), Ts ,[],L):- append(Ts,[N],L).
tlPreorderHelp(node(N,empty,empty), [N] ,[],L):- L = [N].
tlPreorderHelp(node(N,empty,empty),Ts,K,L):- append(Ts,[N],Ts1),accesser(K,0,Elem), splitter(K,0,_,L2),tlPreorderHelp(Elem,Ts1,L2,L).
tlPreorderHelp(node(N,L,R),Ts, B,FY):- append(Ts,[N],Ts1),   K = [R|B], tlPreorderHelp(L,Ts1,K,FY).

trPreorder(empty,[]).
trPreorder(node(N,L,R),LI) :- tlPreorderHelp(node(N,L,R),[], [],LI).
/* Basically I append the node value in a list, then recurse on left subtree and store right subtree in a stack, when I reach bottom most left child, i pop the stack for immediate right child and then continue this process, Node values are appended simultaneously and termination base cases have been defined for singleton tree giving singleton list as answer */
/*  B or K stores stack, Ts stores the list temporarily and FY or L give final answer */
tlInorderHelp(empty,[],[],[],[]).
tlInorderHelp(empty,Ts,K,Q,L):-accesser(K,0,Elem), splitter(K,0,_,L2), accesser(Q,0,Elem1), splitter(Q,0,_,Q1),append(Ts,[Elem1],Ts1),tlInorderHelp(Elem,Ts1,L2,Q1,L).
tlInorderHelp(empty,[H|Ts],[],[],L):- L = [H|Ts].
tlInorderHelp(node(N,empty,empty), [N] ,[],[],[N]). /*   check */
tlInorderHelp(node(N,empty,empty),Ts,[],[],L):- append(Ts,[N],L).
tlInorderHelp(node(N,empty,empty),Ts,K,[],L):-accesser(K,0,Elem), splitter(K,0,_,L2),   append(Ts,[N],Ts1) ,tlInorderHelp(Elem,Ts1,L2,[],L ).
tlInorderHelp(node(N,empty,empty),Ts,K,Q,L):-accesser(K,0,Elem), splitter(K,0,_,L2), accesser(Q,0,Elem1), splitter(Q,0,_,Q1), append(Ts,[N,Elem1],Ts1) ,tlInorderHelp(Elem,Ts1,L2,Q1,L).
tlInorderHelp(node(N,L,R),Ts, B,Q,LIP):- K = [R|B], Q1 = [N|Q] ,tlInorderHelp(L,Ts,K,Q1,LIP).
trInorder(empty,[]).
trInorder(node(N,L,R),LIP) :- tlInorderHelp(node(N,L,R),[], [],[],LIP).
/* Basically it maintains 2 stacks through (B or K) and Q (as seen in last line) . B keeps the right subtrees and Q keeps the rootnode values. when we reach leftmost child, we  append its value, then its parent value through Q and right subtree value through B or K as used */
/* Base cases have been defined accordingly for termination that singleton node gives singleton tree as answer, L gives final answer, Ts just maintains the list */

firstHelp([A, B|Ts2],Ts,L):-   integer(A),integer(B),append(Ts,[A],Ts1), firstHelp([B|Ts2],Ts1,L).
firstHelp([H], Ts,L):-append(Ts,[H],Ts1), tlPostorderHelp(empty,Ts1,[],L).
firstHelp([H, node(X,Y,Z)|Ts2],Ts,L):- append(Ts,[H],Ts1), accesser([node(X,Y,Z)|Ts2],0,Elem),splitter([node(X,Y,Z)|Ts2],0,_,L2),tlPostorderHelp(Elem,Ts1,L2,L).
tlPostorderHelp(empty,[],[],[]).
tlPostorderHelp(empty, Ts ,[node(X,Y,Z)| Ms],L):- accesser([node(X,Y,Z)| Ms],0,Elem), splitter([node(X,Y,Z)| Ms],0,_,L2),tlPostorderHelp(Elem,Ts,L2,L).
tlPostorderHelp(empty,Ts,K,L):- firstHelp(K,Ts,L).
tlPostorderHelp(empty,[H|Ts],[],L):- L = [H|Ts].
tlPostorderHelp(node(N,empty,empty), [N] ,[],L):- L = [N].
tlPostorderHelp(node(N,empty,empty), Ts ,[node(X,Y,Z)| Ms],L):- append(Ts,[N],Ts4),accesser([node(X,Y,Z)| Ms],0,Elem), splitter([node(X,Y,Z)| Ms],0,_,L2),tlPostorderHelp(Elem,Ts4,L2,L).
tlPostorderHelp(node(N,empty,empty),Ts,K,L):- append(Ts,[N],Ts1) ,firstHelp(K,Ts1,L).
tlPostorderHelp(node(N,L,empty),Ts, B,FY):- K = [N|B], tlPostorderHelp(L,Ts,K,FY).
tlPostorderHelp(node(N,L,R),Ts, B,FY):- K = [R,N|B], tlPostorderHelp(L,Ts,K,FY).
trPostorder(empty,[]).
trPostorder(node(N,L,R),LI) :- tlPostorderHelp(node(N,L,R), [],[],LI).
/* Basically, I was putting both right subtree and rootnode value in a stack and recursing on left subtree only, once i found that I cant go left, i ll pop the latest right subtree that I have and work on it, and if I have integers, i can directly add them to the list(append) which is done by firstHelp, if we have 2 back to back integers we need to recall it also and append integers, otherwise if a node comes we need to move back to tlPostorderHelp amd continue on its left subtrees */
/* It was also important to handle cases like there is no right subtree but only left subtree which has been done in 2nd last line of tlPostorderHelp */
/* The only time I can append elements is when I reach leftmostleaf in a particular subtree */
eulerTour(empty,[]). 
eulerTour(node(N,empty,empty),[N,N,N]).
eulerTour(node(N,L,R),G):- eulerTour(L,G1),append([N],G1,G2) /* Left child ET then put root at start and end then right child ET then root at end    */,append(G2,[N],G3), eulerTour(R,G4), append(G4,[N],G5),append(G3,G5,G).
/* euler Tour essentially has to print each node thrice, so base case for empty and leaf nodes have been defined accordingly. For recursive case, we need  to append node value of parent then list of euler tour of left subtree, then again value of the parent node and then euler tour of right subtree anf then again value of parent, all this recursively happens at each step */


preETHelp([],[],[],[]).
preETHelp([],Ts,_,J):- J = Ts.
preETHelp(ET,L,K,J):- accesser(ET,0,Elem), member(Elem,K), splitter(ET,0,_,ET1), preETHelp(ET1,L,K,J).
preETHelp(ET,L,K,J):- accesser(ET,0,Elem),  tupleExtract(Elem,ElemF),append(K,[Elem],K1), append(L,[ElemF],L1),splitter(ET,0,_,ET1), preETHelp(ET1,L1,K1,J).

preET(BT,L):- eulerTourModify(BT,K,1), preETHelp(K,[],[],L).

/* comments after postET */
inETHelp([],[],[],[],[]).
inETHelp([],Ts,_,_,J):- J = Ts.
inETHelp(ET,L,F,S,J):- accesser(ET,0,Elem),member(Elem,F), member(Elem,S), splitter(ET,0,_,ET1), inETHelp(ET1,L,F,S,J).
inETHelp(ET,L,F,S,J):- accesser(ET,0,Elem),member(Elem,F), tupleExtract(Elem,ElemF) ,append(S,[Elem],S1) , append(L,[ElemF],L1),splitter(ET,0,_,ET1), inETHelp(ET1,L1,F,S1,J).
inETHelp(ET,L,F,S,J):- accesser(ET,0,Elem), append(F,[Elem],F1),splitter(ET,0,_,ET1), inETHelp(ET1,L,F1,S,J).
inET(BT,L):- eulerTourModify(BT,K,1), inETHelp(K,[],[],[],L).

postETHelp([],[],[],[],[],[]).
postETHelp([],Ts,_,_,_,J):- J = Ts.
postETHelp(ET,L,F,S,T,J):- accesser(ET,0,Elem),member(Elem,F) ,member(Elem,S)  , member(Elem,T) ,splitter(ET,0,_,ET1), postETHelp(ET1,L,F,S,J).
postETHelp(ET,L,F,S,T,J):- accesser(ET,0,Elem),member(Elem,F), member(Elem,S)  , tupleExtract(Elem,ElemF) , append(L,[ElemF],L1) , append(T,[Elem],T1) , splitter(ET,0,_,ET1), postETHelp(ET1,L1,F,S,T1,J).
postETHelp(ET,L,F,S,T,J):- accesser(ET,0,Elem),member(Elem,F)  , append([Elem],S,S1),splitter(ET,0,_,ET1), postETHelp(ET1,L,F,S1,T,J).
postETHelp(ET,L,F,S,T,J):- accesser(ET,0,Elem), append(F,[Elem],F1),splitter(ET,0,_,ET1), postETHelp(ET1,L,F1,S,T,J).
postET(BT,L):- eulerTourModify(BT,K,1), postETHelp(K,[],[],[],[],L).
/* The PostETHelp essentially has three stacks to keep track of first occurence of elements, once checked element is there in forst stack we add to second and then to third. When the element is getting added for the 3rd time, that means we found its 3rd occurence, which we also need to store in our final list for printing postorder so we append it there also. Rest we are just splitting the Euler Tour at each step ie removing its head so that we can move ahead to process the next element. Splitter and accesser have been explained above */
/* J is the final list to be printed , F,S,T are first second and third stacks, L is temp list to keep track and ET is eulerTour */
/* preET and inET are also working on similar logic but just have 1 and 2 stacks respectively as they need first and second occurence respectively */

toString(empty, "()"). 
toString(node(N,LBT,RBT), S) :- toString(LBT,S1), toString(RBT,S2)  , number_string(N, JK), string_concat(JK, ", ", GV ) ,string_concat(GV, S1, F1), string_concat(F1, ",",G1), string_concat(G1, " ", G2) , string_concat(S2,")",F2), string_concat(G2,F2,S6), string_concat("(", S6, S).
/* toString recursively calls toString on left and right subtrees and then concatenates suitable brakcets and spaces and then finally makes the node in suitable format as given in question . Base case returns "()" for empty tree . */


abso(X,X) :- X>= 0 .
abso(X,Y) :- 0 >= X, Y is -1*X .
/* makes absolute value */

isBalanced(empty):- true.
isBalanced(node(_,LBT,RBT)):-  height(LBT,KI), height(RBT,KI2), T is KI - KI2, abso(T,J), TU is J, TU <2 , isBalanced(LBT), isBalanced(RBT).
/* basically sees if absolute value of difference of heights of subtrees is <= 1 and also that subtrees are balanced */


isBST(empty):- true.
isBST(node(_,empty,empty)):- true.
isBST(node(N,node(N1,LBTL,LBTR),empty)):- N> N1, isBST(node(N1,LBTL,LBTR)) ,true.
isBST(node(N,empty,node(N1,RBTL,RBTR))):- N< N1, isBST(node(N1,RBTL,RBTR)), true.
isBST(node(N,node(N1,LBTL,LBTR),node(N2,RBTL,RBTR))) :- N > N1, N < N2, isBST(node(N1,LBTL,LBTR)), isBST(node(N2,RBTL,RBTR)).
/* Basically checks if left subtree has node value less than parent, right has greater, and then recurses on left and right subtrees, various cases for no rightsubtree or no left subtree or leaf have been added */

accesser([H|_],0,H).
accesser([_|Ts],I,Elem):- J is (I-1), accesser(Ts,J,Elem).
/* helper function to access ith location element in list */

splitter([_],0,[],[]).
splitter([_|Ts],0,[],Ts).
splitter([H|Ts],I,L1,L2):- J is (I-1), splitter(Ts,J,K1,L2), append([H],K1,L1).
/*splits a list into 2 parts about an element whose index has been provided and doesnt include the element in both lists */

makeBST([],empty).
makeBST([N],node(N,empty,empty)).
makeBST(L,BST):- sort(L,K), length(K,M), N is ((M-1)//2) , accesser(K,N,Elem), splitter(K,N,L1,L2),makeBST(L1,BST1), makeBST(L2,BST2), BST = node(Elem,BST1,BST2).
/* sorts the list, the accesses middle element, puts it as value of node and splits the list about it, and subtrees form by these 2 lists become left and right subtrees */

lookup(_,empty):- false.
lookup(L,node(N,empty,empty)):- L is N.
lookup(N,node(N,_,_)):- true.
lookup(K,node(N,BST1,_)):- K<N, lookup(K,BST1).
lookup(K,node(N,_,BST2)):- K>N , lookup(K,BST2).
/* Since we have a BST, we see if element is equal to value in the present node, if yes we are done, otherwise check whether it is greater or smaller and accordingly recurse on left and right subtrees */

insert(N,empty,BT):- BT = node(N,empty,empty).
insert(N, node(K,empty,empty),BT):- N<K , BT=node(K,node(N,empty,empty),empty).
insert(N, node(K,empty,empty),BT):- N>K, BT =node(K,empty,node(N,empty,empty)).
insert(N,node(K,L,R),BST2) :- N<K, insert(N,L,BST21), BST2 = node(K,BST21,R).
insert(N,node(K,L,R),BST2):- N>K , insert(N,R,BST21), BST2 = node(K,L,BST21).
/* first it finds suitable location for addition of new value by going to left and right subtree depending on whether value is smaller or greater than value in present node. Then once it finds a leaf, using base case we attach it to the tree and hence insert it */

indexFind([N],N,0).
indexFind([H|_],H,0).
indexFind([_|Ts],N,I):- indexFind(Ts,N,J), I is (J+1).
/* to find index of an element in a list starting from 0 indexing */

delete(_,empty,_):- false.
delete(N, BT, BT):- not(lookup(N,BT)).
delete(N,node(N,empty,empty), empty).
delete(N,node(N,node(K,L,R),empty),node(K,L,R)).
delete(N,node(N,empty,node(K,L,R)),node(K,L,R)).
delete(N,node(K,L,R),BST2) :- N>K, delete(N, R ,BST21), BST2 = node(K,L,BST21).
delete(N,node(K,L,R),BST2) :- N<K, delete(N, L ,BST21), BST2 = node(K,BST21,R).
delete(N,node(K,L,R),BST2) :- N==K , inorder(node(K,L,R),LISt), indexFind(LISt,N,In), In1 is (In+1), accesser(LISt,In1,Elem)  ,delete(Elem, R ,BST21), BST2 = node(Elem,L,BST21).
/* for simple leaf, just delete it , for a node with only 1 child, delete the node and put its child at its place, for a node with 2 children we find its inorder successor which would be a leaf, then we delete the leaf adn put it in place of the node to be deleted, hence deleting that node, the third last and second last case essentially ensure that element is properly located, like if the node to be deleted is > current node, we will find it in rightnode only. Also we check that if the node is not in tree at all, we return the same tree. */





eulerTourModify(empty,[],_). 
eulerTourModify(node(N,empty,empty),[(N,I),(N,I),(N,I)],I).
eulerTourModify(node(N,L,R),G,I):- I1 = 2*I,eulerTourModify(L,G1,I1),append([(N,I)],G1,G2) /* Left child ET then put root at start and end then right child ET then root at end    */,append(G2,[(N,I)],G3), I2 is (2*I +1) ,eulerTourModify(R,G4,I2), append(G4,[(N,I)],G5),append(G3,G5,G).
/* modified euler tour which converts each node value to a tuple with second value as its index(fot a BT, it generally starts with 1 for rootnode and 2i +1 for right child and 2i for left child when parent has label i), this would allow us to generate new labels for different copies of the same value occuring in different nodes and help us generate an euler tour which in some sense is distinguishing nodes with similar value so that we can extract the proper preorder inorder or postorder traversal for Btree, Otherwise we can show that for a given euler tour, which is not somehow keeping track of similar valued elements different pre/in/post orders can be formed with same euler traversal */
/* it exactly works on euler tour algo as given above */

tupleExtract((N,_),J) :- J = N. /* helper function to extract first element from tuple */