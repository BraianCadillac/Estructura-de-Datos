#include <iostream>
using namespace std;
#include "LinkedList.h"


struct LinkedListSt {
    // INV.REP.: cantidad indica la cantidad de nodos que se pueden recorrer
    // desde primero por siguiente hasta alcanzar a NULL
    // cantidad es 0 sii primero es NULL;
    // cantidad >= 0
    // si el último nodo != NULL, entonces ultimo->siguiente == NULL
    // xs->primero es NULL sii xs->ultimo es NULL
    int cantidad; // cantidad de elementos
    NodoL* primero; // puntero al primer nodo
    NodoL* ultimo;  // puntero al último nodo
};

LinkedList nil(){ //O(1)
    LinkedListSt* xs = new LinkedListSt;
    xs->cantidad = 0;
    xs->primero  = NULL;
    xs->ultimo   = NULL;
    return xs;
}

bool isEmpty(LinkedList xs){ //O(1)
    return(xs->cantidad == 0);
}

int head(LinkedList xs){ //O(1)
    return(xs->primero->elem);
}

void Cons(int x, LinkedList xs){//O(1)
    NodoL* n = new NodoL;
    n->elem = x;
    if(xs->cantidad == 0){
        n->siguiente = NULL;
        xs->primero = n;
        xs->ultimo = n;
    }
    else{
        n->siguiente = xs->primero;
        xs->primero = n;
    }
    xs->cantidad++;
}

void Tail(LinkedList xs){ //O(1)
    if(xs->cantidad > 0){
        NodoL* n = xs->primero;
        xs->primero = xs->primero->siguiente;
        xs->cantidad--;
        delete n;
    }
}

int length(LinkedList xs){ //O(1)
    return xs->cantidad;
}

//void Snoc(int x, LinkedList xs){ //O(n) donde n es la cantidad de elementos de la lista
//    NodoL* n = new NodoL;
//    n->elem = x;
//    n->siguiente = NULL;
//    // Caso lista vacia
//    if(xs->primero == NULL){
//        xs->primero = n;
//    }
//    else{
//        NodoL* temp = xs->primero;
//        while(temp->siguiente != NULL){
//            temp = temp->siguiente;
//        }
//        temp->siguiente = n;
//    }
//    xs->cantidad++;
//}

void Snoc(int x, LinkedList xs){ //O(1)
    NodoL* n = new NodoL;
    n->elem = x;
    n->siguiente = NULL;
    if(xs->cantidad == 0){
        xs->primero = n;
        xs->ultimo = n;
    }
    else{
        xs->ultimo->siguiente = n;
        xs->ultimo = n;
    }
    xs->cantidad++;
}


struct IteratorSt {
    NodoL* current;
};


ListIterator getIterator(LinkedList xs){ //O(1)
    IteratorSt* i = new IteratorSt;
    i->current = xs->primero;
    return i;
}

int current(ListIterator ixs){//O(1)
    return ixs->current->elem;
}

void SetCurrent(int x, ListIterator ixs){ //O(1)
    ixs->current->elem = x;
}

void Next(ListIterator ixs){ //O(1)
    ixs->current = ixs->current->siguiente;
}

bool atEnd(ListIterator ixs){ //O(1)
    return ixs->current == NULL;
}

void DisposeIterator(ListIterator ixs){ //O(1)
    delete ixs;
}

void DestroyL(LinkedList xs){ //O(n) siendo n la cantidad de nodos a recorrer de la lista
    NodoL* temp = xs->primero;
    while(temp != NULL){
        NodoL* siguiente = temp->siguiente;
        delete temp;
        temp = siguiente;
    }
    delete xs;
}

void Append(LinkedList xs, LinkedList ys){ //O(1)
    if(xs->primero == NULL){
        xs->primero = ys->primero;
    }
    else{
        xs->ultimo->siguiente = ys->primero;
    }

    if(ys->ultimo != NULL){
        xs->ultimo = ys->ultimo;
    }
    xs->cantidad += ys->cantidad;
}