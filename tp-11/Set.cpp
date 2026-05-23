#include <iostream>
using namespace std;
#include "Set.h"


struct NodoS{
    int elem; // valor del nodo
    NodoS* siguiente; // puntero al siguiente nodo
};

struct SetSt{
    int cantidad; // cantidad de elementos diferentes
    NodoS* primero; // puntero al primer nodo
};
//INV.REPR: 
// No hay elementos repetidos entre nodos
// cantidad >= 0
// cantidad es la cantidad de nodos a recorrer desde primero pasando por siguiente hasta llegar a NULL

Set emptyS(){ //O(1)
    SetSt* s = new SetSt;
    s->cantidad = 0;
    s->primero  = NULL;
    return s;
}

bool isEmptyS(Set s){ //O(1)
    return s->cantidad == 0;
}

bool belongsS(int x, Set s){ //O(n) donde n es la cantidad de nodos del set
    NodoS* temp = s->primero;
    while(temp != NULL){
        if(temp->elem == x){
            return true;
        }
        temp = temp->siguiente;
    }
    return false;
}

void AddS(int x, Set s){ //O(n) donde n es la cantidad de nodos del set
    NodoS* temp = s->primero;
    if(s->cantidad == 0){
        NodoS* e = new NodoS;
        e->elem = x;
        e->siguiente = NULL;
        s->primero = e;
        s->cantidad++;
    }
    else{
        while(temp != NULL){
            if(temp->elem == x){
                return;
            }
            temp = temp->siguiente;
        }
        NodoS* e = new NodoS;
        e->elem = x;
        e->siguiente = s->primero;
        s->primero = e;
        s->cantidad++;
    }
}


void RemoveS(int x, Set s){
    if(s->cantidad == 0){
        return;
    }
    if(s->primero->elem == x){
        NodoS* temp = s->primero;
        s->primero = temp->siguiente;
        delete temp;
        s->cantidad--;
    }
    NodoS* anterior = s->primero;
    NodoS* actual = s->primero->siguiente;
    while(actual != NULL){
        if(actual->elem == x){
            anterior->siguiente = actual->siguiente;
            delete actual;
            s->cantidad--;
            return;
        }
        anterior = actual;
        actual = actual->siguiente;
    }
}

int sizeS(Set s){ //O(1)
    return s->cantidad;
}

LinkedList setToList(Set s){ //O(n) donde n es la cantidad de nodos del set
    LinkedList xs = nil();
    NodoS* temp = s->primero;
    while(temp != NULL){
        Cons(temp->elem, xs);
        temp = temp->siguiente;
    }
    return xs;
}

void DestroyS(Set s){ //O(n) lineal con respecto a la cantidad de nodos en el set
    NodoS* temp = s->primero;
    while(temp != NULL){
        NodoS* siguiente = temp->siguiente;
        delete temp;
        temp = siguiente;
    }
    delete s;
}