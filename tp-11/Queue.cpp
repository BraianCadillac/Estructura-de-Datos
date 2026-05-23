#include <iostream>
using namespace std;
#include "Queue.h"

struct NodoQ {
    int elem; // valor del nodo
    NodoQ* siguiente; // puntero al siguiente nodo
};

struct QueueSt {
    int cantidad; // cantidad de elementos
    NodoQ* primero; // puntero al primer nodo
    NodoQ* ultimo; // puntero al ultimo nodo
};
//INV REPR:
// cantidad >= 0
// cantidad es la cantidad de nodos a recorrer pasando por primero hasta llegar a NULL
// primero es NULL sii ultimo es NULL
// si ultimo != NULL entonces ultimo->siguiente == NULL

Queue emptyQ(){
    QueueSt* q = new QueueSt;
    q->cantidad = 0;
    q->primero = NULL;
    q->ultimo = NULL;
    return q;
}

bool isEmptyQ(Queue q){
    return q->cantidad == 0;
}

int firstQ(Queue q){
    //PRECOND: Hay al menos un elemento en la cola
    return q->primero->elem;
}

void Enqueue(int x, Queue q){
    NodoQ* n = new NodoQ;
    n->elem = x;
    n->siguiente = NULL;
    if(q->cantidad == 0){
        q->primero = n;
        q->ultimo = n;
    }
    else{
        q->ultimo->siguiente = n;
        q->ultimo = n;
    }
    q->cantidad++;
}

void Dequeue(Queue q){
    if(q->cantidad == 1){
        NodoQ* pr = q->primero;
        q->primero = NULL;
        q->ultimo = NULL;
        q->cantidad--;
        delete pr;
    }
    if(q->cantidad > 1){
        NodoQ* pr = q->primero;
        q->primero = q->primero->siguiente;
        q->cantidad--;
        delete pr;
    }
}

int lengthQ(Queue q){
    return q->cantidad;
}

//Anexa q2 al final de q1, liberando la memoria inservible de q2 en el proceso.
//Nota: Si bien se libera memoria de q2, no necesariamente la de sus nodos.
void MergeQ(Queue q1, Queue q2){
    if(q2->cantidad == 0){
        delete q2;
        return;
    }
    if(q1->cantidad == 0){
        q1->primero = q2->primero;
        q1->ultimo = q2->ultimo;
    }
    else{
        q1->ultimo->siguiente = q2->primero;
        q1->ultimo = q2->ultimo;
    }
    q1->cantidad += q2->cantidad;
    delete q2;
}

void DestroyQ(Queue q){ //O(n) lineal con respecto a la cantidad de nodos de la cola
    NodoQ* temp = q->primero;
    while(temp != NULL){
        NodoQ* siguiente = temp->siguiente;
        delete temp;
        temp = siguiente;
    }
    delete q;
}

