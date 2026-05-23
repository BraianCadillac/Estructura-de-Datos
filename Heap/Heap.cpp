#include <iostream>
using namespace std;
#include <climits>
#include "Heap.h"

struct BinHeapHeaderSt{
    int maxSize; // INV.REP.: curSize < maxSize
    int curSize;
    int* elems;
};

BinHeap emptyHeap(){
    BinHeapHeaderSt* h = new BinHeapHeaderSt;
    h->maxSize = 16;
    h->curSize = 0;
    h->elems   = new int[h->maxSize];
    h->elems[0] = INT_MIN;
    return h;
}

void InsertH(int x, BinHeap h){
    if(h->maxSize - 1 == h->curSize){
        int* nuevos_elementos = new int[h->maxSize*2];
        for(int i = 0; i < h->maxSize; i++){
            nuevos_elementos[i] = h->elems[i];
        }
        delete[] h->elems;
        h->elems   = nuevos_elementos;
        h->maxSize = h->maxSize * 2;
    }
    int current = ++h->curSize;
    while(x < h->elems[current/2]){
        h->elems[current] = h->elems[current/2];
        current = current/2;
    }
    h->elems[current] = x;
}

bool isEmptyHeap(BinHeap h){
    return h->curSize == 0;
}

int findMin(BinHeap h){
    //PRECOND: Hay al menos un elemento en la heap
    return h->elems[1];
}



void deleteMin(BinHeap h){
    //PRECOND: La heap dada no es vacía
    h->elems[1] = h->elems[h->curSize];
    h->curSize--;

    int current = 1;

    while(current*2 <= h->curSize){
        int izq = current*2;
        int der = current*2 + 1;
        int menor = izq;

        if(der <= h->curSize && h->elems[der] < h->elems[izq]){
            menor = der;
        }

        if(h->elems[current] <= h->elems[menor]){
            break;
        }

        int temp = h->elems[menor];

        h->elems[menor] = h->elems[current]; 
        h->elems[current] = temp;
        
        current = menor;
    }
}

