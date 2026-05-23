#include <iostream>
using namespace std;
#include "Heap.h"

BinHeap crearHeap(int* elements, int cant){
    BinHeap fh = emptyHeap();

    for(int i = 0; i < cant; i++){
        InsertH(elements[i], fh);
    }

    return fh;
}

int main(){

    int* e = new int[5];

    e[0] = 4;
    e[1] = 10;
    e[2] = 5;
    e[3] = 2;
    e[4] = 8;

    BinHeap h = crearHeap(e, 5);

    cout << findMin(h) << endl;

    deleteMin(h);

    cout << findMin(h) << endl;

    deleteMin(h);

    cout << findMin(h) << endl;

    delete[] e;
}