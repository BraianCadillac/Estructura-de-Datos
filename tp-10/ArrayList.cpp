#include <iostream>
using namespace std;
#include "ArrayList.h"

struct ArrayListSt {
    int cantidad; // cantidad de elementos
    int* elementos; // array de elementos
    int capacidad; // tamaño del array
};
//INV REPR:
// 0 <= cantidad <= capacidad
// capacidad >= 0


ArrayList newArrayList(){
    ArrayListSt* xs = new ArrayListSt;
    xs->cantidad  = 0;
    xs->elementos = new int[16];
    xs->capacidad = 16;
    return xs;
}

ArrayList newArrayListWith(int capacidad){
    if(capacidad < 0){
        return NULL;
    }
    ArrayListSt* xs = new ArrayListSt;
    xs->cantidad  = 0;
    xs->elementos = new int[capacidad];
    xs->capacidad = capacidad;
    return xs;
}

int lengthAL(ArrayList xs){
    return xs->cantidad;
}

int get(int i, ArrayList xs){
// PRECONDICIÓN:
// 0 <= i < cantidad
    return xs->elementos[i];


void set(int i, int x, ArrayList xs){
// PRECONDICIÓN:
// 0 <= i < cantidad
    xs->elementos[i] = x;
}

void resize(int capacidad, ArrayList xs){
    int* nuevo = new int[capacidad];
    int limite = 0;
    if (xs->cantidad > capacidad){
        limite = capacidad;
    }
    else{
        limite = xs->cantidad;
    }
    for(int i = 0; i < limite; i++){
        nuevo[i] = xs->elementos[i];
    }
    delete[] xs->elementos;
    xs->cantidad  = limite;
    xs->elementos = nuevo;
    xs->capacidad = capacidad;
}

void add(int x, ArrayList xs){
    if(xs->cantidad == xs->capacidad){
        int nuevaCapacidad = xs->capacidad * 2;
        int* nuevoArr = new int[nuevaCapacidad];
        for(int i = 0; i < xs->cantidad; i++){
            nuevoArr[i] = xs->elementos[i];
        }
        delete[] xs->elementos;
        xs->elementos = nuevoArr;
        xs->capacidad = nuevaCapacidad;
    }
    xs->elementos[xs->cantidad] = x;
    xs->cantidad++;
}


void remove(ArrayList xs){
    if(xs->cantidad == 0){
        return;
    }
    xs->cantidad--;
}

