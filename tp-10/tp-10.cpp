#include <iostream>
using namespace std;
#include "ArrayList.h"

int sumatoria(ArrayList xs){
    int sumaTotal = 0;
    for(int i = 0; i < lengthAL(xs); i++){
        sumaTotal = sumaTotal + get(i, xs);
    }
    return sumaTotal;
}

void sucesores(ArrayList xs){
    for(int i = 0; i < lengthAL(xs); i++){
        set(i, get(i, xs) + 1, xs);
    }
}

bool pertenece(int x, ArrayList xs){
    for(int i = 0; i < lengthAL(xs); i++){
        if(get(i, xs) == x){
            return true;
        }
    }
    return false;
}

int apariciones(int x, ArrayList xs){
    int cantidad = 0;
    for(int i = 0; i < lengthAL(xs); i++){
        if(get(i, xs) == x){
            cantidad++;
        }
    }
    return cantidad;
}

ArrayList append(ArrayList xs, ArrayList ys){
    ArrayList arr = newArrayListWith(lengthAL(xs) + lengthAL(ys));
    for(int i = 0; i < lengthAL(xs); i++){
        add(get(i, xs), arr);
    }
    for(int i = 0; i < lengthAL(ys); i++){
        add(get(i, ys), arr);
    }
    return arr;
}

int minimo(ArrayList xs){
//PRECOND: El arraylist no está vacío
    int elMinimo = get(0, xs);
    for(int i = 0; i < lengthAL(xs); i++){
        if(get(i, xs) < elMinimo){
            elMinimo = get(i, xs);
        }
    }
    return elMinimo;
}