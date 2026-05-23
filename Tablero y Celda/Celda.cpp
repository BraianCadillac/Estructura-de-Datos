#include <iostream>
using namespace std;
#include "Celda.h"

struct CeldaSt {
    int azul;
    int negro;
    int rojo;
    int verde;
};

//INV.REPR: 
// *c->azul >= 0  
// *c->negro >= 0 
// *c->rojo >= 0 
// *c->verde >= 0

Celda celdaVacia(){
    CeldaSt* c = new CeldaSt;
    c->azul  = 0;
    c->negro = 0;
    c->rojo  = 0;
    c->verde = 0;
    return c;
}

void BorrarCelda(Celda c){
    delete c;
}


void PonerEnCelda(Celda c, Color color){
    if(color == Azul){
        c->azul++;
    }
    if(color == Negro){
        c->negro++;
    }
    if(color == Rojo){
        c->rojo++;
    }
    if(color == Verde){
        c->verde++;
    }
}

void SacarEnCelda(Celda c, Color color){
//PRECOND: Hay al menos una bolita del color dado en la celda actual
    if(color == Azul && c->azul > 0){
        c->azul--;
    }
    if(color == Negro && c->negro > 0){
        c->negro--;
    }
    if(color == Rojo && c->rojo > 0){
        c->rojo--;
    }
    if(color == Verde && c->verde > 0){
        c->verde--;
    }
}

int nroBolitasEnCelda(Celda c, Color color){
    if(color == Azul){
        return c->azul;
    }
    if(color == Negro){
        return c->negro;
    }
    if(color == Rojo){
        return c->rojo;
    }
    if(color == Verde){
        return c->verde;
    }
    return 0;
}

bool hayBolitasEnCelda(Celda c, Color color){
    return nroBolitasEnCelda(c, color) > 0;
}

void ShowCelda(Celda c){
    cout <<"Cantidad de bolitas azul: " << nroBolitasEnCelda(c, Azul) << endl;
    cout <<"Cantidad de bolitas negro: " << nroBolitasEnCelda(c, Negro) << endl;
    cout <<"Cantidad de bolitas rojo: " << nroBolitasEnCelda(c, Rojo) << endl;
    cout <<"Cantidad de bolitas verde: " << nroBolitasEnCelda(c, Verde) << endl;
}