#include <iostream>
using namespace std;
#include "Pokemon.h"


struct PokeSt {
    TipoDePokemon tipo;
    int vida;
};

//INV REPR:
//0 <= vida <= 100

Pokemon consPokemon(TipoDePokemon tipo){
    PokeSt* p = new PokeSt;
    p->tipo = tipo;
    p->vida = 100;
    return p;
}

TipoDePokemon tipoDePokemon(Pokemon p){
    return p->tipo;
}

int energia(Pokemon p){
    return p->vida;
}

void perderEnergia(int energia, Pokemon p){
    if(energia <= p->vida){
        p->vida = p->vida - energia;
    }
    else{
        p->vida = 0;
    }
}

bool superaA(Pokemon p1, Pokemon p2){
    if((p1->tipo) == "agua" && (p2->tipo) == "fuego"){
        return true;
    }
    if((p1->tipo) == "fuego" && (p2->tipo) == "planta"){
        return true;
    }
    if((p1->tipo) == "planta" && (p2->tipo) == "agua"){
        return true;
    }
    return false;
}

void destruirPokemon(Pokemon p){
    delete p;
}

