#include <iostream>
using namespace std;
#include "Entrenador.h"
#include "Pokemon.h"

struct EntrenadorSt {
    string nombre;
    Pokemon* pokemon;
    int cantPokemon;
};

//INV REPR:
// - nombre no es vacío
// - cantPokemon >= 0

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon){
    if(nombre == ""){
        return NULL;
    }
    if(cantidad < 0){
        return NULL;
    }
    EntrenadorSt* e = new EntrenadorSt;
    e->nombre       = nombre;
    e->pokemon      = pokemon;
    e->cantPokemon  = cantidad;
    return e;
}

string nombreDeEntrenador(Entrenador e){
    return e->nombre;
}

int cantidadDePokemon(Entrenador e){
    return e->cantPokemon;
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e){
    int cantidad = 0;
    Pokemon* p   = e->pokemon;
    for(int i = 0; i < e->cantPokemon; i++){
        if (tipoDePokemon (p[i]) == tipo){
            cantidad++;
        }
    }
    return cantidad;
}

Pokemon pokemonNro(int i, Entrenador e){
    if (i > 0 && i <= e->cantPokemon){
        Pokemon* p = e->pokemon;
        return p[i-1];
    }
    return NULL;
}

bool leGanaATodos(Entrenador e1, Entrenador e2){
    Pokemon* p1 = e1->pokemon;
    Pokemon* p2 = e2->pokemon;
    for(int i = 0; i < e2->cantPokemon; i++){
        int ganador = 0;
        for(int j = 0; j < e1->cantPokemon; j++){
            if(superaA (p1[j], p2[i])){
                ganador = 1;
            }
        }
        if(ganador == 0){
            return false;
        }
    }
    return true;
}

void destruirEntrenador(Entrenador e){
    for(int i = 0; i < e->cantPokemon; i++){
        destruirPokemon(e->pokemon[i]);
    }
    delete[] e->pokemon;
    delete e;
}