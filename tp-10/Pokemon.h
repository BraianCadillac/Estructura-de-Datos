#include <iostream>
using namespace std;


typedef string TipoDePokemon;

struct PokeSt;

typedef PokeSt* Pokemon;

Pokemon consPokemon(TipoDePokemon tipo);
//Dado un tipo devuelve un pokémon con 100% de energia.

TipoDePokemon tipoDePokemon(Pokemon p);
//Devuelve el tipo de un pokemon.

int energia(Pokemon p);
//Devuelve el porcentaje de energía.

void perderEnergia(int energia, Pokemon p);
//Le resta energía al pokemon.

bool superaA(Pokemon p1, Pokemon p2);
//Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.

void destruirPokemon(Pokemon p);
// Libera la memoria ocupada por el pokemon