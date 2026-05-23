#include <iostream>
using namespace std;
#include "Celda.h"

struct TableroSt;

typedef TableroSt* Tablero;

Tablero tableroNuevo(int columnas, int filas);

void EliminarTablero(Tablero t);

void Poner(Tablero t, Color color);

void Sacar(Tablero t, Color color);

void Mover(Tablero t, Direccion dir);

int nroBolitas(Tablero t, Color color);

bool hayBolitas(Tablero t, Color color);

bool puedeMover(Tablero t, Direccion dir);

void ShowTablero(Tablero t);