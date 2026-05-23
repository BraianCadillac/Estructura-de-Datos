#include <iostream>
using namespace std;

typedef int Color;
#define Azul  0
#define Negro 1
#define Rojo  2
#define Verde 3

typedef int Direccion;
#define Norte 0
#define Este  1
#define Sur   2
#define Oeste 3

struct CeldaSt;

typedef CeldaSt* Celda;

Celda celdaVacia();

void BorrarCelda(Celda c);

void PonerEnCelda(Celda c, Color color);

void SacarEnCelda(Celda c, Color color);

int nroBolitasEnCelda(Celda c, Color color);

bool hayBolitasEnCelda(Celda c, Color color);

void ShowCelda(Celda c);