#include <iostream>
using namespace std;
#include "Tablero.h"
#include "Celda.h"

struct TableroSt {
    int maxCol; int maxRow; //Tamaño del tablero
    int curCol; int curRow; //Pos. del cabezal
    Celda* celdas;
};
//INV.REPR:
// *t->maxCol > 0
// *t->maxRow > 0
// *t->curCol < t->maxCol && t->curCol >= 0
// *t->curRow < t->maxRow && t->curRow >= 0

int index(Tablero t, int col, int row) {
    return (row + col * t->maxRow);
}

Tablero tableroNuevo(int columnas, int filas){
    if(columnas <= 0 || filas <= 0){
        return NULL;
    }
	TableroSt* t = new TableroSt;
	t->maxCol    = columnas;
	t->maxRow    = filas;
	t->curCol    = 0;
	t->curRow    = 0;
	t->celdas    = new Celda[columnas*filas];
	for(int f = 0; f < t->maxRow; f++){
		for(int c = 0; c < t->maxCol; c++){
			t->celdas[index(t, c, f)] = celdaVacia();
		}
	}
	return t;
}

void EliminarTablero(Tablero t){
	for(int f = 0; f < t->maxRow; f++){
		for(int c = 0; c < t->maxCol; c++){
			BorrarCelda(t->celdas[index(t, c, f)]);
		}
	}
	delete[] t->celdas;
	delete t;
}

void Poner(Tablero t, Color color){
	PonerEnCelda(t->celdas[index(t, t->curCol, t->curRow)], color);
}

void Sacar(Tablero t, Color color){
	SacarEnCelda(t->celdas[index(t, t->curCol, t->curRow)], color);
}

void Mover(Tablero t, Direccion dir){
	if(dir == Norte && t->curRow < t->maxRow - 1){
		t->curRow++;
	}
	if(dir == Sur && t->curRow > 0){
		t->curRow--;
	}
	if(dir == Este && t->curCol < t->maxCol - 1){
		t->curCol++;
	}
	if(dir == Oeste && t->curCol > 0){
		t->curCol--;
	}
}

int nroBolitas(Tablero t, Color color){
	return nroBolitasEnCelda(t->celdas[index(t, t->curCol, t->curRow)], color);
}

bool hayBolitas(Tablero t, Color color){
    return hayBolitasEnCelda(t->celdas[index(t, t->curCol, t->curRow)], color);
}

bool puedeMover(Tablero t, Direccion dir){
	if(dir == Norte && t->curRow < t->maxRow - 1){
		return true;
	}
	if(dir == Sur && t->curRow > 0){
		return true;
	}
	if(dir == Este && t->curCol < t->maxCol - 1){
		return true;
	}
	if(dir == Oeste && t->curCol > 0){
		return true;
	}
	return false;
}

void ShowTablero(Tablero t){
	cout <<"Tamaño del tablero: " << t->maxCol << "x" << t->maxRow << endl;
	for(int f = 0; f < t->maxRow; f++){
		for(int c = 0; c < t->maxCol; c++){
			cout <<"En la posicion : " << f << "x" << c << endl;
			ShowCelda(t->celdas[index(t, c, f)]);
		}
	}
}
