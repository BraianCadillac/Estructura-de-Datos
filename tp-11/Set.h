#include <iostream>
using namespace std;
#include "LinkedList.h"

struct SetSt;

typedef SetSt* Set; //INV.REPR: El puntero no es NULL

Set emptyS();
//Crea un conjunto vacío.

bool isEmptyS(Set s);
//Indica si el conjunto está vacío.

bool belongsS(int x, Set s);
//Indica si el elemento pertenece al conjunto.

void AddS(int x, Set s);
//Agrega un elemento al conjunto.

void RemoveS(int x, Set s);
//Quita un elemento dado.

int sizeS(Set s);
//Devuelve la cantidad de elementos.

LinkedList setToList(Set s);
//Devuelve una lista con los lementos del conjunto.

void DestroyS(Set s);
//Libera la memoria ocupada por el conjunto.