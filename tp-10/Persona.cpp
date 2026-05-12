#include <iostream>
using namespace std;
#include "Persona.h"

struct PersonaSt {
    string nombre;
    int edad;
};
//INV REPR: 
// - edad >= 0
// - nombre no es vacío

Persona consPersona(string nombre, int edad){
    if (edad < 0){
        return NULL;
    }
    if (nombre == ""){
        return NULL;
    }

    PersonaSt* p = new PersonaSt;
    p->nombre = nombre;
    p->edad   = edad;
    return p; 
}

string nombre(Persona p){
    return(p->nombre);
}

int edad(Persona p){
    return p->edad;
}

void crecer(Persona p){
    p->edad++;
}

void cambioDeNombre(string nombre, Persona p){
    if (nombre == ""){
        return;
    }
    p->nombre = nombre;
}

bool esMayorQueLaOtra(Persona p1, Persona p2){
    return((p1->edad) > (p2->edad));
}

Persona laQueEsMayor(Persona p1, Persona p2){
    if((p1->edad) > (p2->edad)){
        return p1;
    }
    else{
        return p2;
    }
}

void destruirPersona(Persona p){
    delete p;
}