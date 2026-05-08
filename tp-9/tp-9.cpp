#include <iostream>
using namespace std;

// Propósito: Describe los carácteres desde c1 hasta c2 incluídos
// Precondición: c1 < c2

void printFromTo(char c1, char c2){
    for(int i = 0; c1 + i <= c2; i++){
        cout << c1 + i << ", ";
    }
    cout << endl;
}

// Propósito: Describe el factorial de n
// Precondición: n >= 0
int fc(int n){
    int x = 1;
    while(n > 0){
        x = x * n;
        n--;
    }
    return x;
}

// Propósito: Describe la suma desde n hasta m
// Precondición: n <= m
int ft(int n, int m) {
    if (n == m){
        return n;
    }
    return n + ft(n+1, m);
}

//se puede mejorar ya que ft es O(X) lineal X en memoria. Podemos mejorar el costo O(1) en memoria con iteración

int ftI(int n, int m){
    int x = 0;
    while (n <= m){
        x = x + n;
        n++;
    }
    return x;
}


//Ejercicio 3

struct Par {
    int x;
    int y;
};

// Propósito: construye un par
Par consPar(int x, int y){
    struct Par p;
    p.x = x;
    p.y = y;
    return p;
}

// Propósito: devuelve la primera componente
int fst(Par p){
    return p.x;
}

// Propósito: devuelve la segunda componente
int snd(Par p){
    return p.y;
}

// Propósito: devuelve la mayor componente
int maxDelPar(Par p){
    if (p.x > p.y){
        return p.x;
    }
    else{
        return p.y;
    }
}

// Propósito: devuelve un par con las componentes intercambiadas
Par swap(Par p){
    struct Par pa;
    pa.x = p.y;
    pa.y = p.x;
    return pa; 
}

// Propósito: devuelve un par donde la primer componente
// es la división y la segunda el resto entre ambos números
Par divisionYResto(int n, int m){
    struct Par p;
    p.x = n/m;
    p.y = n%m;
    return p;
}


//Ejercicio 4

//Propósito: imprime n veces un string s.
//Recursiva
void printN(int n, string s){
    if (n == 0){
        return;
    }
    cout << s << endl;
    printN (n-1, s);
}

void printNI1(int n, string s){
    while (n > 0){
        cout << s << endl;
        n--;
    }
}

void printNI2(int n, string s){
    for(int i = n; i > 0; i--){
        cout << s << endl;
    }
}

//-----------------------------------------------------------------------------------------------

//Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
void cuentaRegresiva(int n){
    if (n < 0){
        return;
    }
    cout << n << endl;
    cuentaRegresiva(n-1);
}

void cuentaRegresivaI1(int n){
    while (n > 0){
        cout << n << endl;
        n--;
    }
}

void cuentaRegresivaI2(int n){
    for(int i=n; i>0; i--){
        cout << i << endl;
    }
}

//------------------------------------------------------------------------------------------------

// Propósito: imprime los números de 0 hasta n
// Precondición: n >= 0
void desdeCeroHastaN(int n){
    if(n < 0){
        return;
    }
    desdeCeroHastaN(n - 1);
    cout << n << endl;
}

void desdeCeroHastaNI1(int n){
    int x = 0;
    while (x <= n){
        cout << x << endl;
        x++;
    }
}

void desdeCeroHastaNI2(int n){
    for(int i=0; i<=n; i++){
        cout << i << endl;
    }
}

//-----------------------------------------------------------------------------------------------

//Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
int mult(int n, int m){
    if (m == 0){
        return 0;
    }
    return n + mult(n, m - 1);
}

int multI1(int n, int m){
    int x = 0;
    int i = 0;
    while(i < m){
        x = x + n;
        i++;
    }
    return x;
}

int multI2(int n, int m){
    int x = 0;
    for(int i = 0; i < m; i++){
        x = x + n;
    }
    return x;
}

//-------------------------------------------------------------------------------------------------

// Propósito: imprime los primeros n char del string s, separados por un salto de línea.
// Precondición: el string tiene al menos n char.
void primerosN(int n, string s){
    if (n > s.length()){
        cout <<"Error, el número dado es mayor que el string dado" << endl;
    }
    else{
        if(n == 0){
            return;
        }
        primerosN(n - 1, s);
        cout << s[n-1] << endl;
    }
}

void primerosNI1(int n, string s){
    if (n > s.length()){
        cout << "Error, el numero dado es mayor a la longitud del string dado" << endl;
    }
    else{
        int x = 0;
        while(x < n){
            cout << s[x] << endl;
            x++;
        }
    } 
}

void primerosNI2(int n, string s){
    if (n > s.length()){
        cout << "Error, el numero dado es mayor a la longitud del string dado" << endl;
    }
    else{
        for(int i = 0; i < n ; i++){
            cout << s[i] << endl;
        }
    }
}

//----------------------------------------------------------------------------------------

// Propósito: indica si un char c aparece en el string s.
bool pertenece(char c, string s){
    if (s.length() == 0){
        return false;
    }
    if (c == s[0]){
        return true;
    }
    return pertenece(c, s.substr(1));
}

bool perteneceI1(char c, string s){
    int i = 0;
    while(i < s.length()){
        if (s[i] == c){
            return true;
        }
        i++;
    }
    return false;
}

bool perteneceI2(char c, string s){
    for(int i = 0; i < s.length(); i++){
        if (s[i] == c){
            return true;
        }
    }
    return false;
}

//---------------------------------------------------------------------------------------------------

//Propósito: devuelve la cantidad de apariciones de un char c en el string s.
int apariciones(char c, string s){
    int n = 0;
    if (s.length() == 0){
        return 0;
    }
    if (s[0] == c){
        n++;
    }
    return n + apariciones(c, s.substr(1));
}

int aparicionesI1(char c, string s){
    int n = 0;
    int i = 0;
    while (i < s.length()){
        if (s[i] == c){
            n++;
        }
        i++;
    }
    return n;
}

int aparicionesI2(char c, string s){
    int n = 0;
    for(int i = 0; i < s.length(); i++){
        if(s[i] == c){
            n++;
        }
    }
    return n;
}

//-----------------------------------------------------------------------------------------------------

struct Fraccion {
    int numerador;
    int denominador;
};

// Propósito: construye una fraccion
// Precondición: el denominador no es cero
Fraccion consFraccion(int numerador, int denominador){
    struct Fraccion f;
    f.numerador   = numerador;
    f.denominador = denominador;
    return f;
}

// Propósito: devuelve el numerador
int numerador(Fraccion f){
    return f.numerador;
}

// Propósito: devuelve el denominador
int denominador(Fraccion f){
    return f.denominador;
}

// Propósito: devuelve el resultado de hacer la división
float division(Fraccion f){
    return ((float) f.numerador / f.denominador);
}

// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// (sin simplificar)
Fraccion multF(Fraccion f1, Fraccion f2){
    struct Fraccion f;
    f.numerador   = f1.numerador*f2.numerador;
    f.denominador = f1.denominador*f2.denominador;
    return f;
}

// Propósito: devuelve una fracción que resulta
// de simplificar la dada por parámetro
int md(int a, int b){
    if(b == 0){
        return a;
    }

    return md(b, a % b);
}

// Propósito: devuelve una fracción simplificada
Fraccion simplificada(Fraccion p){
    Fraccion f;

    int d = md(p.numerador, p.denominador);

    f.numerador = p.numerador / d;
    f.denominador = p.denominador / d;

    return f;
}

// Propósito: devuelve la fracción resultante de sumar las fracciones
Fraccion sumF(Fraccion f1, Fraccion f2){
    struct Fraccion f;
    f.numerador = (f1.numerador*f2.denominador) + (f1.denominador*f2.numerador);
    f.denominador = f1.denominador * f2.denominador;
    return f;
}


int main (){
    Fraccion fracc1 = consFraccion(7, 5);
    Fraccion fracc2 = consFraccion(9,21);
    Fraccion simp   = simplificada(fracc2);
    cout << "Numerador:" << simp.numerador << endl;
    cout << "Denominador:" << simp.denominador << endl;
}

