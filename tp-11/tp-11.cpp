#include <iostream>
using namespace std;
#include "Set.h"
#include "Tree.h"


//Devuelve la suma de todos los elementos.
int sumatoria(LinkedList xs){ //O(n) donde n es la cantidad de nodos de la lista
    int cantidad = 0;
    ListIterator ixs = getIterator(xs);
    while(!atEnd(ixs)){
        cantidad += current(ixs);
        Next(ixs);
    }
    DisposeIterator(ixs);
    return cantidad;
}

//Incrementa en uno todos los elementos.
void Sucesores(LinkedList xs){ //O(n) donde n es la cantidad de nodos de la lista
    ListIterator ixs = getIterator(xs);
    while(!atEnd(ixs)){
        SetCurrent(current(ixs) + 1, ixs);
        Next(ixs);
    }
    DisposeIterator(ixs);
}

//Indica si el elemento pertenece a la lista.
bool pertenece(int x, LinkedList xs){ //O(n) donde n es la cantidad de nodos de la lista
    ListIterator ixs = getIterator(xs);
    while(!atEnd(ixs)){
        if(current(ixs) == x){
            DisposeIterator(ixs);
            return true;
        }
        Next(ixs);
    }
    DisposeIterator(ixs);
    return false;
}

//Indica la cantidad de elementos iguales a x.
int apariciones(int x, LinkedList xs){ //O(n) donde n es la cantidad de nodos a recorrer de la lista
    int cantidad = 0;
    ListIterator ixs = getIterator(xs);
    while(!atEnd(ixs)){
        if(current(ixs) == x){
            cantidad += 1;
        }
        Next(ixs);
    }
    DisposeIterator(ixs);
    return cantidad;
}

int minimoEntre(int x, int y){
    if(x <= y){
        return x;
    }
    else{
        return y;
    }
}

//Devuelve el elemento más chico de la lista.
int minimo(LinkedList xs){ //O(n) donde n es la cantidad de nodos de la lista
    //PRECOND: Hay al menos un nodo en la lista
    ListIterator ixs = getIterator(xs);
    int minimo = current(ixs);
    while(!atEnd(ixs)){
        minimo = minimoEntre(minimo, current(ixs));
        Next(ixs);
    }
    DisposeIterator(ixs);
    return minimo;
}

//Dada una lista genera otra con los mismos elementos, en el mismo orden.
//Nota: notar que el costo mejoraría si Snoc fuese O(1), ¿cómo podría serlo?
LinkedList copy(LinkedList xs){ //O(n^2), hago snoc por cada nodo de la lista de costo lineal
    ListIterator ixs = getIterator(xs);
    LinkedList ys = nil();
    while(!atEnd(ixs)){
        Snoc(current(ixs), ys);
        Next(ixs);
    }
    DisposeIterator(ixs);
    return ys;
}

//Agrega todos los elementos de la segunda lista al final de los de la primera.
//La segunda lista se destruye.
//Nota: notar que el costo mejoraría si Snoc fuese O(1), ¿cómo podría serlo?
//void Append(LinkedList xs, LinkedList ys){ //O(n^2) donde por cada elemendo del nodo de la lista ys hago una operación lineal O(n) (Snoc)
//    ListIterator iys = getIterator(ys);
//    while(!atEnd(iys)){
//        Snoc(current(iys), xs);
//        Next(iys);
//    }
//    DisposeIterator(iys);
//    DestroyL(ys);
//}

int sumarT(Tree t){
    if(isEmptyT(t)){
        return 0;
    }
    return rootT(t) + (sumarT(left(t))) + (sumarT(right(t)));
}

int sizeT(Tree t){
    if(isEmptyT(t)){
        return 0;
    }
    return 1 + sizeT(left(t)) + sizeT(right(t));
}

bool perteneceT(int e, Tree t){
    if(isEmptyT(t)){
        return false;
    }
    return rootT(t) == e || perteneceT(e, left(t)) || perteneceT(e, right(t));
}

int aparicionesT(int e, Tree t){
    if(rootT(t) == e){
        return 1 + aparicionesT(left(t)) + aparicionesT(right(t));
    }
    else{
        return 0 + aparicionesT(left(t)) + aparicionesT(right(t));
    }
}

int heightT(Tree t){
    return 1 + max (heightT(left(t))) (heightT(right(t)));
}

ArrayList juntarArrays(ArrayList xs, ArrayList ys){
    for(int i = 0; i < lengthAL(ys); i++){
        add(get(i, ys), xs);
    }
    return xs;
}

ArrayList toList(Tree t){
    if(isEmptyT(t)){
        return newArrayList();
    }
    ArrayList izq = toList(left(t));
    ArrayList der = toList(right(t));
    ArrayList arr = juntarArrays(izq, der);
    add(rootT(t), arr);
    return arr;
}

ArrayList leaves(Tree t){
    if(isEmptyT(t)){
        return newArrayList();
    }
    if(isEmptyT(left(t)) && isEmptyT(right(t))){
        ArrayList arr = newArrayList();
        add(rootT(t), arr);
        return arr;
    }
    ArrayList izq = leaves(left(t));
    ArrayList der = leaves(right(t));
    ArrayList al = juntarArrays(izq, der);
    return al;
}

ArrayList levelN(int n, Tree t){
    if(isEmptyT(t)){
        return newArrayList();
    }
    if(n == 0){
        ArrayList arr = newArrayList();
        add(rootT(t), arr);
        return arr;
    }
    ArrayList izq = levelN((n-1), left(t));
    ArrayList der = levelN((n-1), right(t));
    ArrayList al = juntarArrays(izq, der);
    return al;
}

//----------------------------------EJERCICIOS DE PRÁCTICA------------------------------------------
//Devuelve cuántas hojas tiene el árbol.
int cantidadDeHojas(Tree t){
    if(isEmptyT(t)){
        return 0;
    }
    if(isEmptyT(left(t)) && isEmptyT(right(t))){
        return 1;
    }
    int izq = cantidadDeHojas(left(t));
    int der = cantidadDeHojas(right(t));
    return (izq + der);
}

//Cuenta nodos que NO son hojas.
int cantidadDeNodosInternos(Tree t){
    if(isEmptyT(t)){
        return 0;
    }
    if(isEmptyT(left(t)) && isEmptyT(right(t))){
        return 0;
    }
    int izq = cantidadDeNodosInternos(left(t));
    int der = cantidadDeNodosInternos(right(t));
    return (1 + izq + der);
}

//Suma solamente las hojas.
int sumaHojas(Tree t){
    if(isEmptyT(t)){
        return 0;
    }
    if(isEmptyT(left(t)) && isEmptyT(right(t))){
        return(rootT(t));
    }
    int izq = sumaHojas(left(t));
    int der = sumaHojas(right(t));
    return (izq + der);
}

//Devuelve el valor máximo del árbol.
//PRECOND: El arbol no está vacío
int maximoEntre(int x, int y){
    if(x >= y){
        return x;
    }
    else{
        return y;
    }
}

int maximoT(Tree t){
    // PRECOND: el árbol no está vacío

    if(isEmptyT(left(t)) && isEmptyT(right(t))){
        return rootT(t);
    }

    if(isEmptyT(right(t))){
        return maximoEntre(
            rootT(t),
            maximoT(left(t))
        );
    }

    if(isEmptyT(left(t))){
        return maximoEntre(
            rootT(t),
            maximoT(right(t))
        );
    }

    return maximoEntre(
        rootT(t),
        maximoEntre(
            maximoT(left(t)),
            maximoT(right(t))
        )
    );
}

Tree mirrorT(Tree t){
    if(isEmptyT(t)){
        return emptyT();
    }

    Tree izq = mirrorT(right(t));
    Tree der = mirrorT(left(t));

    return nodeT(rootT(t), izq, der);
}

bool todosParesT(Tree t){
    if(isEmptyT(t)){
        return true;
    }
    return
        rootT(t) % 2 == 0
        &&
        todosParesT(left(t))
        &&
        todosParesT(right(t));
}

bool existeHojaT(int x, Tree t){
    if(isEmptyT(t)){
        return false;
    }
    if(isEmptyT(left(t)) && isEmptyT(right(t))){
        return rootT(t) == x;
    }
    return
        existeHojaT(x, left(t))
        ||
        existeHojaT(x, right(t));
}

int sumaNodosInternosT(Tree t){
    if(isEmptyT(left(t)) && isEmptyT(right(t))){
        return 0;
    }
    return(1 + sumaNodosInternosT(left(t)) + sumaNodosInternosT(right(t)));
}

int sumaNodosInternosT(Tree t){
    if(isEmptyT(t)){
        return 0;
    }
    if(isEmptyT(left(t)) && isEmptyT(right(t))){
        return 0;
    }
    return
        rootT(t)
        +
        sumaNodosInternosT(left(t))
        +
        sumaNodosInternosT(right(t));
}

ArrayList unaRamaT(Tree t){
    if(isEmptyT(t)){
        return newArrayList();
    }
    if(isEmptyT(left(t)) && isEmptyT(right(t))){
        ArrayList arr = newArrayList();
        add(rootT(t), arr);
        return arr;
    }
    if(!isEmptyT(left(t))){
        ArrayList arr = unaRamaT(left(t));
        add(rootT(t), arr);
        return arr;
    }
    else{
        ArrayList arr = unaRamaT(right(t));
        add(rootT(t), arr);
        return arr;
    }
}

Tree duplicarT(Tree t){
    if(isEmptyT(t)){
        return emptyT();
    }

    Tree ti = duplicarT(left(t));
    Tree td = duplicarT(right(t));

    return nodeT(rootT(t) * 2, ti, td);
}

//Devuelve cuántos nodos hay en el nivel n.
int cantidadNivelT(int n, Tree t){
    if(isEmptyT(t)){
        return 0;
    }
    if(n == 0){
        return 1;
    }
    int cantidadNivelTI = cantidadNivelT((n-1), left(t));
    int cantidadNivelTD = cantidadNivelT((n-1), right(t));
    return(cantidadNivelTI + cantidadNivelTD);
}

bool mismoTree(Tree t1, Tree t2){
    if(isEmptyT(t1) && isEmptyT(t2)){
        return true;
    }

    if(isEmptyT(t1) || isEmptyT(t2)){
        return false;
    }
    return
        rootT(t1) == rootT(t2)
        &&
        mismoTree(left(t1), left(t2))
        &&
        mismoTree(right(t1), right(t2));
}

int contarParesT(Tree t){
    if(isEmptyT(t)){
        return 0;
    }
    int par = 0;
    if(rootT(t)%2 == 0){
        par = 1;
    }
    int izq = contarParesT(left(t));
    int der = contarParesT(right(t));
    return(par + izq + der);
}

int sumaNivelT(int n, Tree t){
    if(isEmptyT(t)){
        return 0;
    }
    if(n == 0){
        return rootT(t);
    }
    int izq = sumaNivelT(n-1, left(t));
    int der = sumaNivelT(n-1, right(t));
    return(izq+der);
}

bool tieneSoloUnHijoT(Tree t){
    if(isEmptyT(t)){
        return false;
    }
    if(!isEmptyT(left(t)) && isEmptyT(right(t))){
        return true;
    }
    if(isEmptyT(left(t)) && !isEmptyT(right(t))){
        return true;
    }
    return(tieneSoloUnHijoT(left(t)) || tieneSoloUnHijoT(right(t)));
}



int profundidadDeEn(int nivel, int x, Tree t){

    if(isEmptyT(t)){
        return -1;
    }

    if(rootT(t) == x){
        return nivel;
    }

    int izq = profundidadDeEn(nivel + 1, x, left(t));

    if(izq != -1){
        return izq;
    }

    int der = profundidadDeEn(nivel + 1, x, right(t));

    return der;
}

int profundidadDeT(int x, Tree t){
    return profundidadDeEn(0, x, t);
}

ArrayList caminoHastaT(int x, Tree t){

    if(isEmptyT(t)){
        return newArrayList();
    }

    if(rootT(t) == x){
        ArrayList arr = newArrayList();
        add(x, arr);
        return arr;
    }

    ArrayList izq = caminoHastaT(x, left(t));

    if(lengthAL(izq) > 0){
        add(rootT(t), izq);
        return izq;
    }

    ArrayList der = caminoHastaT(x, right(t));

    if(lengthAL(der) > 0){
        add(rootT(t), der);
        return der;
    }

    return newArrayList();
}

bool existeCaminoSumaT(int n, Tree t){
    if(isEmptyT(t)){
        return false;
    }
    if(isEmptyT(left(t)) && isEmptyT(right(t))){
        return n == rootT(t);
    }
    bool izq = existeCaminoSumaT(n - rootT(t), left(t));
    bool der = existeCaminoSumaT(n - rootT(t), right(t));
    return (izq || der);
}

ArrayList caminoMasLargoT(Tree t){
    if(isEmptyT(t)){
        return newArrayList();
    }
    ArrayList ari = caminoMasLargoT(left(t));
    ArrayList ard = caminoMasLargoT(right(t));
    if(lengthAL(ari) >= lengthAL(ard)){
        add(rootT(t), ari);
        return ari;
    }
    if(lengthAL(ard) > lengthAL(ari)){
        add(rootT(t), ard);
        return ard;
    }
}

int sumaDe(ArrayList xs){
    int cantidad = 0;
    for(int i = 0; i < lengthAL(xs); i++){
        cantidad += get(i, xs);
    }
    return cantidad;
}

ArrayList ramaConMayorSumaT(Tree t){
    if(isEmptyT(t)){
        return newArrayList();
    }
    ArrayList ari = ramaConMayorSumaT(left(t));
    ArrayList ard = ramaConMayorSumaT(right(t));
    if(sumaDe(ari) >= sumaDe(ard)){
        add(rootT(t), ari);
        return ari;
    }
    else{
        add(rootT(t), ard);
        return ard;
    }
}



Tree podarHastaT(int nivelActual, int n, Tree t){
    if(isEmptyT(t)){
        return emptyT();
    }
    if(nivelActual > n){
        return emptyT();
    }
    Tree izq = podarHastaT(nivelActual + 1, n, left(t));
    Tree der = podarHastaT(nivelActual + 1, n, right(t));
    return nodeT(rootT(t), izq, der);
}

Tree podarT(int n, Tree t){
    return podarHastaT(0, n, t);
}

Tree eliminarHojasT(Tree t){
    if(isEmptyT(t)){
        return emptyT();
    }
    if(isEmptyT(left(t)) && isEmptyT(right(t))){
        return emptyT();
    }
    Tree izq = eliminarHojasT(left(t));
    Tree der = eliminarHojasT(right(t));
    return nodeT(rootT(t), izq, der);
}

int alturaDelArbol(Tree t){
    if(isEmptyT(t)){
        return 0;
    }
    int izq = alturaDelArbol(left(t));
    int der = alturaDelArbol(right(t));
    return (1 + max (izq, der))
}

bool esBalanceadoT(Tree t){
    if(isEmptyT(t)){
        return true;
    }
    int alturaI = alturaDelArbol(left(t));
    int alturaD = alturaDelArbol(right(t));
    bool actual = (abs(alturaI - alturaD) <= 1);
    return (actual && esBalanceadoT(left(t)) && esBalanceadoT(right(t)));
}


Tree recortarMenoresT(int x, Tree t){

    // PRECOND: t es BST

    if(isEmptyT(t)){
        return emptyT();
    }

    if(rootT(t) < x){
        return recortarMenoresT(x, right(t));
    }

    Tree ti = recortarMenoresT(x,left(t));

    Tree td = recortarMenoresT(x, right(t));

    return nodeT(rootT(t), ti, td);
}

bool esHeapT(Tree t){
    if(isEmptyT(t)){
        return true;
    }
    bool esMayorIzq = true;
    if(!isEmptyT(left(t))){
        esMayorIzq = rootT(t) > rootT(left(t));
    }
    bool esMayorDer = true;
    if(!isEmptyT(right(t))){
        esMayorDer = rootT(t) > rootT(right(t));
    }
    return(esMayorIzq && esMayorDer && esHeapT(left(t)) && esHeapT(right(t)));
}

bool esCaminoCrecienteT(Tree t){
    if(isEmptyT(t)){
        return true;
    }
    bool esCaminoCrecienteIzq = true;
    bool esCaminoCrecienteDer = true;
    if(!isEmptyT(left(t))){
        esCaminoCrecienteIzq = rootT(t) <= rootT(left(t));
    }
    if(!isEmptyT(right(t))){
        esCaminoCrecienteDer = rootT(t) <= rootT(right(t));
    }
    return(esCaminoCrecienteIzq && esCaminoCrecienteDer && esCaminoCrecienteT(left(t)) && esCaminoCrecienteT(right(t)));
}


int alturaDeElementoT(int x, Tree t){
    if(isEmptyT(t)){
        return -1;
    }
    
    if(rootT(t) == x){
        return alturaDelArbol(t);
    }

    int izq = alturaDeElementoT(x, left(t));

    if(izq != -1){
        return izq;
    }
    else{
        int der = alturaDeElementoT(x, right(t));
        return der;
    }

}

Tree reemplazarHojasT(int x, Tree t){
    if(isEmptyT(t)){
        return emptyT();
    }
    if(isEmptyT(left(t)) && isEmptyT(right(t))){
        return nodeT(x, left(t), right(t));
    }
    Tree izq = reemplazarHojasT(x, left(t));
    Tree der = reemplazarHojasT(x, right(t));
    return nodeT(rootT(t), izq, der);
}

Tree duplicarHojasT(Tree t){
    if(isEmptyT(t)){
        return emptyT();
    }
    if(isEmptyT(left(t)) && isEmptyT(right(t))){
        Tree izq = nodeT(rootT(t), emptyT(), emptyT());
        Tree der = nodeT(rootT(t), emptyT(), emptyT());
        return nodeT(rootT(t), izq, der);
    }
    Tree ti = duplicarHojasT(left(t));
    Tree td = duplicarHojasT(right(t));
    return nodeT(rootT(t), ti, td);
}
int sumarT(Tree t)
Tree sumarSubarbolesT(Tree t){
    if(isEmptyT(t)){
        return emptyT();
    }
    int izq = sumarT(left(t));
    int der = sumarT(right(t));
    int n   = rootT(t) + izq + der;
    return nodeT(n, sumarSubarbolesT(left(t)), sumarSubarbolesT(right(t)));
}

//------------------------- RECORRIDO A LO ANCHO (BFS) -------------------------------

int sumarT(Tree t){
    if(isEmptyT(t)){
        return 0;
    }
    Queue q = emptyQ();

    Enqueue(t, q);

    int suma = 0;

    while(!isEmptyQ q){
        Tree actual = firstQ(q);
        dequeue(q);

        suma += rootT(actual);

        if(!isEmptyT(left(actual))){
            Enqueue(left(actual), q);
        }
        if(!isEmptyT(right(actual))){
            Enqueue(right(actual), q);
        }
    }
    return suma;
}

int sizeT(Tree t){
    if(isEmptyT(t)){
        return 0;
    }
    Queue q = emptyQ();

    Enqueue(t, q);
    int size = 0;
    while(!isEmptyQ(q)){
        Tree actual = firstQ(q);
        dequeue(q);

        size++;

        if(!isEmptyT(left(actual))){
            Enqueue(left(actual), q);
        }
        if(!isEmptyT(right(actual))){
            Enqueue(right(actual), q);
        }
    }
    return size;
}


bool perteneceT(int e, Tree t){
    if(isEmptyT(t)){
        return false;
    }

    Queue q = emptyQ();
    Enqueue(t, q);

    while(!isEmptyQ(q)){
        Tree actual = firstQ(q);
        Dequeue(q);

        if(rootT(actual) == e){
            return true;
        }
        if(!isEmptyT(left(actual))){
            Enqueue(left(actual), q);
        }
        if(!isEmptyT(right(actual))){
            Enqueue(right(actual), q);
        }
    }
    return false;
}


int aparicionesT(int e, Tree t){
    if(isEmptyT(t)){
        return 0;
    }
    Queue q = emptyQ();
    Enqueue(t, q);
    int apariciones = 0;
    while(!isEmptyQ(q)){
        Tree actual = firstQ(q);
        Dequeue(q);
        if(rootT(actual) == e){
            apariciones++;
        }
        if(!isEmptyT(left(actual))){
            Enqueue(left(actual), q);
        }
        if(!isEmptyT(right(actual))){
            Enqueue(right(actual), q);
        }
    }
    return apariciones;
}

ArrayList toList(Tree t){
    if(isEmptyT(t)){
        return newArrayList();
    }
    Queue q = emptyQ();
    Enqueue(t, q);
    ArrayList lista = newArrayList();
    while(!isEmptyQ(q)){
        Tree actual = firstQ(q);
        Dequeue(q);
        add(rootT(actual), lista);

        if(!isEmptyT(left(actual))){
            Enqueue(left(actual), q);
        }
        if(!isEmptyT(right(actual))){
            Enqueue(right(actual), q);
        }
    }
    return lista;
}

BinHeap crearHeap(int* elements, int cant){
    BinHeap fh = emptyHeap();

    for(int i = 0; i < cant; i++){
        InsertH(elements[i], fh);
    }

    return fh;
}