#include <iostream>
using namespace std;
#include "Tree.h"

struct NodeT{
    int elem;
    NodeT* left;
    NodeT* right;
};

Tree emptyT(){
    return NULL;
}

Tree nodeT(int elem, Tree left, Tree right){
    NodeT* n = new NodeT;
    n->elem = elem;
    n->left = left;
    n->right = right;
    return n;
}

bool isEmptyT(Tree t){
    return t==NULL;
}

int rootT(Tree t){
    //PRECOND: El árbol no está vacío
    return t->elem;
}

Tree left(Tree t){
    return t->left;
}

Tree right(Tree t){
    return t->right;
}