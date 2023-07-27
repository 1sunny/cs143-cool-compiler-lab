#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"
#include <unordered_map>
#include <vector>

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
public:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;
  std::unordered_map<Symbol, Class_> name2class;
  std::unordered_map<Symbol, int> degree;
  std::unordered_map<Symbol, int> depth;
  std::unordered_map<Symbol, Symbol> class_parent;
  std::unordered_map<Symbol, std::vector<Symbol>> edge;
  std::unordered_map<Symbol, std::unordered_map<Symbol, Feature>> class_methods;
  std::unordered_map<Symbol, std::unordered_map<Symbol, Feature>> class_attrs;

  Classes classes;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

  bool is_subtype(Symbol t1, Symbol t2, Class_ class_);

  Symbol lub(Symbol a, Symbol b, Class_ class_);

  void handle_inheritance(Symbol u, Symbol parent);

  bool is_defined(Symbol type);
};


#endif

