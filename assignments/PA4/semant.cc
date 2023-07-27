

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <queue>
#include <algorithm>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol
        arg,
        arg2,
        Bool,
        concat,
        cool_abort,
        copy,
        Int,
        in_int,
        in_string,
        IO,
        length,
        Main,
        main_meth,
        No_class,
        No_type,
        Object,
        out_int,
        out_string,
        prim_slot,
        self,
        SELF_TYPE,
        Str,
        str_field,
        substr,
        type_name,
        val,
        Method,
        Attr;

//
// Initializing the predefined symbols.
//
static void initialize_constants(void) {
  arg = idtable.add_string("arg");
  arg2 = idtable.add_string("arg2");
  Bool = idtable.add_string("Bool");
  concat = idtable.add_string("concat");
  cool_abort = idtable.add_string("abort");
  copy = idtable.add_string("copy");
  Int = idtable.add_string("Int");
  in_int = idtable.add_string("in_int");
  in_string = idtable.add_string("in_string");
  IO = idtable.add_string("IO");
  length = idtable.add_string("length");
  Main = idtable.add_string("Main");
  main_meth = idtable.add_string("main");
  //   _no_class is a symbol that can't be the name of any
  //   user-defined class.
  No_class = idtable.add_string("_no_class");
  No_type = idtable.add_string("_no_type");
  Object = idtable.add_string("Object");
  out_int = idtable.add_string("out_int");
  out_string = idtable.add_string("out_string");
  prim_slot = idtable.add_string("_prim_slot");
  self = idtable.add_string("self");
  SELF_TYPE = idtable.add_string("SELF_TYPE");
  Str = idtable.add_string("String");
  str_field = idtable.add_string("_str_field");
  substr = idtable.add_string("substr");
  type_name = idtable.add_string("type_name");
  val = idtable.add_string("_val");
  Method = idtable.add_string("Method");
  Attr = idtable.add_string("Attr");
}

bool is_basic_class(Symbol name) {
  return name == Object || name == IO || name == Int || name == Str || name == Bool || name == SELF_TYPE;
}

ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr) {

  /* Fill this in */
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    Class_ class_ = classes->nth(i);
    Symbol name = class_->get_name();
    // check redefinition of basic class
    if (is_basic_class(name)) {
      semant_error(class_) << "Redefinition of basic class " << name << ".\n";
    }
    // check inherit of basic class
    Symbol parent = class_->get_parent();
    if (parent == Int || parent == Str || parent == Bool || parent == SELF_TYPE) {
      semant_error(class_) << "Class " << name << " cannot inherit class " << parent << ".\n";
    }
  }
  // this->classes = classes->copy_list(); // line number loss
  this->classes = classes;

  install_basic_classes();

  for (int i = this->classes->first(); this->classes->more(i); i = this->classes->next(i)) {
    Class_ class_ = this->classes->nth(i);
    Symbol name = class_->get_name();
    // add to class map
    if (name2class.count(name) && !is_basic_class(name)) {
      semant_error(class_) << "Class " << name << " was previously defined.\n";
    }
    name2class[name] = class_;
  }
  if (errors() == 0) {
    // build graph
    for (auto p: name2class) {
      Symbol name = p.first;
      if (name == Object) {
        continue;
      }
      Symbol parent = p.second->get_parent();
      class_parent[name] = parent;
      if (name2class.count(parent)) {
        edge[parent].push_back(name);
        degree[name]++;
      } else {
        semant_error(p.second) << "Class " << name << " inherits from an undefined class " << parent << ".\n";
      }
    }
    for (auto p: name2class) {
      Symbol name = p.first;
      std::reverse(edge[name].begin(), edge[name].end());
    }
  }
  if (errors() == 0) {
    // topological sort
    std::queue<Symbol> que;
    std::unordered_map<Symbol, bool> visit;
    for (auto p: name2class) {
      Symbol name = p.first;
      if (degree[name] == 0) {
        depth[name] = 1;
        que.push(name);
      }
    }
    while (!que.empty()) {
      Symbol u = que.front();
      visit[u] = true;
      que.pop();
      for (Symbol v: edge[u]) {
        if (--degree[v] == 0) {
          depth[v] = depth[u] + 1;
          que.push(v);
        }
      }
    }
    // non-visit
    for (auto p: name2class) {
      Symbol name = p.first;
      if (!visit[name]) {
        semant_error(p.second) << "Class " << name << ", or an ancestor of " << name
                               << ", is involved in an inheritance cycle.\n";
      }
    }
  }
}

void ClassTable::install_basic_classes() {

  // The tree package uses these globals to annotate the classes built below.
  // curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

  // The following demonstrates how to create dummy parse trees to
  // refer to basic Cool classes.  There's no need for method
  // bodies -- these are already built into the runtime system.

  // IMPORTANT: The results of the following expressions are
  // stored in local variables.  You will want to do something
  // with those variables at the end of this method to make this
  // code meaningful.

  //
  // The Object class has no parent class. Its methods are
  //        abort() : Object    aborts the program
  //        type_name() : Str   returns a string representation of class name
  //        copy() : SELF_TYPE  returns a copy of the object
  //
  // There is no need for method bodies in the basic classes---these
  // are already built in to the runtime system.

  Class_ Object_class =
          class_(Object,
                 No_class,
                 append_Features(
                         append_Features(
                                 single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                                 single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                         single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
                 filename);

  classes = cons(Object_class, classes);
  //
  // The IO class inherits from Object. Its methods are
  //        out_string(Str) : SELF_TYPE       writes a string to the output
  //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
  //        in_string() : Str                 reads a string from the input
  //        in_int() : Int                      "   an int     "  "     "
  //
  Class_ IO_class =
          class_(IO,
                 Object,
                 append_Features(
                         append_Features(
                                 append_Features(
                                         single_Features(method(out_string, single_Formals(formal(arg, Str)),
                                                                SELF_TYPE, no_expr())),
                                         single_Features(method(out_int, single_Formals(formal(arg, Int)),
                                                                SELF_TYPE, no_expr()))),
                                 single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                         single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
                 filename);

  classes = cons(IO_class, classes);
  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer.
  //
  Class_ Int_class =
          class_(Int,
                 Object,
                 single_Features(attr(val, prim_slot, no_expr())),
                 filename);
  classes = cons(Int_class, classes);

  //
  // Bool also has only the "val" slot.
  //
  Class_ Bool_class =
          class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename);
  classes = cons(Bool_class, classes);

  //
  // The class Str has a number of slots and operations:
  //       val                                  the length of the string
  //       str_field                            the string itself
  //       length() : Int                       returns length of the string
  //       concat(arg: Str) : Str               performs string concatenation
  //       substr(arg: Int, arg2: Int): Str     substring selection
  //
  Class_ Str_class =
          class_(Str,
                 Object,
                 append_Features(
                         append_Features(
                                 append_Features(
                                         append_Features(
                                                 single_Features(attr(val, Int, no_expr())),
                                                 single_Features(attr(str_field, prim_slot, no_expr()))),
                                         single_Features(method(length, nil_Formals(), Int, no_expr()))),
                                 single_Features(method(concat,
                                                        single_Formals(formal(arg, Str)),
                                                        Str,
                                                        no_expr()))),
                         single_Features(method(substr,
                                                append_Formals(single_Formals(formal(arg, Int)),
                                                               single_Formals(formal(arg2, Int))),
                                                Str,
                                                no_expr()))),
                 filename);
  classes = cons(Str_class, classes);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream &ClassTable::semant_error(Class_ c) {
  return semant_error(c->get_filename(), c);
}

ostream &ClassTable::semant_error(Symbol filename, tree_node *t) {
  error_stream << filename << ":" << t->get_line_number() << ": ";
  return semant_error();
}

ostream &ClassTable::semant_error() {
  semant_errors++;
  return error_stream;
}


/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant() {
  initialize_constants();

  /* ClassTable constructor may do some semantic analysis */
  ClassTable *classtable = new ClassTable(classes);

  /* some semantic analysis code may go here */
  if (classtable->errors() == 0) {
    classtable->handle_inheritance(Object, No_class);
    SymbolTable<Symbol, Entry> *O = new SymbolTable<Symbol, Entry>();
    type_check(O, classtable);
  }

  if (classtable->errors()) {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
  }
}

void ClassTable::handle_inheritance(Symbol u, Symbol parent) {
  Class_ class_ = name2class[u];
  Features features = class_->get_features();

  for (int i = features->first(); features->more(i); i = features->next(i)) {
    bool add_fea = true;
    Feature cur_fea = features->nth(i);
    Symbol cur_fea_type = cur_fea->get_feature_type();
    Symbol cur_fea_name = cur_fea->get_name();

    if (u != Object) {
      if (cur_fea_type == Method && class_methods[parent].count(cur_fea_name)) {
        Feature par_fea = class_methods[parent][cur_fea_name];
        Formals par_formals = par_fea->get_formals();
        Formals cur_formals = cur_fea->get_formals();

        // check that the number of formal is consistent
        if (par_formals->len() != cur_formals->len()) {
          semant_error(class_->get_filename(), cur_fea)
                  << "Incompatible number of formal parameters in redefined method " << cur_fea_name << ".\n";
          add_fea = false;
        } else {
          // check that the formal type is consistent
          for (int j = par_formals->first(); par_formals->more(j); j = par_formals->next(j)) {
            Symbol par_type = par_formals->nth(j)->get_type();
            Symbol cur_type = cur_formals->nth(j)->get_type();

            if (par_type != cur_type) {
              semant_error(class_->get_filename(), cur_fea)
                      << "In redefined method " << cur_fea_name << ", parameter type " << cur_type
                      << " is different from original type " << par_type << "\n";
              add_fea = false;
            }
          }
          // check that the return type is consistent
          if (par_fea->get_return_type() != cur_fea->get_return_type()) {
            semant_error(class_->get_filename(), cur_fea)
                    << "In redefined method " << cur_fea_name << ", return type " << cur_fea->get_return_type()
                    << " is different from original return type " << par_fea->get_return_type() << ".\n";
            add_fea = false;
          }
        }
      } else if (cur_fea_type == Attr) {
        if (class_attrs[parent].count(cur_fea_name)) {
          semant_error(class_->get_filename(), cur_fea)
                  << "Attribute " << cur_fea_name << " is an attribute of an inherited class.\n";
          add_fea = false;
        }
      }
    }
    if (add_fea) {
      if (cur_fea_type == Method) {
        // already defined this method
        if (class_methods[u].count(cur_fea_name)) {
          semant_error(class_->get_filename(), cur_fea)
                  << "Method " << cur_fea_name << " is multiply defined.\n";
        } else {
          class_methods[u][cur_fea_name] = cur_fea;
        }
      } else if (cur_fea_type == Attr) {
        if (class_attrs[u].count(cur_fea_name)) {
          semant_error(class_->get_filename(), cur_fea)
                  << "Attribute " << cur_fea_name << " is multiply defined.\n";
        } else {
          class_attrs[u][cur_fea_name] = cur_fea;
        }
      } else {
        fatal_error("Unknown type");
      }
    }
  }
  // add parent feature that child don't have
  if (u != Object) {
    for (auto p: class_methods[parent]) {
      if (class_methods[u].count(p.first) == false) {
        class_methods[u].insert(p);
      }
    }
    for (auto p: class_attrs[parent]) {
      if (class_attrs[u].count(p.first) == false) {
        class_attrs[u].insert(p);
      }
    }
  }
  // run a deap first search
  for (Symbol v: edge[u]) {
    handle_inheritance(v, u);
  }
}

void
program_class::type_check(SymbolTable<Symbol, Entry> *O, ClassTable *classtable) {
  std::unordered_map<Symbol, bool> defined;
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    Class_ class_ = classes->nth(i);
    class_->type_check(O, classtable);
    defined[class_->get_name()] = true;
  }
  if (defined.count(Main) == false) {
    classtable->semant_error()
            << "Class Main is not defined.\n";
  }
}

Symbol class__class::get_name() {
  return name;
}

Symbol class__class::get_parent() {
  return parent;
}

Features class__class::get_features() {
  return features;
}

void class__class::type_check(SymbolTable<Symbol, Entry> *O, ClassTable *classtable) {
  O->enterscope();
  // add attrs to O
  for (auto p: classtable->class_attrs[name]) {
    O->addid(p.first, p.second->get_type_decl());
  }

  bool main_defined = false;
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    Feature feature = features->nth(i);
    feature->type_check(O, this, classtable);
    if (feature->get_name() == main_meth) {
      main_defined = true;
    }
  }

  if (name == Main && !main_defined) {
    classtable->semant_error(get_filename(), this)
            << "No 'main' method in class Main.\n";
  }
  O->exitscope();
}

bool formal_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_, ClassTable *classtable) {
  if (name == self) {
    classtable->semant_error(class_->get_filename(), this)
            << "'self' cannot be the name of a formal parameter.\n";
  }

  if (type_decl == SELF_TYPE) {
    classtable->semant_error(class_->get_filename(), this)
            << "Formal parameter " << name << " cannot have type SELF_TYPE.\n";
  } else if (classtable->name2class.count(type_decl) == false) {
    classtable->semant_error(class_->get_filename(), this)
            << "Class " << type_decl << " of formal parameter " << name << " is undefined.\n";
  }
  return true;
}

Symbol method_class::get_feature_type() {
  return Method;
}

Symbol method_class::get_name() {
  return name;
}

Formals method_class::get_formals() {
  return formals;
}

Symbol method_class::get_return_type() {
  return return_type;
}

Symbol method_class::get_type_decl() {
  fatal_error("method_class::get_type_decl");
  return No_type;
}

bool method_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                              ClassTable *classtable) {
  O->enterscope();
  O->addid(self, SELF_TYPE);
  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    Formal formal_i = formals->nth(i);
    formal_i->type_check(O, class_, classtable);

    bool defined = false;
    for (int j = formals->first(); j < i; j = formals->next(j)) {
      Symbol formal_name = formals->nth(j)->get_name();
      if (formal_name == formal_i->get_name()) {
        classtable->semant_error(class_->get_filename(), this)
                << "Formal parameter " << formal_i->get_name() << " is multiply defined.\n";
        defined = true;
        break;
      }
    }
    if (!defined) {
      // chose the former defined
      O->addid(formal_i->get_name(), formal_i->get_type());
    }
  }

  bool return_type_ok = true;
  if (classtable->is_defined(return_type) == false) {
    return_type_ok = false;
    classtable->semant_error(class_->get_filename(), this)
            << "Undefined return type " << return_type << " in method " << name << ".\n";
  }

  Symbol expr_type = expr->type_check(O, class_, classtable);
  assert(classtable->is_defined(expr_type));

  // check subtype when return_type is correct
  if (return_type_ok && classtable->is_subtype(expr_type, return_type, class_) == false) {
    classtable->semant_error(class_->get_filename(), this)
            << "Inferred return type " << expr_type << " of method " << name
            << " does not conform to declared return type " << return_type << ".\n";
  }
  O->exitscope();
  return true;
}

Symbol attr_class::get_feature_type() {
  return Attr;
}

Symbol attr_class::get_name() {
  return name;
}

Formals attr_class::get_formals() {
  fatal_error("attr_class::get_formals");
  return NULL;
}

Symbol attr_class::get_return_type() {
  fatal_error("attr_class::get_return_type");
  return No_type;
}

Symbol attr_class::get_type_decl() {
  return type_decl;
}

bool attr_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                            ClassTable *classtable) {
  O->enterscope();
  O->addid(self, SELF_TYPE);
  if (name == self) {
    classtable->semant_error(class_->get_filename(), this)
            << "'self' cannot be the name of an attribute.\n";
  }

  Symbol T0 = type_decl;
  bool T0_ok = true;
  if (classtable->is_defined(T0) == false) {
    T0_ok = false;
    classtable->semant_error(class_->get_filename(), this)
            << "Class " << T0 << " of attribute " << name << " is undefined.\n";
  }

  Symbol T1 = init->type_check(O, class_, classtable);
  assert(T1 == No_type || classtable->is_defined(T1));

  if (T1 != No_type) {
    // check T1 <= T0 when T0 and T1 is correct
    if (T0_ok && classtable->is_subtype(T1, T0, class_) == false) {
      classtable->semant_error(class_->get_filename(), this)
              << "Inferred type " << T1 << " of initialization of attribute " << name
              << " does not conform to declared type " << T0 << ".\n";
    }
  }
  O->exitscope();
  return true;
}

bool ClassTable::is_subtype(Symbol t1, Symbol t2, Class_ class_) {
  assert(t1 && t2 && class_);
  if (t2 == SELF_TYPE) {
    // 1. SELF_TYPE_C <= SELF_TYPE_C
    // 3. T <= SELF_TYPE_C always false
    return t1 == SELF_TYPE;
  }
  // 2. SELF_TYPE_C <= T if C <= T
  if (t1 == SELF_TYPE) {
    t1 = class_->get_name();
  }
  if (t1 == t2 || t2 == Object) {
    return true;
  }
  assert(name2class.count(t1) && name2class.count(t2));
  Symbol cur = t1;
  while (true) {
    if (class_parent[cur] == t2) {
      return true;
    } else if (cur == Object) {
      return false;
    }
    cur = class_parent[cur];
  }
}

bool ClassTable::is_defined(Symbol type) {
  return type == SELF_TYPE || name2class.count(type);
}

Symbol assign_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                                ClassTable *classtable) {
  if (name == self) {
    classtable->semant_error(class_->get_filename(), this)
            << "Cannot assign to 'self'.\n";
  }

  Symbol name_type = O->lookup(name); // T
  if (name_type == NULL) {
    classtable->semant_error(class_->get_filename(), this)
            << "Assignment to undeclared variable " << name << ".\n";
  }

  Symbol expr_type = expr->type_check(O, class_, classtable); // T'
  assert(classtable->is_defined(expr_type));

  if (classtable->is_defined(name_type) &&
      classtable->is_subtype(expr_type, name_type, class_) == false) { // T' <= T
    classtable->semant_error(class_->get_filename(), this)
            << "Type " << expr_type
            << " of assigned expression does not conform to declared type "
            << name_type << " of identifier " << name << ".\n";
  }
  set_type(expr_type);
  return type; // T'
}

Symbol static_dispatch_class::type_check(SymbolTable<Symbol, Entry> *O,
                                         Class_ class_, ClassTable *classtable) {
  if (type_name == SELF_TYPE) {
    classtable->semant_error(class_->get_filename(), this)
            << "Static dispatch to SELF_TYPE.\n";
    set_type(Object);
  } else if (classtable->name2class.count(type_name) == false) {
    classtable->semant_error(class_->get_filename(), this)
            << "Static dispatch to undefined class " << type_name << ".\n";
    set_type(Object);
  } else {
    Symbol T0 = expr->type_check(O, class_, classtable);
    if (classtable->is_defined(T0) && classtable->is_subtype(T0, type_name, class_)) {
      // T0 <= type_name
      if (classtable->class_methods[type_name].count(name) == false) {
        // Method not found
        classtable->semant_error(class_->get_filename(), this)
                << "Dispatch to undefined method " << name << ".\n";
        set_type(Object);
      } else {
        Feature fea = classtable->class_methods[type_name][name];
        if (actual->len() != fea->get_formals()->len()) {
          // wrong number of actual arguments
          classtable->semant_error(class_->get_filename(), this)
                  << "Method " << name << " called with wrong number of arguments.\n";
          set_type(Object);
        } else {
          for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
            Formal formal_i = fea->get_formals()->nth(i);
            Symbol Ti_ = actual->nth(i)->type_check(O, class_, classtable);
            assert(classtable->is_defined(Ti_));
            if (classtable->is_defined(formal_i->get_type()) &&
                classtable->is_subtype(Ti_, formal_i->get_type(), class_) == false) {
              // wrong type of actual argument
              classtable->semant_error(class_->get_filename(), this)
                      << "In call of method " << name << ", type " << Ti_ << " of parameter "
                      << formal_i->get_name() << " does not conform to declared type "
                      << formal_i->get_type() << ".\n";
            }
          }
          Symbol fea_return_type = fea->get_return_type();
          set_type(fea_return_type == SELF_TYPE ? T0 : (classtable->is_defined(fea_return_type) ? fea_return_type
                                                                                                : Object));
        }
      }
    } else {
      classtable->semant_error(class_->get_filename(), this)
              << "Expression type " << T0 << " does not conform to declared static dispatch type " << type_name
              << ".\n";
      set_type(Object);
    }
  }
  return type;
}

Symbol dispatch_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                                  ClassTable *classtable) {
  Symbol T0 = expr->type_check(O, class_, classtable);
  Symbol T0_ = T0;
  if (classtable->is_defined(T0) == false) {
    classtable->semant_error(class_->get_filename(), this)
            << "Dispatch on undefined class " << T0 << ".\n";
    set_type(Object);
  } else {
    // TO is OK
    if (T0 == SELF_TYPE) {
      T0_ = class_->get_name();
    }
    // T0_ is OK
    if (classtable->class_methods[T0_].count(name) == false) {
      // Method not found
      classtable->semant_error(class_->get_filename(), this)
              << "Dispatch to undefined method " << name << ".\n";
      set_type(Object);
    } else {
      Feature fea = classtable->class_methods[T0_][name];
      if (actual->len() != fea->get_formals()->len()) {
        // wrong number of actual arguments
        classtable->semant_error(class_->get_filename(), this)
                << "Method " << name << " called with wrong number of arguments.\n";
        set_type(Object);
      } else {
        for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
          Formal formal_i = fea->get_formals()->nth(i);
          Symbol Ti_ = actual->nth(i)->type_check(O, class_, classtable);
          assert(classtable->is_defined(Ti_));
          if (classtable->is_defined(formal_i->get_type()) &&
              classtable->is_subtype(Ti_, formal_i->get_type(), class_) == false) {
            // wrong type of actual argument
            classtable->semant_error(class_->get_filename(), this)
                    << "In call of method " << name << ", type " << Ti_ << " of parameter "
                    << formal_i->get_name() << " does not conform to declared type "
                    << formal_i->get_type() << ".\n";
          }
        }
        Symbol fea_return_type = fea->get_return_type();
        set_type(fea_return_type == SELF_TYPE ? T0 : (classtable->is_defined(fea_return_type) ? fea_return_type
                                                                                              : Object));
      }
    }
  }
  return type;
}

// Walk towards Object from each of a and b until the paths meet
Symbol ClassTable::lub(Symbol a, Symbol b, Class_ class_) {
  assert(a && b && class_);
  if (a == SELF_TYPE && b == SELF_TYPE) {
    return SELF_TYPE;
  }
  if (a == SELF_TYPE) {
    a = class_->get_name();
  }
  if (b == SELF_TYPE) {
    b = class_->get_name();
  }
  assert(name2class.count(a) && name2class.count(b));
  while (depth[a] > depth[b]) {
    a = class_parent[a];
  }
  while (depth[b] > depth[a]) {
    b = class_parent[b];
  }
  while (a != b) {
    a = class_parent[a];
    b = class_parent[b];
  }
  return a;
}

Symbol cond_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                              ClassTable *classtable) {
  Symbol pred_type = pred->type_check(O, class_, classtable);
  if (pred_type != Bool) {
    classtable->semant_error(class_->get_filename(), this)
            << "Predicate of 'if' does not have type Bool.\n";
  }
  Symbol then_type = then_exp->type_check(O, class_, classtable);
  Symbol else_type = else_exp->type_check(O, class_, classtable);
  assert(classtable->is_defined(then_type));
  assert(classtable->is_defined(else_type));
  Symbol T = classtable->lub(then_type, else_type, class_);
  set_type(T);
  return type;
}

Symbol loop_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                              ClassTable *classtable) {
  Symbol pred_type = pred->type_check(O, class_, classtable);
  if (pred_type != Bool) {
    classtable->semant_error(class_->get_filename(), this)
            << "Loop condition does not have type Bool.\n";
  }
  body->type_check(O, class_, classtable);
  set_type(Object);
  return type;
}

Symbol branch_class::get_type() {
  return type_decl;
}

Symbol branch_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                                ClassTable *classtable) {
  O->enterscope();
  if (type_decl == SELF_TYPE) {
    classtable->semant_error(class_->get_filename(), this)
            << "Identifier " << name << " declared with type SELF_TYPE in case branch.\n";
    // TODO
  }
  if (classtable->is_defined(type_decl) == false) {
    classtable->semant_error(class_->get_filename(), this)
            << "Class " << type_decl << " of case branch is undefined.\n";
  }
  if (name == self) {
    classtable->semant_error(class_->get_filename(), this)
            << "'self' bound in 'case'.\n";
  }
  O->addid(name, type_decl);
  Symbol T = expr->type_check(O, class_, classtable);
  O->exitscope();
  assert(classtable->is_defined(T));
  return T;
}

Symbol typcase_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                                 ClassTable *classtable) {
  expr->type_check(O, class_, classtable);
  Symbol T = cases->nth(0)->type_check(O, class_, classtable);
  assert(classtable->is_defined(T));

  for (int i = cases->next(cases->first()); cases->more(i); i = cases->next(i)) {
    Symbol Ti_ = cases->nth(i)->type_check(O, class_, classtable);
    assert(classtable->is_defined(Ti_));
    T = classtable->lub(T, Ti_, class_);

    for (int j = cases->first(); j < i; j = cases->next(j)) {
      if (cases->nth(i)->get_type() == cases->nth(j)->get_type()) {
        classtable->semant_error(class_->get_filename(), cases->nth(i))
                << "Duplicate branch " << cases->nth(i)->get_type() << " in case statement.\n";
        break;
      }
    }
  }
  set_type(T);
  assert(classtable->is_defined(T));
  return type;
}

Symbol block_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                               ClassTable *classtable) {
  Symbol T;
  for (int i = body->first(); i < body->len(); i = body->next(i)) {
    T = body->nth(i)->type_check(O, class_, classtable);
  }
  set_type(T);
  assert(classtable->is_defined(T));
  return type;
}

Symbol let_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                             ClassTable *classtable) {
  if (identifier == self) {
    classtable->semant_error(class_->get_filename(), this)
            << "'self' cannot be bound in a 'let' expression.\n";
  }

  Symbol T0 = type_decl;
  bool T0_ok = true;
  if (classtable->is_defined(T0) == false) {
    T0_ok = false;
    classtable->semant_error(class_->get_filename(), this)
            << "Class " << type_decl << " of let-bound identifier " << identifier << " is undefined.\n";
  }

  Symbol T1 = init->type_check(O, class_, classtable);
  assert(T1 == No_type || classtable->is_defined(T1));

  if (T1 != No_type && T0_ok && classtable->is_subtype(T1, T0, class_) == false) {
    classtable->semant_error(class_->get_filename(), this)
            << "Inferred type " << T1 << " of initialization of " << identifier
            << " does not conform to identifier's declared type " << type_decl << ".\n";
  }
  // enter a scope; required before any symbols can be added
  O->enterscope();
  O->addid(identifier, T0);

  Symbol T2 = body->type_check(O, class_, classtable);
  assert(classtable->is_defined(T2));

  O->exitscope();

  set_type(T2);
  return T2;
}

Symbol op_type_check(Expression e1, Expression e2, SymbolTable<Symbol, Entry> *O,
                     Class_ class_, ClassTable *classtable, tree_node *t, std::string op) {
  Symbol e1_type = e1->type_check(O, class_, classtable);
  Symbol e2_type = e2->type_check(O, class_, classtable);

  if (e1_type != Int || e2_type != Int) {
    classtable->semant_error(class_->get_filename(), t)
            << "non-Int arguments: " << e1_type << " " << op << " " << e2_type << "\n";
    return Object;
  }
  if (op == "<" || op == "<=") {
    return Bool;
  }
  return Int;
}

Symbol plus_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                              ClassTable *classtable) {
  Symbol T = op_type_check(e1, e2, O, class_, classtable, this, "+");
  set_type(T);
  return type;
}

Symbol sub_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                             ClassTable *classtable) {
  Symbol T = op_type_check(e1, e2, O, class_, classtable, this, "-");
  set_type(T);
  return type;
}

Symbol mul_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                             ClassTable *classtable) {
  Symbol T = op_type_check(e1, e2, O, class_, classtable, this, "*");
  set_type(T);
  return type;
}

Symbol divide_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                                ClassTable *classtable) {
  Symbol T = op_type_check(e1, e2, O, class_, classtable, this, "/");
  set_type(T);
  return type;
}

Symbol neg_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                             ClassTable *classtable) {
  set_type(Int);
  Symbol e1_type = e1->type_check(O, class_, classtable);
  if (e1_type != Int) {
    classtable->semant_error(class_->get_filename(), this)
            << "Argument of '~' has type " << e1_type << " instead of Int.\n";
    set_type(Object);
  }
  return type;
}

Symbol lt_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                            ClassTable *classtable) {
  Symbol T = op_type_check(e1, e2, O, class_, classtable, this, "<");
  set_type(T);
  return type;
}

bool need_same_type_compare(Symbol type) {
  return type == Int || type == Str || type == Bool;
}

Symbol eq_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                            ClassTable *classtable) {
  set_type(Bool);
  Symbol e1_type = e1->type_check(O, class_, classtable);
  Symbol e2_type = e2->type_check(O, class_, classtable);
  if ((need_same_type_compare(e1_type) || need_same_type_compare(e2_type)) && e1_type != e2_type) {
    classtable->semant_error(class_->get_filename(), this)
            << "Illegal comparison with a basic type.\n";
    set_type(Object);
  }
  return type;
}

Symbol leq_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                             ClassTable *classtable) {
  Symbol T = op_type_check(e1, e2, O, class_, classtable, this, "<=");
  set_type(T);
  return T;
}

Symbol comp_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                              ClassTable *classtable) {
  set_type(Bool);
  Symbol e1_type = e1->type_check(O, class_, classtable);
  if (e1_type != Bool) {
    classtable->semant_error(class_->get_filename(), this)
            << "Argument of 'not' has type " << e1_type << " instead of Bool.\n";
    set_type(Object);
  }
  return type;
}

Symbol
int_const_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                            ClassTable *classtable) {
  set_type(Int);
  return type;
}

Symbol
bool_const_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                             ClassTable *classtable) {
  set_type(Bool);
  return type;
}

Symbol
string_const_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                               ClassTable *classtable) {
  set_type(Str);
  return type;
}

Symbol new__class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                              ClassTable *classtable) {
  set_type(type_name);
  if (classtable->is_defined(type_name) == false) {
    classtable->semant_error(class_->get_filename(), this)
            << "'new' used with undefined class " << type_name << ".\n";
    set_type(Object);
  }
  return type;
}

Symbol isvoid_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                                ClassTable *classtable) {
  e1->type_check(O, class_, classtable);
  set_type(Bool);
  return type;
}

Symbol object_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                                ClassTable *classtable) {
  if (name == self) {
    set_type(SELF_TYPE);
  } else {
    Symbol lookup_type = O->lookup(name);
    if (lookup_type == NULL) {
      classtable->semant_error(class_->get_filename(), this)
              << "Undeclared identifier " << name << ".\n";
      set_type(Object);
    } else {
      set_type(lookup_type);
    }
  }
  return type;
}

Symbol formal_class::get_name() {
  return name;
}

Symbol formal_class::get_type() {
  return type_decl;
}

Symbol no_expr_class::type_check(SymbolTable<Symbol, Entry> *O, Class_ class_,
                                 ClassTable *classtable) {
  return No_type;
}