
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include <algorithm>
#include <utilities.h>
#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
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
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
  Method      = idtable.add_string("Method");
  Attr        = idtable.add_string("Attr");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names a0ording to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str, int& offset)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-WORD_SIZE,str);
  offset--;
}

static void emit_pop(char *reg, ostream& str, int& offset)
{
  emit_load(reg,1,SP,str);
  emit_addiu(SP,SP,WORD_SIZE,str);
  offset++;
}

static void emit_pop(int pop, ostream& str, int& offset)
{
  emit_addiu(SP,SP,pop,str);
  assert(pop % WORD_SIZE == 0);
  offset += pop / WORD_SIZE;
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }

void emit_fetch_bool(char *dest, char *reg, ostream &s) {
  emit_fetch_int(dest, reg, s);
}

// static void emit_test_collector(ostream &s)
// {
//   emit_push(A0, s);
//   emit_move(A0, SP, s); // stack end
//   emit_move(A1, ZERO, s); // allocate nothing
//   s << JAL << gc_collect_names[cgen_Memmgr] << endl;
//   emit_addiu(SP,SP,4,s);
//   emit_load(A0,0,SP,s);
// }

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size, (len+1(\0)+3)/4
      << WORD; emit_disptable_ref(Str, s);


 /***** Add dispatch information for class String ******/

      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
  s << endl;
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; emit_disptable_ref(Int, s);

 /***** Add dispatch information for class Int ******/

      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
      s << endl;
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD; emit_disptable_ref(Bool, s);

 /***** Add dispatch information for class Bool ******/

      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
      s << endl;
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;
  str << endl;
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
  str << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
  str << endl;
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : /*nds(NULL) ,*/ str(s), last_tag(-1)
{
  this->classes = classes;

   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();

  for (int i = this->classes->first(); this->classes->more(i); i = this->classes->next(i)) {
    Class_ class_ = this->classes->nth(i);
    Symbol name = class_->get_name();
    // add to class map
    name2class[name] = class_;
  }

  // build graph
  for (auto p: name2class) {
    Symbol name = p.first;
    Symbol parent = p.second->get_parent();
    class_parent[name] = parent;
    if (name == Object) {
      continue;
    }
    assert(name2class.count(parent));

    edge[parent].push_back(name);
    degree[name]++;
  }

  for (auto p: name2class) {
    Symbol name = p.first;
    std::reverse(edge[name].begin(), edge[name].end());
  }

  handle_inheritance(Object, No_class);

  stringclasstag = tag_in[Str];
  intclasstag    = tag_in[Int];
  boolclasstag   = tag_in[Bool];

  code();
}

void CgenClassTable::install_basic_classes() {
  Symbol filename = stringtable.add_string("<basic class>");
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
               single_Features(method(type_name, nil_Formals(), Str, no_expr()))
             ),
             single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))
           ),
           filename
    );

  classes = cons(Object_class, classes);
  //
  // The IO class inherits from Object. Its methods are
  //        out_string(Str) : SELF_TYPE       writes a string to the output
  //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
  //        in_string() : Str                 reads a string from the input
  //        in_int() : Int                      "   an int     "  "     "
  //
  Class_ IO_class =
    class_(IO, Object,
      append_Features(
        append_Features(
          append_Features(
            single_Features(method(out_string,
                                   single_Formals(formal(arg, Str)),
                                   SELF_TYPE, no_expr()
                            )
            ),
            single_Features(method(out_int,
                                   single_Formals(formal(arg, Int)),
                                   SELF_TYPE, no_expr()
                            )
            )
          ),
          single_Features(method(in_string, nil_Formals(), Str, no_expr()))
        ),
        single_Features(method(in_int, nil_Formals(), Int, no_expr()))
      ),
      filename
    );

  classes = cons(IO_class, classes);
  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer.
  //
  Class_ Int_class =
    class_(Int,
           Object,
           single_Features(attr(val, prim_slot, no_expr())),
           filename
    );
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
                   single_Features(attr(str_field, prim_slot, no_expr()))
                 ),
                 single_Features(method(length, nil_Formals(), Int, no_expr()))
               ),
               single_Features(method(concat,
                                      single_Formals(formal(arg, Str)),
                                      Str,
                                      no_expr()
                               )
               )
             ),
             single_Features(method(substr,
                                    append_Formals(single_Formals(formal(arg, Int)),
                                                           single_Formals(formal(arg2, Int))
                                            ),
                                    Str,
                                    no_expr()))
                            ),
             filename
    );
  classes = cons(Str_class, classes);
}

bool contain_feature(std::vector<SF> &v, Symbol name) {
  for (const auto& sf : v) {
    if (sf.name == name) {
      return true;
    }
  }
  return false;
}

void CgenClassTable::handle_inheritance(Symbol u, Symbol parent) {
  stringtable.add_string(u->get_string());

  Class_ class_ = name2class[u];
  Features features = class_->get_features();
  std::vector<SF> temp_method;
  std::vector<SF> temp_attr;

  /**
   * Problem Note:
   * testfile: copy-self-dispatch.cl
   * 子类继承的函数顺序要与父类一样,否则子类指向父类时无法进行动态调用
   */
  if (u != Object) {
    class_methods[u] = class_methods[parent];
    class_attrs[u] = class_attrs[parent];
  }

  for (int i = features->first(); features->more(i); i = features->next(i)) {
    Feature cur_fea = features->nth(i);
    Symbol cur_fea_type = cur_fea->get_feature_type();
    Symbol cur_fea_name = cur_fea->get_name();

    bool is_over_write = false;
    if (cur_fea_type == Method) {
      for (int j = 0; j < class_methods[u].size(); ++j) {
        if (cur_fea_name == class_methods[u][j].name) {
          is_over_write = true;
          class_methods[u][j] = {u, cur_fea_name, cur_fea};
        }
      }
      if (is_over_write == false) {
        class_methods[u].push_back({u, cur_fea_name, cur_fea});
      }
    } else if (cur_fea_type == Attr) {
      for (int j = 0; j < class_attrs[u].size(); ++j) {
        if (cur_fea_name == class_attrs[u][j].name) {
          is_over_write = true;
          class_attrs[u][j] = {u, cur_fea_name, cur_fea};
        }
      }
      if (is_over_write == false) {
        class_attrs[u].push_back({u, cur_fea_name, cur_fea});
      }
    }
  }

  tag_in[u] = ++last_tag;
  tag2name.push_back(u);

  // run a deep first search
  for (Symbol v: edge[u]) {
    handle_inheritance(v, u);
  }

  tag_out[u] = last_tag;
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

  if (cgen_debug) cout << "coding class_nameTab" << endl;
  code_class_nameTab();

  if (cgen_debug) cout << "coding class_objTab" << endl;
  code_class_objTab();

  if (cgen_debug) cout << "coding prototype objects" << endl;
  code_prototype_objects();

  if (cgen_debug) cout << "coding dispatch tables" << endl;
  code_dispatch_tables();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

  if (cgen_debug) cout << "coding object initializer" << endl;
  code_object_initializer();

  if (cgen_debug) cout << "coding class methods" << endl;
  code_class_methods();
}

void emit_default(Symbol type, ostream& s) {
  if (type == Bool) {
    falsebool.code_ref(s);
  } else if (type == Int) {
    inttable.add_string("0")->code_ref(s);
  } else if (type == Str) {
    stringtable.add_string("")->code_ref(s);
  } else {
    // 0 address
    s << 0;
  }
}

void CgenClassTable::code_prototype_objects() {
  str << "# Class Prototype" << endl;

  for (auto name : tag2name) {
    str << WORD << "-1" << endl;

    emit_protobj_ref(name, str); str << LABEL
    << WORD << tag_in[name] << endl
    << WORD << (DEFAULT_OBJFIELDS + class_attrs[name].size()) << endl
    << WORD; emit_disptable_ref(name, str); str << endl;
    // attr
    for (auto sf : class_attrs[name]) {
      str << WORD;
      const Symbol &type = sf.feature->get_type_decl();
      emit_default(type, str);
      str << endl;
    }
    str << endl;
  }
  str << endl;
}

void CgenClassTable::code_class_nameTab() {
  str << "# Class Name String Table" << endl;
  str << CLASSNAMETAB << LABEL;

  for (auto name : tag2name) {
    str << WORD; stringtable.lookup_string(name->get_string())->code_ref(str);
    str << endl;
  }
  str << endl;
}

void CgenClassTable::code_dispatch_tables() {
  str << "# Class Dispatch Table" << endl;
  for (auto p : name2class) {
    emit_disptable_ref(p.first, str); str << LABEL;
    for (auto sf : class_methods[p.first]) {
      str << WORD; emit_method_ref(sf.class_name, sf.name, str);
      str << endl;
    }
  }
  str << endl;
}

void CgenClassTable::func_start(int& offset) {
  // $fp are considered callee-saved by runtime system
  emit_push(FP, str, offset); // save caller's $fp
  emit_move(FP, SP, str); // set $fp
  emit_push(S0, str, offset); // save caller's $s0
  emit_push(RA, str, offset); // save caller's $ra
  emit_move(S0, A0, str); // save a0 to s0
}

void CgenClassTable::func_end(int nargs) {
  emit_load(RA, 1, SP, str); // restore $ra
  emit_load(S0, 2, SP, str); // restore caller's $s0
  emit_load(FP, 3, SP, str); // restore caller's $fp
  // 不能在这个函数改变offset,因为内置函数结束时无法改变定义的offset
  // emit_pop(nargs * WORD_SIZE + WORD_SIZE + WORD_SIZE + WORD_SIZE, str, offset);
  // pop stack, nargs + 1($ra) + 1($s0) + 1($fp)
  emit_addiu(SP, SP, nargs * WORD_SIZE + WORD_SIZE + WORD_SIZE + WORD_SIZE, str);
  emit_return(str); // jr str
  str << endl;
}

// For the initializations methods,
// Coolaid and the runtime system consider $a0 to be callee-saved
// (in addition to the callee-saved registers for normal methods)
void CgenClassTable::code_object_initializer() {
  str << "# Class Initializer" << endl;

  SymbolTable<Symbol, Loc> *E = new SymbolTable<Symbol, Loc>();

  for (auto p : name2class) {
    Symbol name = p.first;
    emit_init_ref(name, str); str << LABEL;
    if (name == Object) {
      int offset = 1;
      func_start(offset);
      func_end(0);
    } else {
      E->enterscope();
      // add attrs to environment first, although they haven't init
      for (int i = 0; i < class_attrs[name].size(); i++) {
        SF attr = class_attrs[name][i];
        E->addid(attr.name, new Loc(i + DEFAULT_OBJFIELDS, S0));
      }

      int offset = 1;
      func_start(offset);
      // initialize parent
      str << JAL; emit_init_ref(class_parent[name], str); str << endl;

      // initialize attrs
      for (int i = 0; i < class_attrs[name].size(); i++) {
        SF attr = class_attrs[name][i];
        if (attr.class_name == name) {
          Expression attr_init = attr.feature->get_expr();
          // no_expr ?
          // if init expr don't exist, use default value (protobj)
          if (attr_init->get_type() != NULL && attr_init->get_type() != No_type) {
            attr_init->code(str, E, p.second, this, offset);
            emit_store(A0, i + DEFAULT_OBJFIELDS, S0, str); // attr = res
          }
        }
      }

      emit_move(A0, S0, str); // For the initializations methods, the runtime system consider $a0 to be callee-saved
      func_end(0);

      E->exitscope();
    }
  }
  str << endl;
}

bool is_basic_class(Symbol name) {
  return name == Object || name == IO || name == Int || name == Str || name == Bool || name == SELF_TYPE;
}

void CgenClassTable::code_class_methods() {
  for (auto p : name2class) {
    Symbol name = p.first;
    Class_ class_ = p.second;
    if (is_basic_class(name)) {
      continue;
    }
    // add attr to environment
    SymbolTable<Symbol, Loc> *E = new SymbolTable<Symbol, Loc>();
    E->enterscope();
    for (int i = 0; i < class_attrs[name].size(); i++) {
      E->addid(class_attrs[name][i].name, new Loc(i + DEFAULT_OBJFIELDS, S0));
    }

    for (auto method : class_methods[name]) {
      if (name == method.class_name) {
        emit_method_ref(name, method.name, str); str << LABEL;

        E->enterscope();

        Formals formals = method.feature->get_formals();
        // add formals to environment
        int formal_len = formals->len();
        for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
          Formal formal_i = formals->nth(i);
          // first argument pushed first
          E->addid(formal_i->get_name(), new Loc(formal_len - i + 1, FP));
        }

        int offset = 1; // 因为在设置 FP之前有一次 push
        func_start(offset);
        method.feature->get_expr()->code(str, E, class_, this, offset);
        func_end(formal_len);

        E->exitscope();
      }
    }
    E->exitscope();
  }
}

void CgenClassTable::code_class_objTab() {
  str << "# Class Obj Table" << endl;
  str << CLASSOBJTAB << LABEL;

  for (auto name : tag2name) {
    str << WORD; emit_protobj_ref(name, str);
    str << endl;
    str << WORD; emit_init_ref(name, str);
    str << endl;
  }
  str << endl;
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************
void print_asm_comment(ostream& s, const char* str) {
  s << "# " << str << endl;
}

void assign_class::code(ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  print_asm_comment(s, "assign_class");

  expr->code(s, E, class_, ct, offset);
  // get variable location
  Loc *pLoc = E->lookup(name);
  assert(pLoc);
  emit_store(A0, pLoc->offset, pLoc->reg, s);

  print_asm_comment(s, "end assign_class ---");
}

void code_dispatch(Expression expr, Symbol type, Symbol name, Expressions actual,
                   int line_number,
                   ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  print_asm_comment(s, name->get_string());

  int actual_len = actual->len();
  // push args
  for (int i = 0; i < actual_len; i++) {
    Expression ei = actual->nth(i);
    ei->code(s, E, class_, ct, offset);
    emit_push(A0, s, offset);
  }
  // will set a0
  expr->code(s, E, class_, ct, offset);

  // runtime error: dispatch on void
  emit_bne(A0, ZERO, label_index, s);
  // load filename -> a0
  emit_load_string(A0, stringtable.lookup_string(class_->get_filename()->get_string()), s);
  emit_load_imm(T1, line_number, s);
  emit_jal(DISPATCH_ABORT, s);
  // --------------------------
  // a0 is a valid object
  emit_label_def(label_index, s);
  label_index++;
  /**
   * Problem Note
   * copy-self-init.cl
   * static dispatch 应该 load指定 type的 dispatch table
   */
  // get expr's type
  Symbol T0;
  if (type == nullptr) { // dispatch
    // object address in a0
    emit_load(T1, DISPTAB_OFFSET, A0, s); // load dispatch tab address

    T0 = expr->get_type();
    if (T0 == SELF_TYPE) {
      // SELF_TYPE只会在本class_中出现(表达式返回类型),其它class的SELF_TYPE已经被类型检查推断出具体类型了
      T0 = class_->get_name();
    }
  } else { // static dispatch
    emit_partial_load_address(T1, s); emit_disptable_ref(type, s); s << endl;

    T0 = type;
  }

  // find method offset in dispatch table
  int i;
  for (i = 0; i < ct->class_methods[T0].size(); i++) {
    SF sf = ct->class_methods[T0][i];
    if (sf.name == name) {
      break;
    }
  }
  assert(i < ct->class_methods[T0].size());
  // load method address from dispatch tab
  emit_load(T1, i, T1, s);
  emit_jalr(T1, s);
  // update offset
  offset += actual_len;
}

void static_dispatch_class::code(ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  print_asm_comment(s, "static_dispatch_class");

  code_dispatch(expr, type_name, name, actual, get_line_number(), s, E, class_, ct, offset);

  print_asm_comment(s, "end static_dispatch_class ---");
}

void dispatch_class::code(ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  print_asm_comment(s, "dispatch_class");

  code_dispatch(expr, NULL, name, actual, get_line_number(), s, E, class_, ct, offset);

  print_asm_comment(s, "end dispatch_class ---");
}

void cond_class::code(ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  print_asm_comment(s, "cond_class");

  pred->code(s, E, class_, ct, offset);
  emit_fetch_bool(T1, A0, s);

  emit_beqz(T1, label_index, s);
  int else_label_index = label_index;
  label_index++;

  int fi_label_index = label_index;
  label_index++;

  // then branch
  then_exp->code(s, E, class_, ct, offset);
  emit_branch(fi_label_index, s);

  // else branch
  emit_label_def(else_label_index, s);
  else_exp->code(s, E, class_, ct, offset);

  // out if(fi)
  emit_label_def(fi_label_index, s);

  print_asm_comment(s, "end cond_class ---");
}

void loop_class::code(ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  print_asm_comment(s, "loop_class");

  // loop start label index
  int loop_index = label_index;
  label_index++;
  // emit ref
  emit_label_def(loop_index, s);

  pred->code(s, E, class_, ct, offset);

  emit_fetch_bool(T1, A0, s);
  // loop end label index
  int pool_index = label_index;
  label_index++;
  emit_beq(T1, ZERO, pool_index, s);

  body->code(s, E, class_, ct, offset);
  // back to start
  emit_branch(loop_index, s);

  emit_label_def(pool_index, s);
  emit_move(A0, ZERO, s); // while has value void

  print_asm_comment(s, "end loop_class ---");
}

void typcase_class::code(ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  print_asm_comment(s, "typcase_class");

  expr->code(s, E, class_, ct, offset);

  // record the out case label index
  int esac_index = label_index;
  label_index++;
  // case start label index
  int case_index = label_index;
  label_index++;

  emit_bne(A0, ZERO, case_index, s);
  // a case is attempted on a void object
  emit_load_string(A0, stringtable.lookup_string(class_->get_filename()->get_string()), s);
  emit_load_imm(T1, get_line_number(), s);
  emit_jal(CASE_ON_VOID, s);

  emit_label_def(case_index, s);
  // load class tag
  emit_load(T2, CLASS_TAG_OFFSET, A0, s);

  int len = cases->len();
  std::vector<Case> v_cases(len);
  for (int i = 0; i < len; i++) {
    v_cases[i] = cases->nth(i);
  }

  std::sort(v_cases.begin(), v_cases.end(), [&](Case cs1, Case cs2) {
    return ct->tag_in[cs1->get_type()] > ct->tag_in[cs2->get_type()];
  });

  // case branch
  for (int i = 0; i < len; i++) {
    Case cs = v_cases[i];
    Symbol type_decl = cs->get_type();
    int tag_in = ct->tag_in[type_decl];
    int tag_out = ct->tag_out[type_decl];

    int next_branch_index = label_index;
    label_index++;

    emit_blti(T2, tag_in, next_branch_index, s);
    emit_bgti(T2, tag_out, next_branch_index, s);

    E->enterscope();
    E->addid(cs->get_name(), new Loc(offset, FP));

    /**
     * Problem Note
     * shadow-attr-case.cl
     * 不应该把 $a0设为默认值,直接用 expr的值
     */
    // emit_partial_load_address(A0, s);
    // emit_default(type_decl, s);
    // s << endl;
    // save branch declared variable
    emit_push(A0, s, offset);
    // code match branch's expr
    cs->get_expr()->code(s, E, class_, ct, offset);

    emit_pop(WORD_SIZE, s, offset);
    emit_branch(esac_index, s); // b	label_esac_index
    E->exitscope();

    // next branch's label
    emit_label_def(next_branch_index, s);
    if (i == len - 1) {
      // case has no match
      emit_jal(CASE_NO_MATCH, s);
    }
  }

  emit_label_def(esac_index, s);

  print_asm_comment(s, "end typcase_class ---");
}

void block_class::code(ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  print_asm_comment(s, "block_class");

  for (int i = body->first(); i < body->len(); i = body->next(i)) {
    body->nth(i)->code(s, E, class_, ct, offset);
  }

  print_asm_comment(s, "end block_class ---");
}

void let_class::code(ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  print_asm_comment(s, "let_class");

  if (init->type == nullptr || init->type == No_type) {
    emit_partial_load_address(A0, s);
    emit_default(type_decl, s);
    s << endl;
  } else {
    init->code(s, E, class_, ct, offset);
  }

  E->enterscope();
  E->addid(identifier, new Loc(offset, FP));

  emit_push(A0, s, offset);

  body->code(s, E, class_, ct, offset);

  emit_pop(WORD_SIZE, s, offset);
  E->exitscope();

  print_asm_comment(s, "end let_class ---");
}

void arith(Expression e1, Expression e2, std::string op, ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  e1->code(s, E, class_, ct, offset);
  emit_push(A0, s, offset);

  e2->code(s, E, class_, ct, offset);
  emit_jal(OBJECT_COPY, s);
  emit_pop(T1, s, offset);
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, A0, s);

  if (op == "+")
    emit_add(T1, T1, T2, s);
  else if (op == "-")
    emit_sub(T1, T1, T2, s);
  else if (op == "*")
    emit_mul(T1, T1, T2, s);
  else if (op == "/")
    emit_div(T1, T1, T2, s);
  else
    assert(0);

  emit_store_int(T1, A0, s);
}


void plus_class::code(ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  print_asm_comment(s, "plus_class");

  arith(e1, e2, "+", s, E, class_, ct, offset);

  print_asm_comment(s, "end plus_class ---");
}

void sub_class::code(ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  print_asm_comment(s, "sub_class");

  arith(e1, e2, "-", s, E, class_, ct, offset);

  print_asm_comment(s, "end sub_class ---");
}

void mul_class::code(ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  print_asm_comment(s, "mul_class");

  arith(e1, e2, "*", s, E, class_, ct, offset);

  print_asm_comment(s, "end mul_class ---");
}

void divide_class::code(ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  print_asm_comment(s, "divide_class");

  arith(e1, e2, "/", s, E, class_, ct, offset);

  print_asm_comment(s, "end divide_class ---");
}

// negative
void neg_class::code(ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  print_asm_comment(s, "neg_class");

  e1->code(s, E, class_, ct, offset);
  emit_jal(OBJECT_COPY, s);
  emit_fetch_int(T1, A0, s);
  emit_neg(T1, T1, s);
  emit_store_int(T1, A0, s);

  print_asm_comment(s, "end neg_class ---");
}

void compare(Expression e1, Expression e2, std::string op, ostream& s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  e1->code(s, E, class_, ct, offset);
  emit_push(A0, s, offset);

  e2->code(s, E, class_, ct, offset);

  emit_pop(T1, s, offset);
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, A0, s);

  emit_load_bool(A0, truebool, s); // la $a0 bool_const1

  if (op == "<")
    emit_blt(T1, T2, label_index, s); // blt $t1 $t2 label x
  else if (op == "<=")
    emit_bleq(T1, T2, label_index, s); // ble $t1 $t2 label x
  else
    assert(0);

  emit_load_bool(A0, falsebool, s); // la $a0 bool_const0

  emit_label_def(label_index, s); // label x:
  label_index++;
}

void lt_class::code(ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  print_asm_comment(s, "lt_class");

  compare(e1, e2, "<", s, E, class_, ct, offset);

  print_asm_comment(s, "end lt_class ---");
}

void eq_class::code(ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  print_asm_comment(s, "eq_class");

  e1->code(s, E, class_, ct, offset);
  emit_push(A0, s, offset);

  e2->code(s, E, class_, ct, offset);
  emit_move(T2, A0, s);
  emit_pop(T1, s, offset);

  emit_load_bool(A0, truebool, s);
  emit_beq(T1, T2, label_index, s);
  emit_load_bool(A1, falsebool, s);
  emit_jal(EQUALITY_TEST, s);

  emit_label_def(label_index, s); // label x:
  label_index++;

  print_asm_comment(s, "end eq_class ---");
}

void leq_class::code(ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  print_asm_comment(s, "leq_class");

  compare(e1, e2, "<=", s, E, class_, ct, offset);

  print_asm_comment(s, "end leq_class ---");
}

// Not
void comp_class::code(ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  print_asm_comment(s, "not_class");

  e1->code(s, E, class_, ct, offset);

  emit_fetch_int(T1, A0, s);

  emit_load_bool(A0, truebool, s); // la $a0 bool_const1

  emit_beqz(T1, label_index, s);

  emit_load_bool(A0, falsebool, s); // la $a0 bool_const1

  emit_label_def(label_index, s); // label x:
  label_index++;

  print_asm_comment(s, "end not_class ---");
}

void int_const_class::code(ostream& s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset)
{
  print_asm_comment(s, "int_const_class");

  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(A0,inttable.lookup_string(token->get_string()),s);

  print_asm_comment(s, "end int_const_class ---");
}

void string_const_class::code(ostream& s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset)
{
  print_asm_comment(s, "string_const_class");

  emit_load_string(A0,stringtable.lookup_string(token->get_string()),s);

  print_asm_comment(s, "end string_const_class ---");
}

void bool_const_class::code(ostream& s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset)
{
  print_asm_comment(s, "bool_const_class");

  emit_load_bool(A0, val, s);

  print_asm_comment(s, "end bool_const_class ---");
}

void new__class::code(ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  print_asm_comment(s, "new__class");

  Symbol type = new__class::type_name;
  if (type == SELF_TYPE) {
    /**
     * Problem Note
     * new-self-dispatch.cl
     * 父类函数体中的 new SELF_TYPE需要根据 class_objTab 和 a0中的 class tag确定
     */
    emit_load_address(T1, CLASSOBJTAB, s); // la $t1 class_objTab
    emit_load(T2, CLASS_TAG_OFFSET, S0, s); // lw $t2 0($s0)
    emit_sll(T2, T2, 3, s); // $t2 = class_tag * 8 logical shift left
    emit_addu(T1, T1, T2, s); // $t1 = $t1 + $t2
    emit_move(S1, T1, s); // save T1 in S1, object.copy modify some register
    emit_load(A0, 0, T1, s); // lw $a0 0($t1)
    emit_jal(OBJECT_COPY, s);

    // invoke init method, protObj + 4
    emit_load(T1, 1, S1, s);
    emit_jalr(T1, s);
  } else {
    emit_partial_load_address(A0, s); emit_protobj_ref(type, s); s << endl;
    emit_jal(OBJECT_COPY, s);
    s << JAL; emit_init_ref(type, s); s << endl;
  }
  // a0 is callee-save in init method

  print_asm_comment(s, "end new__class ---");
}

void isvoid_class::code(ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  print_asm_comment(s, "isvoid_class");

  e1->code(s, E, class_, ct, offset);
  emit_move(T1, A0, s);

  emit_load_bool(A0, truebool, s);
  emit_beqz(T1, label_index, s);
  emit_load_bool(A0, falsebool, s);

  emit_label_def(label_index, s); // label x:
  label_index++;

  print_asm_comment(s, "end isvoid_class ---");
}

void no_expr_class::code(ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  throw std::runtime_error("Not impl");
}

void object_class::code(ostream &s, SymbolTable<Symbol, Loc> *E, Class_ class_, CgenClassTable *ct, int& offset) {
  print_asm_comment(s, "object_class");

  if (name == self) {
    // self address in s0
    emit_move(A0, S0, s);
    return;
  }
  Loc *pLoc = E->lookup(name);
  assert(pLoc);
  emit_load(A0, pLoc->offset, pLoc->reg, s);

  print_asm_comment(s, "end object_class ---");
}

Features class__class::get_features() {
  return features;
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

Expression method_class::get_expr() {
  return expr;
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

Expression attr_class::get_expr() {
  return init;
}

Symbol branch_class::get_type() {
  return type_decl;
}

Symbol branch_class::get_name() {
  return name;
}

Expression branch_class::get_expr() {
  return expr;
}

Symbol formal_class::get_name() {
  return name;
}

Symbol formal_class::get_type() {
  return type_decl;
}
