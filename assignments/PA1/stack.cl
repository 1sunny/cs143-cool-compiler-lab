(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

class StackCmd inherits A2I {
   cmd_: String;

   init(cmd: String): SELF_TYPE {
      {
         cmd_ <- cmd;
         self;
      }
   };

   to_string(): String {
      cmd_
   };

   execute(stk: Stack): SELF_TYPE {
      self
   };

   int(): Int {
      0
   };
};

class StackItem {
   item_: StackCmd;
   next_: StackItem;

   init(cmd: StackCmd): SELF_TYPE {
      {
         item_ <- cmd;
         self;
      }
   };

   item(): StackCmd {
      item_
   };

   next(): StackItem {
      next_
   };

   set_next(next: StackItem): SELF_TYPE {
      {
         next_ <- next;
         self;
      }
   };

   flatten(): String {
      if (isvoid next_) then
         ""
      else
         item_.to_string().concat("\n").concat(next_.flatten())
      fi
   };
};

class Stack {
   top_: StackItem;
   size_: Int;

   init(): SELF_TYPE {
      {
         top_ <- (new StackItem).init(new StackCmd);
         size_ <- 0;
         self;
      }
   };

   push(cmd: StackCmd): Object {
      {
         let new_top : StackItem <- (new StackItem).init(cmd) in {
            new_top.set_next(top_);
            top_ <- new_top;
         };
         size_ <- size_ + 1;
         self;
      }
   };

   pop(): StackCmd {
      {
         let old_top: StackCmd <- top_.item() in {
            top_ <- top_.next();
            old_top;
         };
      }
   };

   execute(): SELF_TYPE {
      {
         if (size_ = 0) then 
            self
         else
            pop().execute(self)
         fi;
         self;
      }
   };

   display(): String {
      top_.flatten()
   };
};

class IntCmd inherits StackCmd {
   num_: Int;

   init(cmd: String): SELF_TYPE {
      {
         cmd_ <- cmd;
         num_ <- a2i(cmd);
         self;
      }
   };

   int(): Int {
      num_
   };

   execute(stk: Stack): SELF_TYPE {
      {
         stk.push(self);
         self;
      }
   };
};

class AddCmd inherits StackCmd {
   execute(stk: Stack): SELF_TYPE {
      {
         let a: Int <- stk.pop().int(), b: Int <- stk.pop().int() 
         in stk.push((new IntCmd).init(i2a(a + b)));
         self;
      }
   };
};

class SwapCmd inherits StackCmd {
   execute(stk: Stack): SELF_TYPE {
      {
         let a: StackCmd <- stk.pop(), b: StackCmd <- stk.pop() in {
            stk.push(a);
            stk.push(b);
         };
         self;
      }
   };
};

class Main inherits IO {

   main() : Object {
      {
         let exit : Bool <- false, stk : Stack <- (new Stack).init() in {
            while (not exit) loop {
               out_string(">");
               let s : String <- in_string() in { 
                  if (s = "+") then
                     stk.push((new AddCmd).init(s))
                  else
                     if (s = "s") then
                        stk.push((new SwapCmd).init(s))
                     else
                        if (s = "e") then
                           stk.execute()
                        else
                           if (s = "d") then
                              out_string(stk.display())
                           else
                              if (s = "x") then
                                 exit <- true
                              else
                                 stk.push((new IntCmd).init(s))
                              fi
                           fi
                        fi
                     fi
                  fi;
               };
            } pool;
         };
      }
   };

};
