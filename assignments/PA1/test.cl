class A inherits IO{
    o():Object{
        out_string("A\n")
    };
};
class B inherits A{

    o():Object{
        out_string("B\n")
    };
};
class Main inherits IO {
    main(): Object {
        {
            let a:A <- (new B) in a.o();
        }
    };
};