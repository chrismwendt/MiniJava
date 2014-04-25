class Fields {
    public static void main(String[] a) {
        new tester().fun();
    }
}

class A1 {
    int a;
}

class A2 extends A1 {
    int a;
}

class A3 extends A2 {
    int a;
}

class A4 extends A3 {
    int a;
}

class tester {
    public int fun() {
        A4 a4;
        A3 a3;
        A2 a2;
        A1 a1;
        A4 b;

        a4 = new A4();
        a4.a = 4;

        a3 = a4;
        a3.a = 3;

        a2 = a4;
        a2.a = 2;

        a1 = a4;
        a1.a = 1;

        b = a4;
        b.a=10;

        System.out.println(a4.a);
        System.out.println(a3.a);
        System.out.println(a2.a);
        System.out.println(a1.a);
        System.out.println(b.a);

        return 1;
    }
}
