class Extended {
    public static void main(String[] a) {
        System.out.println(new A().go());
    }
}

class A {
    public int go() {
        Class1 c1;
        Class2 c2;
        Class3 c3;
        Class1 c1_2;
        Class1 c1_3;
        Class2 c2_3;
        c1 = new Class1();
        c2 = new Class2();
        c3 = new Class3();
        c1_2 = new Class2();
        c1_3 = new Class3();
        c2_3 = new Class3();
        if (c1.AMethod() == 0) { System.out.println(1337); } else { System.out.println(0); }
        if (c1.BMethod() == 1) { System.out.println(1337); } else { System.out.println(0); }
        if (c1.CMethod() == 2) { System.out.println(1337); } else { System.out.println(0); }
        if (c2.AMethod() == 0) { System.out.println(1337); } else { System.out.println(0); }
        if (c2.BMethod() == 3) { System.out.println(1337); } else { System.out.println(0); }
        if (c2.CMethod() == 2) { System.out.println(1337); } else { System.out.println(0); }
        if (c2.DMethod() == 4) { System.out.println(1337); } else { System.out.println(0); }
        if (c3.AMethod() == 7) { System.out.println(1337); } else { System.out.println(0); }
        if (c3.BMethod() == 3) { System.out.println(1337); } else { System.out.println(0); }
        if (c3.CMethod() == 2) { System.out.println(1337); } else { System.out.println(0); }
        if (c3.DMethod() == 5) { System.out.println(1337); } else { System.out.println(0); }
        if (c3.FMethod() == 6) { System.out.println(1337); } else { System.out.println(0); }
        if (c1_2.AMethod() == 0) { System.out.println(1337); } else { System.out.println(0); }
        if (c1_2.BMethod() == 3) { System.out.println(1337); } else { System.out.println(0); }
        if (c1_2.CMethod() == 2) { System.out.println(1337); } else { System.out.println(0); }
        if (c1_3.AMethod() == 7) { System.out.println(1337); } else { System.out.println(0); }
        if (c2_3.DMethod() == 5) { System.out.println(1337); } else { System.out.println(0); }
        return 1337;
    }
}

class Class1 {
    public int AMethod() {
        return 0;
    }

    public int BMethod() {
        return 1;
    }

    public int CMethod() {
        return 2;
    }
}

class Class2 extends Class1 {
    public int BMethod() {
        return 3;
    }

    public int DMethod() {
        return 4;
    }
}

class Class3 extends Class2 {
    public int AMethod() {
        return 7;
    }

    public int DMethod() {
        return 5;
    }

    public int FMethod() {
        return 6;
    }
}
