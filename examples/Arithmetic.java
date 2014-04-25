class Arithmetic {
    public static void main(String[] a) {
        System.out.println(new A().go());
    }
}

class A {
    public int go() {
        System.out.println(1336 + 1);
        System.out.println(1338 - 1);
        System.out.println(7 * 191);
        System.out.println(2674 / 2);
        System.out.println(3337 % 2000);
        if (2 < 4) { System.out.println(1337); } else { System.out.println(0); }
        if (4 > 2) { System.out.println(1337); } else { System.out.println(0); }
        if (2 <= 4) { System.out.println(1337); } else { System.out.println(0); }
        if (2 <= 2) { System.out.println(1337); } else { System.out.println(0); }
        if (7 >= 5) { System.out.println(1337); } else { System.out.println(0); }
        if (7 >= 7) { System.out.println(1337); } else { System.out.println(0); }
        if (7 == 7) { System.out.println(1337); } else { System.out.println(0); }
        if (7 != 8) { System.out.println(1337); } else { System.out.println(0); }
        if (true && true) { System.out.println(1337); } else { System.out.println(0); }
        if (!(true && false)) { System.out.println(1337); } else { System.out.println(0); }
        if (!(false && false)) { System.out.println(1337); } else { System.out.println(0); }
        if (!(false && true)) { System.out.println(1337); } else { System.out.println(0); }
        if (false || true) { System.out.println(1337); } else { System.out.println(0); }
        if (true || false) { System.out.println(1337); } else { System.out.println(0); }
        if (true || true) { System.out.println(1337); } else { System.out.println(0); }
        if (!(false || false)) { System.out.println(1337); } else { System.out.println(0); }
        if (7 >= 7) { System.out.println(1337); } else { System.out.println(0); }
        if (7 >= 7) { System.out.println(1337); } else { System.out.println(0); }
        if (true) { System.out.println(1337); } else { System.out.println(0); }
        if (!false) { System.out.println(1337); } else { System.out.println(0); }

        return 1337;
    }
}
